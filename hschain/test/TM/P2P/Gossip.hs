{-# LANGUAGE DataKinds #-}
-- | Tests for gossip.
--
-- These tests prepare environment, call functions from HSChain.P2P
-- and then check results. Because of these functions don't contain
-- any callback we can rely upon only tracing ('TepgvCurrent' for example).
--

{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE ViewPatterns      #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE ScopedTypeVariables      #-}

module TM.P2P.Gossip (tests) where

import Codec.Serialise (deserialise)
import Control.Concurrent.Chan
import Control.Concurrent.STM.TBQueue
import Control.Concurrent.STM.TQueue
import Control.Concurrent.STM.TChan
import Control.Concurrent.STM.TVar
import Control.Concurrent.MVar
import Control.Monad
import Control.Monad.Catch
import Control.Monad.Trans.State.Strict
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Class
import Control.Monad.Fail
import Control.Monad.Fix
import Control.Monad.IO.Class
import Control.Monad.STM
import Data.Coerce
import Lens.Micro
import Prelude                      as P
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict    as Map
import qualified Data.Set           as Set

import HSChain.Blockchain.Internal.Engine.Types
import HSChain.Blockchain.Internal.Types
import HSChain.Control
import HSChain.Crypto
import HSChain.Crypto.Containers
import HSChain.Debug.Trace
import HSChain.Logger
import HSChain.Mock.Types
import HSChain.P2P
import HSChain.P2P.Internal
import HSChain.P2P.PeerState.Types
import HSChain.P2P.Types
import HSChain.Run
import HSChain.Store
import HSChain.Store.Internal.BlockDB (storeCommit)
import HSChain.Store.STM
import HSChain.Types.Blockchain
import HSChain.Types.Validators
import HSChain.Utils
import qualified HSChain.Mock.KeyVal as Mock

import qualified HSChain.P2P.PeerState.Types as P2P
import qualified HSChain.P2P.PeerState.Handle as P2P (handler)

import Test.Tasty
import Test.Tasty.HUnit

import TM.Util.Network
import TM.Util.MockChain

tests :: TestTree
tests = testGroup "eigen-gossip"
    [ testCase "unknown"              testRawGossipUnknown
    , testCase "lagging"              testRawGossipLagging
    , testCase "ahead"                testRawGossipAhead
    , testGroup "current"
        [ testCase "proposal"                      testRawGossipCurrentSentProposal
        , testCase "nothing"                       testRawGossipCurrent1
        , testCase "proposal"                      testRawGossipCurrent2
        , testCase "prevotes"                      testRawGossipCurrent3
        , testCase "precommits"                    testRawGossipCurrent4
        , testCase "proposal,prevotes"             testRawGossipCurrent5
        , testCase "proposal,precommits"           testRawGossipCurrent6
        , testCase "precommits,prevotes"           testRawGossipCurrent7
        , testCase "proposal,precommits,prevotes"  testRawGossipCurrent8
        ]
    , testGroup "gossip-newstyle"
      [ testCase "silent peer"     testGossipUnknown
      , testCase "peer ahead"      testGossipAhead
      , testCase "peer lagging"    testGossipLagging
      , testCase "peer is current" testGossipCurrent
      ]
    ]

----------------------------------------------------------------
-- Gossip tests
----------------------------------------------------------------

-- Peer is silent and its state remains unknown
testGossipUnknown :: IO ()
testGossipUnknown = withGossip 3 $ do
  -- Must start from unknown state
  get >>= \case
    WrapState (Unknown _) -> return ()
    _                     -> () <$ throwM (TestError "Should start from Unknown state")
  -- Start consensus. We changed state. Peer is silent so it's still unknown
  (_,cmds) <- step =<< startConsensus
  get >>= \case
    WrapState (Unknown _) -> return ()
    _                     -> () <$ throwM (TestError "Should start from Unknown state")
  case cmds of
    [Push2Gossip (GossipAnn (AnnStep (FullStep (Height 4) (Round 0) (StepNewHeight 0))))] -> return ()
    _ -> error "Unexpected message"


-- Peer is ahead of us
testGossipAhead :: IO ()
testGossipAhead = withGossip 3 $ do
  _ <- step =<< startConsensus
  (s',[]) <- step $ EGossip $ GossipAnn $ AnnStep $ FullStep (Height 100) (Round 0) (StepNewHeight 0)
  case s' of
    WrapState (Ahead _) -> return ()
    _                   -> () <$ error "Should start from Unknown state"
  -- We don't have anything to send
  (_,[]) <- step EVotesTimeout
  (_,[]) <- step EBlocksTimeout
  return ()

-- Peer is lagging
testGossipLagging :: IO ()
testGossipLagging = withGossip 3 $ do
  _ <- step =<< startConsensus
  -- Peer announce its state
  do (st,[]) <- step $ EGossip $ GossipAnn $ AnnStep $ FullStep (Height 3) (Round 0) (StepNewHeight 0)
     case st of
       WrapState (Lagging _) -> return ()
       _                     -> () <$ error "Peer should be lagging"
  -- We should receive 4 votes
  (_,[Push2Gossip (GossipPreCommit _)]) <- step EVotesTimeout
  (_,[Push2Gossip (GossipPreCommit _)]) <- step EVotesTimeout
  (_,[Push2Gossip (GossipPreCommit _)]) <- step EVotesTimeout
  (_,[Push2Gossip (GossipPreCommit _)]) <- step EVotesTimeout
  (s,[])                                <- step EVotesTimeout
  liftIO $ print s
  -- At this point peer has enough votes to commit block but FSM is
  -- not smart enough to figure it on its own.
  --
  -- NOTE: subject to changes in the future
  (_,[]) <- step EBlocksTimeout
  -- When peer has enough precommits it announces that it has "proposal"
  (_,[]) <- step $ EGossip $ GossipAnn $ AnnHasProposal (Height 3) (Round 0)
  (_,[Push2Gossip (GossipBlock _)]) <- step EBlocksTimeout
  -- Peer commits and advances to the same height as we
  do (s',[]) <- step $ EGossip $ GossipAnn $ AnnStep $ FullStep (Height 4) (Round 0) (StepNewHeight 0)
     case s' of
       WrapState (Current _) -> return ()
       _                     -> () <$ error "Peer should be lagging"


-- Peer is current
testGossipCurrent :: IO ()
testGossipCurrent = withGossip 3 $ do
  _ <- step =<< startConsensus
  do (st,[]) <- step $ EGossip $ GossipAnn $ AnnStep $ FullStep (Height 4) (Round 0) (StepNewHeight 0)
     case st of
       WrapState (Current _) -> return ()
       _                     -> () <$ error "Peer should be current"
  -- FIXME: we don't test anything of substance here

  -- where
  --   block = mockchain !! 4
  --   bid   = blockHash block
  --   idx   = indexByValidator valSet (publicKey k1)



----------------------------------------------------------------
-- Helpers
----------------------------------------------------------------

type GossipM alg a = DBT 'RW alg a (NoLogsT IO)
type TestM   alg a = StateT  (P2P.SomeState alg a)
                   ( ReaderT ( P2P.Config (GossipM alg a) alg a
                             , TVar (Maybe (Height, TMState alg a)))
                   ( GossipM alg a
                   ))

-- Start gossip FSM all alone
withGossip :: Int -> TestM TestAlg Mock.BData x -> IO x
withGossip n action = do
  withDatabase "" genesis $ \conn -> runNoLogsT $ runDBT conn $ do
    consensusState <- liftIO $ newTVarIO Nothing
    gossipCnts     <- newGossipCounters
    props          <- newSTMPropStorage
    cursor         <- getMempoolCursor nullMempool
    let config = P2P.Config
                   (makeReadOnlyPS props)
                   cursor
                   (readTVar consensusState)
                   gossipCnts
    seedDatabase n
    flip runReaderT (config, consensusState)
      $ flip evalStateT (P2P.wrap P2P.UnknownState)
      $ action



mockchain :: [Block TestAlg Mock.BData]
mockchain = scanl mintBlock genesis [Mock.BData [("K",i)] | i <- [100..]]

-- Seed database with given number of blocks
seedDatabase :: Int -> GossipM TestAlg Mock.BData ()
seedDatabase n = do
  mustQueryRW $ forM_ blockAndCmt $ \(b,Just cmt) ->
    storeCommit cmt b valSet
  where
    blockAndCmt = take n
                $ tail
                $ mockchain `zip` (blockLastCommit <$> tail mockchain)

-- Start "consensus engine"
startConsensus :: TestM TestAlg Mock.BData (P2P.Event TestAlg Mock.BData)
startConsensus = do
  h     <- queryRO blockchainHeight
  varSt <- lift $ asks snd
  atomicallyIO $ writeTVar varSt (Just (succ h, TMState
    { smRound         = Round 0
    , smStep          = StepNewHeight 0
    , smProposals     = mempty
    , smPrevotesSet   = newHeightVoteSet valSet
    , smPrecommitsSet = newHeightVoteSet valSet
    , smLockedBlock   = Nothing
    , smLastCommit    = Nothing
    }))
  return $ EAnnouncement $ TxAnn $ AnnStep $ FullStep (succ h) (Round 0) (StepNewHeight 0)

-- Perform single step
step :: (Crypto alg, BlockData a)
     => Event alg a -> TestM alg a (P2P.SomeState alg a, [Command alg a])
step e = do
  cfg       <- lift $ asks fst
  st        <- get
  (st',cmd) <- lift $ lift $ P2P.handler cfg st e
  put st'
  return (st',cmd)


----------------------------------------------------------------

-- | Test 'Unknown' branch of peerGossipVotes.
--   Simple run and check that nothing changed.
--
testRawGossipUnknown :: IO ()
testRawGossipUnknown =
    withGossipEnv $ \peerChans gossipCh _env mempool -> do
        recvCh   <- liftIO newTChanIO
        pexCh    <- liftIO newTChanIO
        runConcurrently
            [ void $ peerFSM peerChans pexCh gossipCh recvCh mempool
            , waitSec 1
            ]
        -- TODO проверить, что ничего не поменялось


-- | Connected peer is lagging from current node, so
--   current node send 'GossipPreCommit' from previous heights/rounds.
--
testRawGossipLagging :: IO ()
testRawGossipLagging =
    withGossipEnv $ \peerChans gossipCh env@GossipEnv{..} mempool -> do
        addSomeBlocks env 10
        ourH  <- succ <$> queryRO blockchainHeight
        let step = FullStep (pred ourH) (Round 0) (StepNewHeight 0)
        recvCh   <- liftIO newTChanIO
        liftIO $ atomically $
          writeTChan recvCh (EGossip $ GossipAnn $ AnnStep step)
        pexCh    <- liftIO newTChanIO
        -- Run peerGossipVotes
        runConcurrently
            [ void $ peerFSM peerChans pexCh gossipCh recvCh mempool
            , waitForEvents envEventsQueue [(TepgvStarted, 1), (TepgvNewIter, 2), (TepgvLagging, 2)]
            , waitSec 5.0 >> throwM (TestError "Timeout!")
            ]
        -- Check sent messages
        liftIO $ do
            atomically (tryReadTBQueue gossipCh) >>= \case
                Just (GossipPreCommit _) -> return ()
                Nothing                  -> assertFailure "`gossipCh` does not contains any message!"
                msg                      -> assertFailure ("Wrong message: " ++ show msg)


-- | Connected peer is ahead of us, so do nothing.
--
testRawGossipAhead :: IO ()
testRawGossipAhead =
    withGossipEnv $ \peerChans gossipCh env@GossipEnv{..} mempool -> do
        addSomeBlocks env 10
        ourH  <- succ <$> queryRO blockchainHeight
        let step = FullStep (succ ourH) (Round 0) (StepNewHeight 0)
        recvCh   <- liftIO newTChanIO
        liftIO $ atomically $
          writeTChan recvCh (EGossip $ GossipAnn $ AnnStep step)
        pexCh    <- liftIO newTChanIO
        -- Run peerGossipVotes
        runConcurrently
            [ void $ peerFSM peerChans pexCh gossipCh recvCh mempool
            , waitForEvents envEventsQueue [(TepgvStarted, 1), (TepgvNewIter, 2), (TepgvAhead, 2)]
            , waitSec 5.0 >> throwM (TestError "Timeout!")
            ]
        -- Check sent messages
        liftIO $
            atomically (tryReadTBQueue gossipCh) >>= \case
                Just msg -> assertFailure $ "`gossipCh` contains some message: " ++ show msg ++ "!"
                Nothing  -> return ()



-- | Connected peer at our height/round, and we are proposal.
--
testRawGossipCurrentSentProposal :: IO ()
testRawGossipCurrentSentProposal = do
    withGossipEnv $ \peerChans gossipCh env@GossipEnv{..} mempool -> do
        (_lastBlock, lastCommit) <- last <$> addSomeBlocks' env 10
        ourH  <- succ <$> queryRO blockchainHeight
        let step = FullStep ourH (Round 0) (StepNewHeight 0)
        currentTime <- getCurrentTime
        let proposal = Proposal { propHeight = ourH
                                , propRound = Round 0
                                , propTimestamp = currentTime
                                , propPOL = Nothing
                                , propBlockID = commitBlockID lastCommit
                                }
        newTMState env ourH $ \tm -> tm { smProposals = Map.singleton (Round 0) (signValue currentNodeValIdx currentNodePrivKey proposal) }
        recvCh   <- liftIO newTChanIO
        liftIO $ atomically $
          writeTChan recvCh (EGossip $ GossipAnn $ AnnStep step)
        pexCh    <- liftIO newTChanIO
        -- Run peerGossipVotes
        buffer <- liftIO newTQueueIO
        resState <- liftIO $ newMVar $ wrap UnknownState
        runConcurrently
            [ peerFSM         peerChans pexCh gossipCh recvCh mempool >>= liftIO . modifyMVar_ resState . const . return
            , peerSend        peerChans gossipCh P2PConnection
                { send          = liftIO . atomically . writeTQueue buffer
                , recv          = forever (return ())
                , close         = return ()
                , connectedPeer = PeerInfo (PeerId 100) 100 100
                }
            , do waitForEvents envEventsQueue [(TepgvStarted, 1), (TepgvNewIter, 2), (TepgvCurrent, 1)]
                 liftIO $ atomically $ writeTChan recvCh EQuit
                 waitSec 0.1
            , waitSec 5.0 >> throwM (TestError "Timeout!")
            ]

        -- Check proposal message sent
        liftIO $
            (fmap deserialise <$> atomically (tryReadTQueue buffer)) >>= \case
                Just (GossipProposal (signedValue -> sentProposal)) ->
                    proposal @=? sentProposal
                Nothing -> assertFailure "`gossipCh` does not contains any message!"
                msg     -> assertFailure ("Wrong message: " ++ show msg)
        -- Check state has updated
        liftIO $ withMVar resState $ \case
          WrapState (Current ps) -> ps ^. peerProposals @?= Set.singleton (Round 0)
          s                      -> assertFailure ("Wrong state: " ++ show s)


-- | Auxiliary test function.
--   Prepare state according to arguments, run peerGossipVotes and then check messages and state
--
internalTestRawGossipCurrentCurrent :: Bool -> Bool -> Bool -> IO ()
internalTestRawGossipCurrentCurrent isTestingSendProposals isTestingSendPrevotes isTestingSendPrecommits =
    withGossipEnv $ \peerChans gossipCh env@GossipEnv{..} mempool -> do
        -- Prepare state
        (_lastBlock, lastCommit) <- last <$> addSomeBlocks' env 10
        ourH  <- succ <$> queryRO blockchainHeight
        let step = FullStep ourH (Round 0) (StepNewHeight 0)
        currentTime <- getCurrentTime
        let proposal = Proposal { propHeight = ourH
                                , propRound = Round 0
                                , propTimestamp = currentTime
                                , propPOL = Nothing
                                , propBlockID = commitBlockID lastCommit
                                }
        let vote :: Vote tag TestAlg Mock.BData
            vote = Vote { voteHeight = ourH
                        , voteRound = Round 0
                        , voteTime = currentTime
                        , voteBlockID = Just (commitBlockID lastCommit)
                        }
        newTMState env ourH $
            (\tm -> if isTestingSendProposals then
                        tm { smProposals = Map.singleton (Round 0) (signValue currentNodeValIdx currentNodePrivKey proposal) }
                    else tm) .
            (\tm -> if isTestingSendPrevotes then
                        case addSignedValue (Round 0) (signValue currentNodeValIdx currentNodePrivKey vote) (smPrevotesSet tm) of
                            InsertOK votes -> tm { smPrevotesSet = votes }
                            _ -> error "Can't insert votes"
                    else tm) .
            (\tm -> if isTestingSendPrecommits then
                        case addSignedValue (Round 0) (signValue currentNodeValIdx currentNodePrivKey vote) (smPrecommitsSet tm) of
                            InsertOK votes -> tm { smPrecommitsSet = votes }
                            _ -> error "Can't insert votes"
                    else tm)
        -- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        -- Run peerGossipVotes
        buffer <- liftIO newTQueueIO
        recvCh <- liftIO newTChanIO
        liftIO $ atomically $
          writeTChan recvCh (EGossip $ GossipAnn $ AnnStep step)
        pexCh  <- liftIO newTChanIO
        resState <- liftIO $ newMVar $ wrap UnknownState
        runConcurrently
            [ peerFSM peerChans pexCh gossipCh recvCh mempool >>= liftIO . modifyMVar_ resState . const . return
            , peerSend        peerChans gossipCh P2PConnection
                { send          = liftIO . atomically . writeTQueue buffer
                , recv          = forever (return ())
                , close         = return ()
                , connectedPeer = PeerInfo (PeerId 100) 100 100
                }
            , do waitForEvents envEventsQueue [(TepgvStarted, 1), (TepgvNewIter, 2), (TepgvCurrent, 1)]
                 liftIO $ atomically $ writeTChan recvCh EQuit
                 waitSec 0.1
            , waitSec 5.0 >> throwM (TestError "Timeout!")
            ]
        -- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        -- Check sent messages
        liftIO $ do
            messages <- fmap deserialise <$> atomically (flushTQueue buffer)
            let expectedNumberOfMessages = length $ filter id [isTestingSendProposals, isTestingSendPrevotes, isTestingSendPrecommits]
            assertEqual "" expectedNumberOfMessages (length messages)
            assertBool "" $ (if isTestingSendProposals then id else not) $ (flip any) messages $ \case
                    GossipProposal (signedValue -> sentProposal) -> proposal == sentProposal
                    _ -> False
            assertBool "" $ (if isTestingSendPrevotes then id else not) $ (flip any) messages $ \case
                    GossipPreVote (signedValue -> sentPrevote) -> vote == sentPrevote
                    _ -> False
            assertBool "" $ (if isTestingSendPrecommits then id else not) $ (flip any) messages $ \case
                    GossipPreCommit (signedValue -> sentPrecommit) -> coerce vote == sentPrecommit
                    _ -> False
        -- Check final state
        liftIO $ withMVar resState $ \case
          WrapState (Current ps)
            | isTestingSendProposals  -> ps ^. peerProposals @?= Set.singleton (Round 0)
            | isTestingSendPrevotes   -> ps ^. peerPrevotes @?= Map.singleton (Round 0) (insertValidatorIdx (ValidatorIdx 0) (emptyValidatorISet 4 ))
            | isTestingSendPrecommits -> ps ^. peerPrecommits @?= Map.singleton (Round 0) (insertValidatorIdx (ValidatorIdx 0) (emptyValidatorISet 4 ))
            | otherwise               -> True @?= True
          s                           -> assertFailure ("Wrong state: " ++ show s)


-- | Testing all combinations of behaviour
testRawGossipCurrent1, testRawGossipCurrent2,
  testRawGossipCurrent3, testRawGossipCurrent4,
  testRawGossipCurrent5, testRawGossipCurrent6,
  testRawGossipCurrent7, testRawGossipCurrent8  :: IO ()

testRawGossipCurrent1 = internalTestRawGossipCurrentCurrent False False False
testRawGossipCurrent2 = internalTestRawGossipCurrentCurrent True  False False
testRawGossipCurrent3 = internalTestRawGossipCurrentCurrent False True  False
testRawGossipCurrent4 = internalTestRawGossipCurrentCurrent False False True
testRawGossipCurrent5 = internalTestRawGossipCurrentCurrent True  True  False
testRawGossipCurrent6 = internalTestRawGossipCurrentCurrent True  False True
testRawGossipCurrent7 = internalTestRawGossipCurrentCurrent False True  True
testRawGossipCurrent8 = internalTestRawGossipCurrentCurrent True  True  True


-- * Some useful utilities ------------------------


-- | Private key of current test node.
--
currentNodePrivKey :: PrivKey TestAlg
currentNodePrivKey = validatorPrivKey $ head testValidators


currentNodeValIdx :: ValidatorIdx TestAlg
currentNodeValIdx = ValidatorIdx 0


-- | Some object in environment.
--
data GossipEnv = GossipEnv
    { envValidatorSet :: ValidatorSet TestAlg
    , envEventsQueue  :: Chan TraceEvents
    , envConsensus    :: TVar (Maybe (Height, TMState TestAlg Mock.BData))
    }


-- | Add some test blocks to blockchain.
addSomeBlocks
    :: (MonadIO m, MonadFail m, MonadThrow m, MonadDB m TestAlg Mock.BData)
    => GossipEnv
    -> Int       -- ^ Number of blocks to add
    -> m ()
addSomeBlocks env blocksCount = void $ addSomeBlocks' env blocksCount


-- | Add some test blocks to blockchain and return a list of these blocks & commits.
--
addSomeBlocks'
    :: (MonadIO m, MonadFail m, MonadThrow m, MonadDB m TestAlg Mock.BData)
    => GossipEnv
    -> Int -- ^ Number of blocks to add
    -> m [(Block TestAlg Mock.BData, Commit TestAlg Mock.BData)]
addSomeBlocks' GossipEnv{..} blocksCount =
  mapM addOneBlock [ Mock.BData [(show i, 4433)] | i <- [1 .. blocksCount] ]
  where
    addOneBlock tx = do
        t  <- getCurrentTime
        h  <- queryRO blockchainHeight
        -- st <- stateAtH bchState (succ h) -- WTF?
        let block = Block
                { blockHeader = Header
                    { headerHeight         = succ h
                    , headerLastBlockID    = Nothing -- TODO get from previous block
                    , headerValidatorsHash = Hashed $ Hash ""
                    , headerDataHash       = hashed tx
                    , headerValChangeHash  = hashed mempty
                    , headerLastCommitHash = Hashed $ Hash ""
                    , headerEvidenceHash   = hashed []
                    }
                , blockData       = tx
                , blockLastCommit = Nothing
                , blockEvidence   = []
                , blockValChange  = mempty
                }
            bid = blockHash block
            commit = Commit { commitBlockID    = bid
                            , commitPrecommits = NE.fromList [signValue currentNodeValIdx currentNodePrivKey Vote
                                      { voteHeight  = succ h
                                      , voteRound   = Round 0
                                      , voteTime    = t
                                      , voteBlockID = Just bid
                                      }]}
        mustQueryRW $ storeCommit commit block envValidatorSet
        return (block, commit)


-- | Prepare test environment and run callback in this environment with created objects.
--
withGossipEnv
    :: (    forall m . (MonadDB m TestAlg Mock.BData, MonadMask m, MonadIO m, MonadLogger m, MonadTrace m, MonadFork m, MonadFail m)
            => PeerChans m TestAlg Mock.BData
            -> TBQueue (GossipMsg TestAlg Mock.BData)
            -> GossipEnv
            -> MempoolCursor m TestAlg (TX Mock.BData)
            -> m ()
       )
    -> IO ()
withGossipEnv fun = do
    let dbValidatorSet = makeValidatorSetFromPriv testValidators
    eventsQueue <- newChan
    withConnection ":memory:" $ \conn -> do
        initDatabase conn (Mock.genesisBlock dbValidatorSet)
        runTracerT (writeChan eventsQueue) . runNoLogsT . runDBT conn $ do
            proposalStorage <- newSTMPropStorage
            peerChanTx              <- liftIO newTChanIO
            peerChanPex             <- liftIO newBroadcastTChanIO
            peerChanPexNewAddresses <- liftIO newTChanIO
            consensusState'         <- liftIO (newTVarIO Nothing)
            let consensusState      =  readTVar consensusState'
            gossipCnts              <- newGossipCounters
            let peerChans = PeerChans { proposalStorage = makeReadOnlyPS proposalStorage
                                      , p2pConfig       = cfgNetwork (defCfg :: Configuration Example)
                                      , peerChanRx      = const $ return ()
                                      , ..
                                      }
            gossipCh <- liftIO $ newTBQueueIO 1000
            cursor   <- getMempoolCursor nullMempool
            --
            let genv = GossipEnv dbValidatorSet eventsQueue consensusState'
            --
            fun peerChans gossipCh genv cursor


-- | Wait for events; fail if another event occurs.
--
waitForEvents :: (MonadIO m)
              => Chan TraceEvents
              -> [(PeerGossipVotesTraceEvents, Int)]
              -> m ()
waitForEvents queue events = liftIO $
    flip fix (Map.fromList events) $ \next residualEvents ->
        when (not $ Map.null residualEvents) $
            readChan queue >>= \case
                TePeerGossipVotes v ->
                    case Map.updateLookupWithKey (\_ n -> if n == 1 then Nothing else Just (pred n)) v residualEvents of
                        (Just _, re) -> next re
                        a            -> throwM $ TestError ("Excessive event: " ++ show v ++ ", residual events: " ++ show a)
                _ -> next residualEvents


-- | Create test TMState.
--
newTMState :: (MonadIO m)
           => GossipEnv
           -> Height
           -> (TMState TestAlg Mock.BData -> TMState TestAlg Mock.BData) -- ^ Postprocessor of created TMState
           -> m ()
newTMState GossipEnv{..} h postProcess = do
    let voteSet = newHeightVoteSet envValidatorSet
        tmState = postProcess $ TMState (Round 0) (StepNewHeight 0) Map.empty voteSet (coerce voteSet) Nothing Nothing
    liftIO $ atomically $ writeTVar envConsensus (Just (h, tmState))


data TestError = TestError String
  deriving Show
instance Exception TestError
