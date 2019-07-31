-- | Tests for gossip.
--
-- These tests prepare environment, call functions from Thundermint.P2P
-- and then check results. Because of these functions don't contain
-- any callback we can rely upon only tracing ('TepgvCurrent' for example).
--

{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE ViewPatterns      #-}

module TM.Gossip (tests) where


import Control.Concurrent.Chan
import Control.Concurrent.STM.TBQueue
import Control.Concurrent.STM.TChan
import Control.Concurrent.STM.TVar
import Control.Monad
import Control.Monad.Catch
import Control.Monad.Fail
import Control.Monad.Fix
import Control.Monad.IO.Class
import Control.Monad.STM
import Prelude                      as P
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict    as Map
import qualified Data.Set           as Set

import Thundermint.Blockchain.Internal.Engine.Types
import Thundermint.Blockchain.Internal.Types
import Thundermint.Control
import Thundermint.Crypto
import Thundermint.Crypto.Containers
import Thundermint.Debug.Trace
import Thundermint.Logger
import Thundermint.Mock.Types
import Thundermint.P2P
import Thundermint.P2P.PeerState
import Thundermint.Run
import Thundermint.Store
import Thundermint.Store.Internal.BlockDB (storeCommit, storeValSet)
import Thundermint.Store.STM
import Thundermint.Types.Blockchain
import Thundermint.Types.Validators
import Thundermint.Utils
import qualified Thundermint.Mock.KeyVal as Mock

import Test.Tasty
import Test.Tasty.HUnit

import TM.Util.Network


-- | Test 'Unknown' branch of peerGossipVotes.
--   Simple run and check that nothing changed.
--
testRawGossipUnknown :: IO ()
testRawGossipUnknown =
    withGossipEnv $ \peerStateObj peerChans gossipCh _env -> do
        runConcurrently
            [ peerGossipVotes peerStateObj peerChans gossipCh
            , waitSec 1
            ]
        -- TODO проверить, что ничего не поменялось


-- | Connected peer is lagging from current node, so
--   current node send 'GossipPreCommit' from previous heights/rounds.
--
testRawGossipLagging :: IO ()
testRawGossipLagging =
    withGossipEnv $ \peerStateObj peerChans gossipCh env@GossipEnv{..} -> do
        addSomeBlocks env 10
        ourH  <- succ <$> queryRO blockchainHeight
        advancePeer peerStateObj (FullStep (pred ourH) (Round 0) StepNewHeight)
        -- Run peerGossipVotes
        catchTestError $ runConcurrently
            [ peerGossipVotes peerStateObj peerChans gossipCh
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
    withGossipEnv $ \peerStateObj peerChans gossipCh env@GossipEnv{..} -> do
        addSomeBlocks env 10
        ourH  <- succ <$> queryRO blockchainHeight
        advancePeer peerStateObj (FullStep (succ ourH) (Round 0) StepNewHeight)
        -- Run peerGossipVotes
        catchTestError $ runConcurrently
            [ peerGossipVotes peerStateObj peerChans gossipCh
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
    withGossipEnv $ \peerStateObj peerChans gossipCh env@GossipEnv{..} -> do
        (_lastBlock, lastCommit) <- last <$> addSomeBlocks' env 10
        ourH  <- succ <$> queryRO blockchainHeight
        advancePeer peerStateObj (FullStep ourH (Round 0) StepNewHeight)
        currentTime <- getCurrentTime
        let proposal = Proposal { propHeight = ourH
                                , propRound = Round 0
                                , propTimestamp = currentTime
                                , propPOL = Nothing
                                , propBlockID = commitBlockID lastCommit
                                }
        newTMState env ourH $ \tm -> tm { smProposals = Map.singleton (Round 0) (signValue currentNodeValIdx currentNodePrivKey proposal) }
        prePeerState <- (getPeerState peerStateObj >>= \case
                Current cp -> return cp
                _ -> liftIO $ assertFailure "Wrong initial state")
        -- Run peerGossipVotes
        catchTestError $ runConcurrently
            [ peerGossipVotes peerStateObj peerChans gossipCh
            , waitForEvents envEventsQueue [(TepgvStarted, 1), (TepgvNewIter, 2), (TepgvCurrent, 1)]
            , waitSec 5.0 >> throwM (TestError "Timeout!")
            ]
        -- Check proposal message sent
        liftIO $
            atomically (tryReadTBQueue gossipCh) >>= \case
                Just (GossipProposal (signedValue -> sentProposal)) ->
                    proposal @=? sentProposal
                Nothing -> assertFailure "`gossipCh` does not contains any message!"
                msg     -> assertFailure ("Wrong message: " ++ show msg)
        -- Check state has updated
        getPeerState peerStateObj >>= liftIO . \case
            Current p -> p @?= (prePeerState { peerProposals = Set.insert (Round 0) (peerProposals prePeerState) })
            s         -> assertFailure ("Wrong state: " ++ show s)


-- | Auxiliary test function.
--   Prepare state according to arguments, run peerGossipVotes and then check messages and state
--
internalTestRawGossipCurrentCurrent :: Bool -> Bool -> Bool -> IO ()
internalTestRawGossipCurrentCurrent isTestingSendProposals isTestingSendPrevotes isTestingSendPrecommits =
    withGossipEnv $ \peerStateObj peerChans gossipCh env@GossipEnv{..} -> do
        -- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        -- Prepare state
        (_lastBlock, lastCommit) <- last <$> addSomeBlocks' env 10
        ourH  <- succ <$> queryRO blockchainHeight
        advancePeer peerStateObj (FullStep ourH (Round 0) StepNewHeight)
        currentTime <- getCurrentTime
        let proposal = Proposal { propHeight = ourH
                                , propRound = Round 0
                                , propTimestamp = currentTime
                                , propPOL = Nothing
                                , propBlockID = commitBlockID lastCommit
                                }
        let vote = Vote { voteHeight = ourH
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
        prePeerState <- (getPeerState peerStateObj >>= \case
                Current cp -> return cp
                _ -> liftIO $ assertFailure "Wrong initial state")
        -- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        -- Run peerGossipVotes
        catchTestError $ runConcurrently
            [ peerGossipVotes peerStateObj peerChans gossipCh
            , waitForEvents envEventsQueue [(TepgvStarted, 1), (TepgvNewIter, 2), (TepgvCurrent, 1)]
            , waitSec 5.0 >> throwM (TestError "Timeout!")
            ]
        -- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        -- Check sent messages
        liftIO $ do
            messages <- atomically $ flushTBQueue gossipCh
            let expectedNumberOfMessages = sum $ map fromEnum [isTestingSendProposals, isTestingSendPrevotes, isTestingSendPrecommits]
            assertEqual "" expectedNumberOfMessages (length messages)
            assertBool "" $ (if isTestingSendProposals then id else not) $ (flip any) messages $ \case
                    GossipProposal (signedValue -> sentProposal) -> proposal == sentProposal
                    _ -> False
            assertBool "" $ (if isTestingSendPrevotes then id else not) $ (flip any) messages $ \case
                    GossipPreVote (signedValue -> sentPrevote) -> vote == sentPrevote
                    _ -> False
            assertBool "" $ (if isTestingSendPrecommits then id else not) $ (flip any) messages $ \case
                    GossipPreCommit (signedValue -> sentPrecommit) -> vote == sentPrecommit
                    _ -> False
        -- Check final state
        let expectedPeerState =
                (\ps -> if isTestingSendProposals then
                            (ps { peerProposals = Set.insert (Round 0) (peerProposals ps) })
                        else ps) $
                (\ps -> if isTestingSendPrevotes then
                            (ps { peerPrevotes  = Map.insert (Round 0) (insertValidatorIdx (ValidatorIdx 0) (emptyValidatorISet 4 )) (peerPrevotes ps) })
                        else ps) $
                (\ps -> if isTestingSendPrecommits then
                            (ps { peerPrecommits = Map.insert (Round 0) (insertValidatorIdx (ValidatorIdx 0) (emptyValidatorISet 4 )) (peerPrecommits ps) })
                        else ps) $
                prePeerState
        getPeerState peerStateObj >>= liftIO . \case
            Current ps -> ps @?= expectedPeerState
            s          -> assertFailure ("Wrong state: " ++ show s)


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
currentNodePrivKey = validatorPrivKey $ snd $ head $ Map.toList testValidators


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
--
addSomeBlocks
    :: (MonadIO m, MonadFail m, MonadDB m TestAlg Mock.BData)
    => GossipEnv
    -> Int       -- ^ Number of blocks to add
    -> m ()
addSomeBlocks env blocksCount = void $ addSomeBlocks' env blocksCount


-- | Add some test blocks to blockchain and return a list of these blocks & commits.
--
addSomeBlocks'
    :: (MonadIO m, MonadFail m, MonadDB m TestAlg Mock.BData)
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
                    { headerChainID        = "TEST"
                    , headerHeight         = succ h
                    , headerTime           = t
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
        Just () <- queryRW $ storeCommit commit block
        Just () <- queryRW $ storeValSet block envValidatorSet
        return (block, commit)


-- | Prepare test environment and run callback in this environment with created objects.
--
withGossipEnv
    :: (    forall m . (MonadDB m TestAlg Mock.BData, MonadMask m, MonadIO m, MonadLogger m, MonadTrace m, MonadFork m, MonadFail m)
            => PeerStateObj m TestAlg Mock.BData
            -> PeerChans m TestAlg Mock.BData
            -> TBQueue (GossipMsg TestAlg Mock.BData)
            -> GossipEnv
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
            peerStateObj <- newPeerStateObj (makeReadOnlyPS proposalStorage)
            -- let peerId = 0xDEADC0DE
            let peerChanRx = \_ -> return ()
            peerChanTx              <- liftIO $ newTChanIO
            peerChanPex             <- liftIO newBroadcastTChanIO
            peerChanPexNewAddresses <- liftIO newTChanIO
            consensusState'         <- liftIO (newTVarIO Nothing)
            let consensusState      =  readTVar consensusState'
            gossipCnts              <- newGossipCounters
            let peerChans = PeerChans { proposalStorage = makeReadOnlyPS proposalStorage
                                      , p2pConfig       = cfgNetwork (defCfg :: Configuration Example)
                                      , ..
                                      }
            gossipCh <- liftIO $ newTBQueueIO 1000
            --
            let genv = GossipEnv dbValidatorSet eventsQueue consensusState'
            --
            fun peerStateObj peerChans gossipCh genv


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
    currentTime <- getCurrentTime
    let voteSet = newHeightVoteSet envValidatorSet currentTime
    let tmState = postProcess $ TMState (Round 0) StepNewHeight Map.empty voteSet voteSet Nothing Nothing
    liftIO $ atomically $ writeTVar envConsensus (Just (h, tmState))


data TestError = TestError String
  deriving Show
instance Exception TestError


catchTestError :: MonadCatch m => m a -> m a
catchTestError act = catch act (\err@(TestError _) -> P.fail $ show err)


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
    ]
