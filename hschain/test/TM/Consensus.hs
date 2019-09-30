{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE DeriveFunctor    #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE MultiWayIf       #-}
{-# LANGUAGE RecordWildCards  #-}
-- | Tests for consensus
--
module TM.Consensus (tests) where

import Control.Concurrent.STM
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Free
import Data.IORef
import Data.Int
import Text.Printf

import HSChain.Blockchain.Internal.Engine.Types
import HSChain.Blockchain.Internal.Engine
import HSChain.Blockchain.Internal.Types
import HSChain.Control
import HSChain.Crypto
import HSChain.Logger
import HSChain.Store
import HSChain.Store.STM
import HSChain.Types
import HSChain.Mock.KeyVal  (BData(..))

import Test.Tasty
import Test.Tasty.HUnit

import TM.Util.Network
import TM.Util.MockChain


----------------------------------------------------------------
-- Test tree
----------------------------------------------------------------

tests :: TestTree
tests = testGroup "eigen-consensus"
  [ testGroup "Single round"
    [ testCase (printf "key=%s PV=%i PC=%i" keyNm nPV nPC)
    $ testConsensusNormal nPV nPC k
    | (k,keyNm) <- [(k1,"k1"), (k2,"k2")]
    , nPV       <- [0 .. 3]
    , nPC       <- [0 .. 3]
    ]
  , testCase "Split vote byz=1" $ testSplitVote 1
  , testCase "Split vote byz=2" $ testSplitVote 2
  , testCase "Split vote byz=3" $ testSplitVote 3
  , testCase "WAL replay" testWalReplay
  ]

----------------------------------------------------------------
-- Consensus tests
--
-- We test consesus engine in separation from network code and run
-- only single node. All communications from other nodes are hardcoded
----------------------------------------------------------------

-- Test that engine behaves correctly for single round of consensus
-- when provided with different number of votes
testConsensusNormal :: Int -> Int -> PrivKey TestAlg -> IO ()
testConsensusNormal nPV nPC k = testConsensus k $ do
  -- Consensus enters new height and proposer
  ()  <- expectStep 1 0 (StepNewHeight 0)
  ()  <- expectStep 1 0 StepProposal
  -- let consensus engine create proposal or we should do it ourselves
  bid <- if | k == proposer -> propBlockID <$> expectProp
            | otherwise     -> proposeBlock (Round 0) proposer block1
  -- We prevote and wait for prevote from engine. It should prevote
  -- valid block
  () <- voteFor (Just bid) =<< expectPV
  prevote (Height 1) (Round 0) votersPV (Just bid)
  -- We precommit and wait for precommit from engine
  () <- voteFor (pcBID bid) =<< expectPC
  precommit (Height 1) (Round 0) votersPC (Just bid)
  -- If we have issued enough PV & PC node will commit block
  if | nPV >= 2 && nPC >= 2 -> checkCommit bid
     | nPC == 3             -> checkCommit bid
     | otherwise            -> expectStep 1 1 StepProposal
  where
    -- Proposer for H=1, R=0
    proposer = k1
    -- Voters for prevote & precommit
    votersPV = take nPV $ filter (/=k) privK
    votersPC = take nPC $ filter (/=k) privK
    -- We need enough prevotes for engine to precommit block
    pcBID | nPV >= 2  = Just
          | otherwise = const Nothing
    -- We have commited new block
    checkCommit bid = do
      expectStep 2 0 (StepNewHeight 0)
      lift (queryRO (retrieveBlockID (Height 1))) >>= \case
        Nothing                 -> error "testConsensusNormal: block is not commited"
        Just bid' | bid /= bid' -> error "testConsensusNormal: incorrect block commited"
                  | otherwise   -> return ()


-- Vote is split between two block.
--
--  - Out engine proposes correct block and hones nodes work according
--    to procol
--  - Byzantinous nodes prevote and precommit another block.
testSplitVote :: Int -> IO ()
testSplitVote nByz = testConsensus k1 $ do
  -- Consensus enters new height and proposes
  ()  <- expectStep 1 0 (StepNewHeight 0)
  ()  <- expectStep 1 0 StepProposal
  bid <- propBlockID <$> expectProp
  -- Prevote (split vote)
  () <- voteFor (Just bid) =<< expectPV
  prevote (Height 1) (Round 0) honestKeys (Just bid)
  prevote (Height 1) (Round 0) byzKeys    (Just byzBID)
  -- Precommit
  () <- voteFor (honestPC bid) =<< expectPC
  precommit (Height 1) (Round 0) honestKeys (honestPC bid)
  precommit (Height 1) (Round 0) byzKeys    (Just byzBID)
  -- Send byzantinous block once more. It was discarded earlise since
  -- it wasn't proposed
  reply [ RxBlock byzBlock ]
  -- If there's 1 byz. node we commit. Go to other round otherwise
  if | nByz <= 1 -> expectStep 2 0 (StepNewHeight 0)
     | nByz == 3 -> expectStep 2 0 (StepNewHeight 0)
     | otherwise -> expectStep 1 1 StepProposal
  where
    (byzKeys,honestKeys) = splitAt nByz [k2,k3,k4]
    byzBlock             = block1'
    byzBID               = blockHash byzBlock
    -- Honest nodes will prevote block iff we have no more that 1 byz. node
    honestPC | nByz <= 1 = Just
             | nByz == 3 = const (Just byzBID)
             | otherwise = const Nothing

-- Test that we replay WAL correctly. Most of all that we don't
-- generate different block!
--
-- See #446 for more details
testWalReplay :: IO ()
testWalReplay = withDatabase "" genesis $ \conn -> run conn $ do
  bidRef <- liftIO $ newIORef Nothing
  -- First run. We run consensus and then we abort execution of
  -- consensus engine.
  do (chans, action) <- startConsensus k1
     runConcurrently
       [ action
       , expect valSet chans $ do
           ()  <- expectStep 1 0 (StepNewHeight 0)
           ()  <- expectStep 1 0 StepProposal
           bid <- propBlockID <$> expectProp
           liftIO $ writeIORef bidRef $ Just bid
           -- Prevote
           () <- voteFor (Just bid) =<< expectPV
           prevote (Height 1) (Round 0) keys (Just bid)
           -- Precommit
           () <- voteFor (Just bid) =<< expectPC
           return ()
       ]
  -- Second run after crash. We should get same output from consensus
  -- engine without sending any input.
  do (chans, action) <- startConsensus k1
     runConcurrently
       [ action
       , expect valSet chans $ do
           ()  <- expectStep 1 0 (StepNewHeight 0)
           ()  <- expectStep 1 0 StepProposal
           bid <- propBlockID <$> expectProp
           ()  <- voteFor (Just bid) =<< expectPV
           ()  <- voteFor (Just bid) =<< expectPC
           -- Check that we generate same block!
           oldBid <- liftIO $ readIORef bidRef
           when (oldBid /= Just bid) $ error "WAL replay: BID mismatch!"
       ]
  where
    keys = [k2,k3,k4]



  
----------------------------------------------------------------
-- Helpers for running tests
----------------------------------------------------------------

type ConsensusM = DBT 'RW TestAlg BData (NoLogsT IO)

run :: Connection 'RW alg a -> DBT 'RW alg a (NoLogsT IO) x -> IO x
run c = runNoLogsT . runDBT c

-- Simple test of consensus
testConsensus :: PrivKey TestAlg -> Expect ConsensusM TestAlg BData () -> IO ()
testConsensus k messages = withDatabase "" genesis $ \conn -> run conn $ do
  (chans, action) <- startConsensus k
  runConcurrently
    [ action
    , expect valSet chans messages
    ]

startConsensus :: PrivKey TestAlg -> ConsensusM ( ( TChan   (MessageTx TestAlg BData)
                                                  , TBQueue (MessageRx 'Unverified TestAlg BData)
                                                  )
                                                , ConsensusM ()
                                                )
startConsensus k = do
  logic <- mkAppLogic
  chans <- newAppChans cfg
  ch    <- atomicallyIO $ dupTChan $ appChanTx chans
  return ( ( ch
           , appChanRx chans
           )
         , runApplication cfg (Just (PrivValidator k)) logic mempty chans
         )
  where
    cfg = cfgConsensus (defCfg :: Configuration FastTest)

-- Create default application logic
mkAppLogic :: MonadIO m => m (AppLogic m TestAlg BData)
mkAppLogic = do
  store <- newSTMBchStorage mempty
  cnt   <- liftIO $ newIORef 0
  return AppLogic
    { appBlockGenerator = \b _ -> do i <- liftIO $ readIORef cnt
                                     liftIO $ writeIORef cnt $! i + 1
                                     return ( BData [("K2", i)]
                                            , newBlockState b
                                            )
    , appValidationFun  = \_   -> return . Just
    , appMempool        = nullMempool
    , appBchState       = store
    }


----------------------------------------------------------------
-- High level API for interacting with consensus engine
----------------------------------------------------------------

type Expect m alg a = FreeT (ExpectF alg a) m

expectStep :: Monad m => Int64 -> Int64 -> Step -> Expect m alg a ()
expectStep h r s = wrap $ ExpectStep (Height h) (Round r) s (return ())

expectProp :: Monad m => Expect m alg a (Proposal alg a)
expectProp = wrap $ ExpectProp return

expectPV :: Monad m => Expect m alg a (Vote 'PreVote alg a)
expectPV = wrap $ ExpectPV return

expectPC :: Monad m => Expect m alg a (Vote 'PreCommit alg a)
expectPC = wrap $ ExpectPC return

reply :: Monad m => [MessageRx 'Unverified alg a] -> Expect m alg a ()
reply msg = wrap $ Reply msg (return ())

prevote
  :: (Monad m, Crypto alg)
  => Height
  -> Round
  -> [PrivKey alg]
  -> Maybe (BlockID alg a)
  -> Expect m alg a()
prevote h r keys mbid = do
  vals <- wrap $ GetValSet return
  reply [ RxPreVote $ signValue i k $ Vote h r (Time 0) mbid
        | k <- keys
        , let Just i = indexByValidator vals (publicKey k)
        ]

precommit
  :: (Monad m, Crypto alg)
  => Height
  -> Round
  -> [PrivKey alg]
  -> Maybe (BlockID alg a)
  -> Expect m alg a()
precommit h r keys mbid = do
  vals <- wrap $ GetValSet return
  reply [ RxPreCommit $ signValue i k $ Vote h r (Time 0) mbid
        | k <- keys
        , let Just i = indexByValidator vals (publicKey k)
        ]

proposeBlock
  :: (Monad m, Crypto alg)
  => Round
  -> PrivKey alg
  -> Block alg a
  -> Expect m alg a (BlockID alg a)
proposeBlock r k b = do
  vals <- wrap $ GetValSet return
  reply [ let Just i = indexByValidator vals (publicKey k)
          in  RxProposal $ signValue i k
                         $ Proposal (headerHeight (blockHeader b)) r (Time 0) Nothing bid
        , RxBlock b
        ]
  return bid
  where
    bid = blockHash b

voteFor :: Monad m => (Maybe (BlockID alg a)) -> Vote ty alg a -> m ()
voteFor mbid v
  | mbid == voteBlockID v = return ()
  | otherwise             = error $ unlines
                            [ "Unexpected vote. Expecting BID"
                            , "    " ++ show mbid
                            , "Got:"
                            , "    " ++ show (voteBlockID v)
                            ]


----------------------------------------------------------------
-- Harness for interacting with consensus engine
--
-- Free monads are used for no particular reason
----------------------------------------------------------------

-- What message do we expect from consensus engine and how should we
-- react to it.
data ExpectF alg a x
  = ExpectStep Height Round Step x
  | ExpectProp (Proposal        alg a -> x)
  | ExpectPV   (Vote 'PreVote   alg a -> x)
  | ExpectPC   (Vote 'PreCommit alg a -> x)
  | Reply      [MessageRx 'Unverified alg a] x
  | GetValSet  (ValidatorSet alg -> x)
  deriving (Functor)

-- Read outbound messages from consensus engine and reply to it
-- according to instructions. Unexpeceted messages will result in
-- exception
expect
  :: (MonadIO m, Crypto alg)
  => ValidatorSet alg
  -> ( TChan (MessageTx alg a)
     , TBQueue (MessageRx 'Unverified alg a)
     )
  -> Expect m alg a ()
  -> m ()
expect vals (chTx, chRx) expected = do
  iterT step expected
  where
    step func = case func of
      ExpectStep h r s next -> readMsg >>= \case
        TxAnn (AnnStep (FullStep h' r' s'))
          | h == h'
          , r == r'
          , s == s' -> next
        m -> failure m
      ExpectProp next  -> readMsg >>= \case
        TxProposal sp  -> next $ signedValue sp
        m              -> failure m
      ExpectPV   next  -> readMsg >>= \case
        TxPreVote sv   -> next $ signedValue sv
        m              -> failure m
      ExpectPC   next  -> readMsg >>= \case
        TxPreCommit sv -> next $ signedValue sv
        m              -> failure m
      Reply msg next   -> do
        atomicallyIO $ mapM_ (writeTBQueue chRx) msg
        next
      GetValSet next   -> next vals
      where
        failure m = error $ unlines [ "Expected:"
                                    , "    " ++ expectedName func
                                    , "Got"
                                    , "    " ++ show m
                                    ]
    --
    readMsg = do
      m <- liftIO $ atomically $ readTChan chTx
      case m of
        TxAnn AnnStep{} -> return m
        TxAnn _         -> readMsg
        _               -> return m
    --
    expectedName = \case
      ExpectStep h r s _ -> "Step " ++ show h ++ " " ++ show r ++ " " ++ show s
      ExpectProp{}       -> "Proposal"
      ExpectPV{}         -> "Prevote"
      ExpectPC{}         -> "Precommit"
      _                  -> "<shouldn't happen>"
