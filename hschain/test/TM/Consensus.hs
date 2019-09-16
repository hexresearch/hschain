{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}
-- | Tests for consensus
--
module TM.Consensus (tests) where

import Control.Concurrent.STM
import Control.Monad.Trans.Free
import Control.Monad.Catch
import Control.Monad.IO.Class
import Data.Int
import Data.IORef

import HSChain.Blockchain.Internal.Engine.Types
import HSChain.Blockchain.Internal.Engine
import HSChain.Blockchain.Internal.Types
import HSChain.Control
import HSChain.Crypto
import HSChain.Logger
import HSChain.Store
import HSChain.Store.STM
import HSChain.Run
import HSChain.Types
import HSChain.Utils
import HSChain.Mock.KeyList
import HSChain.Mock.KeyVal  (BData(..))
import HSChain.Mock.Types   (makeGenesis)

import Test.Tasty
import Test.Tasty.HUnit

import TM.Util.Network

----------------------------------------------------------------
-- Test tree
----------------------------------------------------------------

tests :: TestTree
tests = testGroup "eigen-consensus"
    [ testCase "Some byzantine 1" testConsensusLightByzantine1
    , testCase "Some byzantine 2" testConsensusLightByzantine2
    , testCase "Some byzantine 3" testConsensusLightByzantine3
    , testCase "Some byzantine 4" testConsensusLightByzantine4
    , testCase "Some byzantine 5" testConsensusLightByzantine5
    , testCase "Some byzantine 6" testConsensusLightByzantine6

    , testCase "Normal execution (P)"          testConsensusNormal1
    , testCase "Normal execution (not P)"      testConsensusNormal2
    , testCase "Will go to next round (p)"     testConsensusSilence1
    , testCase "Will go to next round (not P)" testConsensusSilence2
    ]

----------------------------------------------------------------
-- Consensus tests
--
-- We test consesus engine in separation from network code and run
-- only single node. All communications from other nodes are hardcoded
----------------------------------------------------------------

testConsensusNormal1 :: IO ()
testConsensusNormal1 = testConsensus k1 $ do
  ()                        <- expectStep 1 0 (StepNewHeight 0)
  ()                        <- expectStep 1 0 StepProposal
  Proposal{propBlockID=bid} <- expectProp
  prevote (Height 1) (Round 0) [k2,k3,k4] (Just bid)
  () <- voteFor (Just bid) =<< expectPV
  precommit (Height 1) (Round 0) [k2,k3,k4] (Just bid)
  () <- voteFor (Just bid) =<< expectPC
  expectStep 2 0 (StepNewHeight 0)

testConsensusNormal2 :: IO ()
testConsensusNormal2 = testConsensus k2 $ do
  ()  <- expectStep 1 0 (StepNewHeight 0)
  ()  <- expectStep 1 0 StepProposal
  bid <- proposeBlock (Round 0) k1 block1
  prevote (Height 1) (Round 0) [k1,k3,k4] (Just bid)
  () <- voteFor (Just bid) =<< expectPV
  precommit (Height 1) (Round 0) [k1,k3,k4] (Just bid)
  () <- voteFor (Just bid) =<< expectPC
  expectStep 2 0 (StepNewHeight 0)

testConsensusSilence1 :: IO ()
testConsensusSilence1 = testConsensus k1 $ do
  expectStep 1 0 (StepNewHeight 0)
  expectStep 1 0 StepProposal
  _ <- expectProp
  _ <- expectPV
  _ <- expectPC
  expectStep 1 1 StepProposal

testConsensusSilence2 :: IO ()
testConsensusSilence2 = testConsensus k2 $ do
  expectStep 1 0 (StepNewHeight 0)
  expectStep 1 0 StepProposal
  _ <- expectPV
  _ <- expectPC
  expectStep 1 1 StepProposal


----------------------------------------------------------------
-- Helpers for running tests
----------------------------------------------------------------

type ConsensusM = DBT 'RW TestAlg BData (NoLogsT IO)

run :: Connection 'RW alg a -> DBT 'RW alg a (NoLogsT IO) x -> IO x
run c = runNoLogsT . runDBT c

-- Simple test of consensus
testConsensus :: PrivKey TestAlg -> Expect ConsensusM TestAlg BData () -> IO ()
testConsensus k messages = withDatabase "" genesis $ \conn -> run conn $ do
  logic <- mkAppLogic
  chans <- newAppChans cfg
  runConcurrently
    [ runApplication cfg
        (Just (PrivValidator k))
        logic mempty chans
    , expect valSet chans messages
    ]
  where
    cfg = cfgConsensus (defCfg :: Configuration FastTest)

-- Create default application logic
mkAppLogic :: MonadIO m => m (AppLogic m TestAlg BData)
mkAppLogic = do
  store <- newSTMBchStorage mempty
  return AppLogic
    { appBlockGenerator = \b _ -> return ( BData [("K", 0)]
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
  -> AppChans m alg a
  -> Expect m alg a ()
  -> m ()
expect vals AppChans{..} expected = do
  ch <- atomicallyIO $ dupTChan appChanTx
  iterT (step ch) expected
  where
    step ch func = case func of
      ExpectStep h r s next -> readMsg ch >>= \case
        TxAnn (AnnStep (FullStep h' r' s'))
          | h == h'
          , r == r'
          , s == s' -> next
        m -> failure m func
      ExpectProp next  -> readMsg ch >>= \case
        TxProposal sp  -> next $ signedValue sp
        m              -> failure m
      ExpectPV   next  -> readMsg ch >>= \case
        TxPreVote sv   -> next $ signedValue sv
        m              -> failure m
      ExpectPC   next  -> readMsg ch >>= \case
        TxPreCommit sv -> next $ signedValue sv
        m              -> failure m
      Reply msg next   -> do
        atomicallyIO $ mapM_ (writeTBQueue appChanRx) msg
        next
      GetValSet next   -> next vals
      where
        failure m = error $ unlines [ "Expected:"
                                    , "    " ++ expectedName func
                                    , "Got"
                                    , "    " ++ show m
                                    ]
    --
    readMsg ch = do
      m <- liftIO $ atomically $ readTChan ch
      case m of
        TxAnn AnnStep{} -> return m
        TxAnn _         -> readMsg ch
        _               -> return m
    --
    expectedName = \case
      ExpectStep h r s _ -> "Step " ++ show h ++ " " ++ show r ++ " " ++ show s
      ExpectProp{}       -> "Proposal"
      ExpectPV{}         -> "Prevote"
      ExpectPC{}         -> "Precommit"
      _                  -> "<shouldn't happen>"

----------------------------------------------------------------
-- Constants
----------------------------------------------------------------

-- Genesis block of BCh
genesis :: Block TestAlg BData
genesis = makeGenesis (BData []) valSet

block1 :: Block TestAlg BData
block1 = Block
  { blockHeader = Header
      { headerHeight         = Height 1
      , headerLastBlockID    = Just $ blockHash genesis
      , headerValidatorsHash = hashed valSet
      , headerValChangeHash  = hashed mempty
      , headerDataHash       = hashed dat
      , headerLastCommitHash = hashed Nothing
      , headerEvidenceHash   = hashed []
      }
  , blockData       = dat
  , blockValChange  = mempty
  , blockLastCommit = Nothing
  , blockEvidence   = []
  }
  where
    dat = BData [("K",100)]

privK       :: [PrivKey TestAlg]
k1,k2,k3,k4 :: PrivKey TestAlg
privK@[k1,k2,k3,k4] = take 4 $ makePrivKeyStream 1337

valSet :: ValidatorSet TestAlg
Right valSet = makeValidatorSet [Validator (publicKey k) 1 | k <- privK]

----------------------------------------------------------------
-- Old consesnsus tests
----------------------------------------------------------------


-- | В этом тесте одна из четырёх нод работает медленно
--   консенсус есть, БЧ растёт.
--
testConsensusLightByzantine1 :: IO ()
testConsensusLightByzantine1 = do
    achievedHeight <- newIORef (Height 0)
    runConcurrently
        [ createGossipTestNetwork
            [ byz $ \vote -> liftIO (waitSec 3.0) >> return (Just vote)
            , mempty
            , mempty
            , waitHeight (Height 2) achievedHeight
            ]
        , waitSec 10
        ]
    readIORef achievedHeight >>= assertBool "Must achieve some height" . (>= Height 1)


-- | В этом тесте одна из четырёх нод работает медленно
--   консенсус есть, БЧ растёт.
--
testConsensusLightByzantine2 :: IO ()
testConsensusLightByzantine2 = do
    achievedHeight <- newIORef (Height 0)
    runConcurrently
        [ createGossipTestNetwork
            [ byz $ \_ -> return Nothing
            , mempty
            , mempty
            , waitHeight (Height 2) achievedHeight
            ]
        , waitSec 10
        ]
    readIORef achievedHeight >>= assertBool "Must achieve some height" . (>= Height 1)


-- | В этом тесте одна из четырёх нод выдаёт неправильный голос;
--   консенсус есть, БЧ растёт.
--
testConsensusLightByzantine3 :: IO ()
testConsensusLightByzantine3 = do
    achievedHeight <- newIORef (Height 0)
    runConcurrently
        [ createGossipTestNetwork
            [ byz $ \vote -> return (Just $ vote { voteHeight = Height 555 })
            , byz $ \vote -> return (Just $ vote { voteHeight = Height 666 })
            , mempty
            , waitHeight (Height 2) achievedHeight
            ]
        , waitSec 5
        ]
    readIORef achievedHeight >>= assertEqual "Must stay on initial height" (Height 0)


-- | В этом тесте две из четырёх нод работает медленно;
--   консенсуса не образуется и поэтому сеть остаётся на исходной высоте.
--
testConsensusLightByzantine4 :: IO ()
testConsensusLightByzantine4 = do
    achievedHeight <- newIORef (Height 0)
    runConcurrently
        [ createGossipTestNetwork
            [ byz $ \vote -> liftIO (waitSec 3.0) >> return (Just vote)
            , byz $ \vote -> liftIO (waitSec 3.0) >> return (Just vote)
            , mempty
            , waitHeight (Height 2) achievedHeight
            ]
        , waitSec 5
        ]
    readIORef achievedHeight >>= assertEqual "Must stay on initial height" (Height 0)


-- | В этом тесте две из четырёх нод пропускают выдачу голоса;
--   консенсуса не образуется и поэтому сеть остаётся на исходной высоте.
--
testConsensusLightByzantine5 :: IO ()
testConsensusLightByzantine5 = do
    achievedHeight <- newIORef (Height 0)
    runConcurrently
        [ createGossipTestNetwork
            [ byz $ \_ -> return Nothing
            , byz $ \_ -> return Nothing
            , mempty
            , waitHeight (Height 2) achievedHeight
            ]
        , waitSec 5
        ]
    readIORef achievedHeight >>= assertEqual "Must stay on initial height" (Height 0)


-- | В этом тесте две из четырёх нод выдают не тот голос;
--   консенсуса не образуется и поэтому сеть остаётся на исходной высоте.
--
testConsensusLightByzantine6 :: IO ()
testConsensusLightByzantine6 = do
    achievedHeight <- newIORef (Height 0)
    runConcurrently
        [ createGossipTestNetwork
            [ byz $ \vote -> return (Just $ vote { voteHeight = Height 555 })
            , byz $ \vote -> return (Just $ vote { voteHeight = Height 666 })
            , mempty
            , waitHeight (Height 2) achievedHeight
            ]
        , waitSec 5
        ]
    readIORef achievedHeight >>= assertEqual "Must stay on initial height" (Height 0)


-- * Some useful utilities ------------------------


blockHeight :: Block alg a -> Height
blockHeight = headerHeight . blockHeader


byz :: (Monad m)
    => (Vote 'PreVote alg a -> m (Maybe (Vote 'PreVote alg a)))
    -> AppCallbacks m alg a
byz byzc = mempty
  { appByzantine = mempty { byzantineCastPrevote = Just $ byzc }
  }


waitHeight :: (MonadIO m, MonadThrow m)
           => Height
           -> IORef Height
           -> (AppCallbacks m alg a)
waitHeight h achievedHeightIORef
  =  mempty { appCommitCallback = liftIO . writeIORef achievedHeightIORef . blockHeight }
  <> callbackAbortAtH h
