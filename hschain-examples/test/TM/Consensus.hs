{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE MultiWayIf          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns        #-}
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
import Data.Maybe
import Text.Printf

import HSChain.Blockchain.Internal.Engine.Types
import HSChain.Blockchain.Internal.Engine
import HSChain.Blockchain.Internal.Types
import HSChain.Control
import HSChain.Crypto
import HSChain.Logger
import HSChain.Store
import HSChain.Store.STM
import HSChain.Store.Internal.Query
import HSChain.Store.Internal.Proposals
import HSChain.Types
import HSChain.Mock.KeyVal  (BData(..),process)
import HSChain.Types.Merkle.Types

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
    | (k,keyNm) <- [(k1,"k1"::String), (k2,"k2")]
    , nPV       <- [0 .. 3]
    , nPC       <- [0 .. 3]
    ]
  , testCase "Two blocks"  test2Blocks
  , testCase "Split vote byz=1" $ testSplitVote 1
  , testCase "Split vote byz=2" $ testSplitVote 2
  , testCase "Split vote byz=3" $ testSplitVote 3
  , testCase "WAL replay" testWalReplay
  , testGroup "Locking"
    [ testCase ("Key=" ++ nm) $ testLocking k
    | (nm,k) <- [("k1",k1), ("k2",k2), ("k3",k3), ("k4",k4)]
    ]
  , testGroup "Evidence"
    [ testCase "PV stored immedieately"   evidenceIsStoredImmediatelyPV
    , testCase "PC stored immedieately"   evidenceIsStoredImmediatelyPC
    , testCase "Prop stored immedieately" evidenceIsStoredImmediatelyProp
    , testCase "Evidence is included in block" evidenceIsRecordedProp
    , testCase "Evidence (out of turn) BAD"  $ evidenceValidated False (evidenceOutOfTurn False)
    , testCase "Evidence (out of turn) GOOD" $ evidenceValidated True  (evidenceOutOfTurn True)
    , testGroup "PreVote validation"
      [ testCase "Confict"   $ evidenceValidated True  $ conflictingVote         ConflictingPreVote
      , testCase "Order"     $ evidenceValidated False $ badConflictingvoteOrder ConflictingPreVote
      , testCase "Same vote" $ evidenceValidated False $ badConflictVoteSame     ConflictingPreVote
      , testCase "Diff. R"   $ evidenceValidated False $ badConflictVoteDiffR    ConflictingPreVote
      , testCase "Diff. H"   $ evidenceValidated False $ badConflictVoteDiffH    ConflictingPreVote
      , testCase "Bad sign"  $ evidenceValidated False $ badConflictVoteSign     ConflictingPreVote
      ]
    , testGroup "PreCommit validation"
      [ testCase "Confict"   $ evidenceValidated True  $ conflictingVote         ConflictingPreCommit
      , testCase "Order"     $ evidenceValidated False $ badConflictingvoteOrder ConflictingPreCommit
      , testCase "Same vote" $ evidenceValidated False $ badConflictVoteSame     ConflictingPreCommit
      , testCase "Diff. R"   $ evidenceValidated False $ badConflictVoteDiffR    ConflictingPreCommit
      , testCase "Diff. H"   $ evidenceValidated False $ badConflictVoteDiffH    ConflictingPreCommit
      , testCase "Bad sign"  $ evidenceValidated False $ badConflictVoteSign     ConflictingPreCommit
      ]
    ]
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
    proposer = proposerKey (Height 1) (Round 0)
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


-- Tets generations of two blocks. Mostly in order to check whether
-- mockchain is generated properly
test2Blocks :: IO ()
test2Blocks = testConsensus k4 $ do
  -- H=1
  do ()  <- expectStep 1 0 (StepNewHeight 0)
     ()  <- expectStep 1 0 StepProposal
     bid <- proposeBlock (Round 0) k_H1 $ mockchain !! 1
     -- PREVOTE
     ()  <- voteFor (Just bid) =<< expectPV
     prevote (Height 1) (Round 0) [k1,k2] (Just bid)
      -- PRECOMMIT
     () <- voteFor (Just bid) =<< expectPC
     precommit (Height 1) (Round 0) [k1,k2] (Just bid)
  -- H=2
  do ()  <- expectStep 2 0 (StepNewHeight 0)
     ()  <- expectStep 2 0 StepProposal
     bid <- proposeBlock (Round 0) k_H2 $ mockchain !! 2
     -- PREVOTE
     ()  <- voteFor (Just bid) =<< expectPV
     prevote (Height 2) (Round 0) [k1,k2] (Just bid)
     --  -- PRECOMMIT
     -- () <- voteFor (Just bid) =<< expectPC
     -- precommit (Height 2) (Round 0) [k1,k2] (Just bid)
  where
    k_H1 = proposerKey (Height 1) (Round 0)
    k_H2 = proposerKey (Height 2) (Round 0)

-- Vote is split between two block.
--
--  - Out engine proposes correct block and hones nodes work according
--    to procol
--  - Byzantinous nodes prevote and precommit another block.
testSplitVote :: Int -> IO ()
testSplitVote nByz = testConsensus thisK $ do
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
    thisK                = proposerKey (Height 1) (Round 0)
    otherK               = filter (/=thisK) privK
    (byzKeys,honestKeys) = splitAt nByz otherK
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
testWalReplay = withEnvironment $ do
  bidRef <- liftIO $ newIORef Nothing
  -- First run. We run consensus and then we abort execution of
  -- consensus engine.
  execConsensus thisK $ do
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
  -- Second run after crash. We should get same output from consensus
  -- engine without sending any input.
  execConsensus thisK $ do
    ()  <- expectStep 1 0 (StepNewHeight 0)
    ()  <- expectStep 1 0 StepProposal
    bid <- propBlockID <$> expectProp
    ()  <- voteFor (Just bid) =<< expectPV
    ()  <- voteFor (Just bid) =<< expectPC
    -- Check that we generate same block!
    oldBid <- liftIO $ readIORef bidRef
    when (oldBid /= Just bid) $ error "WAL replay: BID mismatch!"
  where
    thisK = proposerKey (Height 1) (Round 0)
    keys  = filter (/=thisK) privK

-- Test case which is used for illustration for necessity of locking
-- in "Tendermint: byzantine fault tolerance in the age of
-- blockchains", §3.2.3
testLocking :: PrivKey TestAlg -> IO ()
testLocking k = testConsensus k $ do
  -- Consensus enters new height and block is proposed
  ()  <- expectStep 1 0 (StepNewHeight 0)
  ()  <- expectStep 1 0 StepProposal
  bid <- if | k == proposer1 -> propBlockID <$> expectProp
            | otherwise      -> proposeBlock (Round 0) proposer1 block1
  -- PREVOTE
  --
  -- All validators except "A" get PoLCa
  () <- voteFor (Just bid) =<< expectPV
  when (k /= kA) $ do
    prevote (Height 1) (Round 0) votersPV (Just bid)
  -- PRECOMMIT
  --
  -- Only "D" gets precommits
  () <- voteFor (if k == kA then Nothing else Just bid) =<< expectPC
  when (k == kD) $ do
    precommit (Height 1) (Round 0) votersPC (Just bid)
    precommit (Height 1) (Round 0) [k1]     Nothing
     -- "D" enters new height
  if | k == kD   -> expectStep 2 0 (StepNewHeight 0)
     -- "A,B,C" enter round 1
     | otherwise -> do
         expectStep 1 1 StepProposal
         -- Proposer must be locked on previously proposed block for
         -- which it precommitted
         when (k == proposer2) $ do
           p <- expectProp
           unless (propBlockID p == bid && propPOL p == Just (Round 0))
             $ error "Invalid POL"
            -- "A" could vote whatever
         if | k == kA   -> return ()
            -- Other validators should vote for locked block even if
            -- they didn't see proposal
            | otherwise -> voteFor (Just bid) =<< expectPV
  where
    -- We name keys accordint to names in paper
    kA = k1
    kD = k4
    --
    proposer1 = proposerKey (Height 1) (Round 0)
    proposer2 = proposerKey (Height 2) (Round 0)
    --
    votersPV = filter (/=k) privK
    votersPC = filter (/=k1) $ filter (/=k) privK


evidenceIsStoredImmediatelyProp :: IO ()
evidenceIsStoredImmediatelyProp = withEnvironment $ do
  execConsensus proposerH1R0 $ do
    -- Consensus enters new height and proposer
    ()  <- expectStep 1 0 (StepNewHeight 0)
    -- OUT OT TURN PROPOSAL
    _   <- proposeBlock (Round 0) k4 block1
    ()  <- expectStep 1 0 StepProposal
    bid <- propBlockID <$> expectProp
    -- PREVOTE
    () <- voteFor (Just bid) =<< expectPV
    return ()
  --
  ev :: [(CBORed (ByzantineEvidence TestAlg BData), Bool)] <- queryRO $
    basicQuery_ "SELECT evidence,recorded FROM thm_evidence"
  case ev of
      [(CBORed (OutOfTurnProposal _), False)]
        -> return ()
      _ -> error $ unlines $ "Incorrect evidence: " : map show ev


evidenceIsStoredImmediatelyPV :: IO ()
evidenceIsStoredImmediatelyPV = withEnvironment $ do
  execConsensus proposerH1R0 $ do
    -- Consensus enters new height and proposer
    ()  <- expectStep 1 0 (StepNewHeight 0)
    ()  <- expectStep 1 0 StepProposal
    bid <- propBlockID <$> expectProp
    -- PREVOTE: k2,k3 vote normally. k4 votes for two blocks!
    () <- voteFor (Just bid) =<< expectPV
    prevote (Height 1) (Round 0) othersH1R0 (Just bid)
    prevote (Height 1) (Round 0) [k4]       (Just bid')
    voteFor (Just bid) =<< expectPC
    expectStep 1 1 StepProposal
  ----------------
  queryRO selectAllEvidence >>= \case
    [(CBORed (ConflictingPreVote v1 v2), False)]
      | conflictingVotesOK v1 v2 -> return ()
    ev -> error $ unlines $ "Incorrect evidence: " : map show ev
  where
    bid' = blockHash block1'


evidenceIsStoredImmediatelyPC :: IO ()
evidenceIsStoredImmediatelyPC = withEnvironment $ do
  execConsensus proposerH1R0 $ do
    -- Consensus enters new height and proposer
    ()  <- expectStep 1 0 (StepNewHeight 0)
    ()  <- expectStep 1 0 StepProposal
    bid <- propBlockID <$> expectProp
    -- PREVOTE: k2,k3 vote normally. k4 votes for two blocks!
    () <- voteFor (Just bid) =<< expectPV
    prevote (Height 1) (Round 0) othersH1R0 (Just bid)
    -- PRECOMMIT: vote normally
    () <- voteFor (Just bid) =<< expectPC
    precommit (Height 1) (Round 0) [k4] (Just bid')
    precommit (Height 1) (Round 0) [k4] (Just bid)
    expectStep 1 1 StepProposal
  ----------------
  queryRO selectAllEvidence >>= \case
    [(CBORed (ConflictingPreCommit v1 v2), False)]
      | conflictingVotesOK v1 v2 -> return ()
    ev -> error $ unlines $ "Incorrect evidence: " : map show ev
  where
    bid' = blockHash block1'

evidenceIsRecordedProp :: IO ()
evidenceIsRecordedProp = withEnvironment $ do
  execConsensus proposerH2R0 $ do
    -- H=1
    do ()   <- expectStep 1 0 (StepNewHeight 0)
       -- OUT OT TURN PROPOSAL
       ()  <- expectStep 1 0 StepProposal
       _   <- proposeBlock (Round 1) k4 block1
       bid <- propBlockID <$> expectProp
       ()  <- voteFor (Just bid) =<< expectPV
       prevote   (Height 1) (Round 0) otherK (Just bid)
       ()  <- voteFor (Just bid) =<< expectPC
       precommit (Height 1) (Round 0) otherK (Just bid)
    -- H=2
    do () <- expectStep 2 0 (StepNewHeight 0)
       () <- expectStep 2 0 StepProposal
       -- Check that block contain evidence
       bid    <- propBlockID <$> expectProp
       Just b <- lookupBID (Height 2) bid
       case merkleValue $ blockEvidence b of
         [OutOfTurnProposal _] -> return ()
         e                     -> error $ unlines $ "Evidence:" : map show e
       ()   <- voteFor (Just bid) =<< expectPV
       prevote   (Height 2) (Round 0) otherK (Just bid)
       ()   <- voteFor (Just bid) =<< expectPC
       precommit (Height 2) (Round 0) otherK (Just bid)
    -- H=3. Block with evidence is commited
    expectStep 3 0 (StepNewHeight 0)
    expectStep 3 0 StepProposal
  --
  queryRO selectAllEvidence >>= \case
    [(CBORed (OutOfTurnProposal _), True)] -> return ()
    ev -> error $ unlines $ "Incorrect evidence: " : map show ev
  where
    otherK = take 2 $ filter (/=proposerH2R0) privK

selectAllEvidence
  :: MonadQueryRO m alg a
  => m [(CBORed (ByzantineEvidence TestAlg BData), Bool)]
selectAllEvidence
  = basicQuery_ "SELECT evidence,recorded FROM thm_evidence"


evidenceValidated
  :: Bool
  -> ByzantineEvidence TestAlg BData
  -> IO ()
evidenceValidated ok ev = testConsensus k4 $ do
  -- HEIGHT 1
  do ()  <- expectStep 1 0 (StepNewHeight 0)
     ()  <- expectStep 1 0 StepProposal
     bid <- proposeBlock (Round 0) proposerH1R0 (mockchain !! 1)     
     ()  <- voteFor (Just bid) =<< expectPV
     prevote (Height 1) (Round 0) [k2,k3] (Just bid)
     ()  <- voteFor (Just bid) =<< expectPC
     precommit (Height 1) (Round 0) [k2,k3] (Just bid)
  -- HEIGHT 2
  do expectStep 2 0 (StepNewHeight 0)
     expectStep 2 0 StepProposal
     --
     bid <- proposeBlock (Round 0) proposerH2R0 b
     ()  <- voteFor (expectedBID bid) =<< expectPV
     return ()
  where
    expectedBID | ok        = Just
                | otherwise = const Nothing
    b0 = mockchain !! 2
    b  = b0 { blockEvidence = merkled [ev] }

-- Proposals made out of turn
evidenceOutOfTurn :: Bool -> ByzantineEvidence TestAlg BData
evidenceOutOfTurn ok
  = OutOfTurnProposal
  $ signValue i k
  $ Proposal (Height 1) r (Time 0) Nothing (blockHash block1)
  where
    k = proposerH1R0
    r | ok        = Round 5
      | otherwise = Round 0
    Just i = indexByValidator valSet (publicKey k)

conflictingVote,badConflictingvoteOrder,badConflictVoteSame, badConflictVoteDiffR,
  badConflictVoteDiffH,badConflictVoteSign
  :: (CryptoHashable (Vote ty TestAlg BData))
  => (forall alg a. Signed 'Unverified alg (Vote ty alg a)
                 -> Signed 'Unverified alg (Vote ty alg a)
                 -> ByzantineEvidence alg a)
  -> ByzantineEvidence TestAlg BData
-- Conflicting vote
conflictingVote make = make (min v1 v2) (max v1 v2)
  where
    Just i = indexByValidator valSet (publicKey k4)
    v1     = signValue i k4 $ Vote (Height 1) (Round 0) (Time 0) (Just $ blockHash block1)
    v2     = signValue i k4 $ Vote (Height 1) (Round 0) (Time 0) (Just $ blockHash block1')
-- Invalid order of votes
badConflictingvoteOrder make = make (max v1 v2) (min v1 v2)
  where
    Just i = indexByValidator valSet (publicKey k4)
    v1     = signValue i k4 $ Vote (Height 1) (Round 0) (Time 0) (Just $ blockHash block1)
    v2     = signValue i k4 $ Vote (Height 1) (Round 0) (Time 0) (Just $ blockHash block1')
-- Conflicting votes are in fact same
badConflictVoteSame make = make v1 v1
  where
    Just i = indexByValidator valSet (publicKey k4)
    v1     = signValue i k4 $ Vote (Height 1) (Round 0) (Time 0) (Just $ blockHash block1)
-- Different round
badConflictVoteDiffR make = make v1 v2
  where
    Just i = indexByValidator valSet (publicKey k4)
    v1     = signValue i k4 $ Vote (Height 1) (Round 0) (Time 0) (Just $ blockHash block1)
    v2     = signValue i k4 $ Vote (Height 1) (Round 1) (Time 0) (Just $ blockHash block1')
-- Different height
badConflictVoteDiffH make = make v1 v2
  where
    Just i = indexByValidator valSet (publicKey k4)
    v1     = signValue i k4 $ Vote (Height 1) (Round 0) (Time 0) (Just $ blockHash block1)
    v2     = signValue i k4 $ Vote (Height 2) (Round 0) (Time 0) (Just $ blockHash block1')
-- Invalid signature
badConflictVoteSign make = make v1 v2
  where
    Just i = indexByValidator valSet (publicKey k4)
    v1     = signValue i k3 $ Vote (Height 1) (Round 0) (Time 0) (Just $ blockHash block1)
    v2     = signValue i k4 $ Vote (Height 1) (Round 0) (Time 0) (Just $ blockHash block1')



conflictingVotesOK
  :: (CryptoHashable (Vote v TestAlg BData))
  => Signed 'Unverified TestAlg (Vote v TestAlg BData)
  -> Signed 'Unverified TestAlg (Vote v TestAlg BData)
  -> Bool
conflictingVotesOK v1 v2
  | byzIdx /= signedKeyInfo v1 || byzIdx /= signedKeyInfo v2
    = error "Wrong validator recorded"
  | isNothing (verifySignature valSet v1) || isNothing (verifySignature valSet v2)
    = error "Bad signature"
  | (voteBlockID . signedValue) v1 == (voteBlockID . signedValue) v2
    = error "Votes are same!"
  | otherwise
    = True
  where
    Just byzIdx = indexByValidator valSet $ publicKey k4


----------------------------------------------------------------
-- Helpers for running tests
----------------------------------------------------------------

type ConsensusM = DBT 'RW TestAlg BData (NoLogsT IO)

run :: Connection 'RW TestAlg BData -> ConsensusM a -> IO a
run c = runNoLogsT . runDBT c

withEnvironment :: ConsensusM x -> IO x
withEnvironment act = withDatabase "" $ \conn -> run conn act

execConsensus
  :: PrivKey TestAlg
  -> Expect ConsensusM TestAlg BData ()
  -> ConsensusM ()
execConsensus k messages = do
  (chans, prop, action) <- startConsensus k
  runConcurrently
    [ action
    , expect valSet prop chans messages
    ]

-- Simple test of consensus
testConsensus :: PrivKey TestAlg -> Expect ConsensusM TestAlg BData () -> IO ()
testConsensus k messages = withEnvironment $ execConsensus k messages

startConsensus
  :: PrivKey TestAlg
  -> ConsensusM ( ( TChan   (MessageTx TestAlg BData)
                  , TBQueue (MessageRx 'Unverified TestAlg BData)
                  )
                , ProposalStorage 'RW ConsensusM TestAlg BData
                , ConsensusM ()
                )
startConsensus k = do
  logic <- mkAppLogic
  chans <- newAppChans cfg
  ch    <- atomicallyIO $ dupTChan $ appChanTx chans
  return ( ( ch
           , appChanRx chans
           )
         , appPropStorage chans
         , runApplication cfg (Just (PrivValidator k)) (genesis,valSet) logic mempty chans
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
                                     return ( BData [("K" ++ let Height h = newBlockHeight b in show h, i)]
                                            , newBlockState b
                                            )
    , appValidationFun  = \b (BlockchainState st valset) -> do
        return $ do st' <- foldM (flip process) st (let BData tx = merkleValue $ blockData b in tx)
                    return $ BlockchainState st' valset
    , appMempool        = nullMempool
    , appBchState       = store
    }

proposerChoice :: Crypto alg => ValidatorSet alg -> Height -> Round -> ValidatorIdx alg
proposerChoice = randomProposerSHA512

proposerKey :: Height -> Round -> PrivKey TestAlg
proposerKey h r = head [ k | k <- privK
                           , publicKey k == validatorPubKey v
                           ]
  where    
    Just v = validatorByIndex valSet $ proposerChoice valSet h r

proposerH1R0,proposerH2R0 :: PrivKey TestAlg
proposerH1R0 = proposerKey (Height 1) (Round 0)
proposerH2R0 = proposerKey (Height 2) (Round 0)

othersH1R0 :: [PrivKey TestAlg]
othersH1R0 = filter (/=proposerH1R0) privK

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

lookupBID :: Monad m => Height -> BlockID alg a -> Expect m alg a (Maybe (Block alg a))
lookupBID h bid = wrap $ LookupBID h bid return

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
                         $ Proposal (blockHeight b) r (Time 0) Nothing bid
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
  | LookupBID  Height (BlockID alg a) (Maybe (Block alg a) -> x)
  deriving (Functor)

-- Read outbound messages from consensus engine and reply to it
-- according to instructions. Unexpeceted messages will result in
-- exception
expect
  :: (MonadIO m, Crypto alg)
  => ValidatorSet alg
  -> ProposalStorage 'RW m alg a
  -> ( TChan (MessageTx alg a)
     , TBQueue (MessageRx 'Unverified alg a)
     )
  -> Expect m alg a ()
  -> m ()
expect vals prop (chTx, chRx) expected = do
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
      LookupBID h bid next ->
        next . blockFromBlockValidation =<< retrievePropByID prop h bid
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
