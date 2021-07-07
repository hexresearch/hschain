{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | Tests for gossip. Here we tests gossip FSM in isolation by
--   provisin all events manually and analysing responces
module TM.P2P.Gossip (tests) where

import Control.Arrow (second)
import Control.Concurrent.STM.TVar
import Control.Monad
import Control.Monad.Trans.State.Strict
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Class
import Control.Monad.IO.Class
import Data.Coerce
import Data.List (sort)
import qualified Data.Map.Strict as Map

import HSChain.Blockchain.Internal.Engine.Types
import HSChain.Blockchain.Internal.Types
import HSChain.Control.Util
import HSChain.Crypto
import HSChain.Crypto.Containers
import HSChain.Internal.Types.Messages
import HSChain.Internal.Types.Consensus
import HSChain.P2P
import HSChain.P2P.PeerState.Types
import HSChain.Store
import HSChain.Store.Internal.Proposals
import HSChain.Store.Internal.BlockDB (storeCommit,storeGenesis,storeValSet)
import HSChain.Types
import HSChain.Types.Merkle.Types
import qualified HSChain.Mock.KeyVal as Mock

import qualified HSChain.P2P.PeerState.Types  as P2P
import qualified HSChain.P2P.PeerState.Handle as P2P

import Test.Tasty
import Test.Tasty.HUnit
import TM.Util.MockChain


tests :: TestTree
tests = testGroup "eigen-gossip"
  [ testCase "silent peer"     testGossipUnknown
  , testCase "peer ahead"      testGossipAhead
  , testCase "peer lagging"    testGossipLagging
  , testCase "peer is current (1)" $ testGossipCurrent True
  , testCase "peer is current (2)" $ testGossipCurrent False
  , testCase "POL (1)" $ testGossipPOL True
  , testCase "POL (2)" $ testGossipPOL False
  ]

----------------------------------------------------------------
-- Gossip tests
----------------------------------------------------------------

-- Peer is silent and its state remains unknown
testGossipUnknown :: IO ()
testGossipUnknown = withGossip 3 $ do
  -- Must start from unknown state
  get >>= \case
    Unknown _ -> return ()
    _         -> error "Should start from Unknown state"
  -- Start consensus. We changed state. Peer is silent so it's still unknown
  (Unknown{},cmds) <- step =<< startConsensus
  case cmds of
    [Push2Gossip (GossipAnn (AnnStep (FullStep (Height 4) (Round 0) (StepNewHeight 0))))] -> return ()
    _ -> error "Unexpected message"


-- Peer is ahead of us
testGossipAhead :: IO ()
testGossipAhead = withGossip 3 $ do
  _ <- step =<< startConsensus
  -- (Ahead{},[]) <-
  (Ahead{},[]) <- step $ EGossip $ GossipAnn $ AnnStep $ FullStep (Height 100) (Round 0) (StepNewHeight 0)
  -- We don't have anything to send
  (_,[]) <- step $ ETimeout TimeoutProposal
  (_,[]) <- step $ ETimeout TimeoutPrevote
  (_,[]) <- step $ ETimeout TimeoutPrecommit
  (_,[]) <- step $ ETimeout TimeoutBlock
  return ()

-- Peer is lagging
testGossipLagging :: IO ()
testGossipLagging = withGossip 3 $ do
  _ <- step =<< startConsensus
  -- Peer announce its state
  (Lagging{},[]) <- step $ EGossip $ GossipAnn $ AnnStep $ FullStep (Height 3) (Round 0) (StepNewHeight 0)
  -- We should receive 4 votes
  (_,[Push2Gossip (GossipPreCommit _)]) <- step $ ETimeout TimeoutPrecommit
  (_,[Push2Gossip (GossipPreCommit _)]) <- step $ ETimeout TimeoutPrecommit
  (_,[Push2Gossip (GossipPreCommit _)]) <- step $ ETimeout TimeoutPrecommit
  (_,[Push2Gossip (GossipPreCommit _)]) <- step $ ETimeout TimeoutPrecommit
  (_,[])                                <- step $ ETimeout TimeoutPrecommit
  (_,[])                                <- step $ ETimeout TimeoutProposal
  (_,[])                                <- step $ ETimeout TimeoutPrevote
  -- At this point peer has enough votes to commit block but FSM is
  -- not smart enough to figure it on its own.
  --
  -- NOTE: subject to changes in the future
  (_,[]) <- step $ ETimeout TimeoutBlock
  -- When peer has enough precommits it announces that it has "proposal"
  (_,[]) <- step $ EGossip $ GossipAnn $ AnnHasProposal (Height 3) (Round 0)
  (_,[Push2Gossip (GossipBlock _)]) <- step $ ETimeout TimeoutBlock
  -- Peer commits and advances to the same height as we
  (Current{},[]) <- step $ EGossip $ GossipAnn $ AnnStep $ FullStep (Height 4) (Round 0) (StepNewHeight 0)
  return ()

-- Peer is current
testGossipCurrent :: Bool -> IO ()
testGossipCurrent isRecvProp = withGossip 3 $ do
  _ <- step =<< startConsensus
  (Current{},[]) <- step $ EGossip $ GossipAnn $ AnnStep $ FullStep (Height 4) (Round 0) (StepNewHeight 0)
  -- PROPOSAL
  case isRecvProp of
    -- Receive proposal from peer
    True  -> do (Current{}, [SendRX (RxProposal prop')]) <- step $ EGossip $ GossipProposal prop
                liftIO $ prop @=? prop'
    -- Either proposer or gets proposal from peer
    False -> do addProposal prop
                (Current{}, [Push2Gossip (GossipProposal prop')]) <- step $ ETimeout TimeoutProposal
                liftIO $ prop @=? prop'
  -- PREVOTE.
  --
  -- We add two votes from peer and other two from other sources. We
  -- must gossip ones that didn't receive from peer
  do addPrevote spv1
     addPrevote spv2
     (Current{},[SendRX (RxPreVote _)]) <- step $ EGossip $ GossipPreVote $ signValue i3 k3 vote
     (Current{},[SendRX (RxPreVote _)]) <- step $ EGossip $ GossipPreVote $ signValue i4 k4 vote
     (Current{},[Push2Gossip (GossipPreVote v1)]) <- step $ ETimeout TimeoutPrevote
     (Current{},[Push2Gossip (GossipPreVote v2)]) <- step $ ETimeout TimeoutPrevote
     liftIO $ sort [spv1,spv2] @=? sort [v1,v2]
  -- PRECOMMIT (same as prevote)
  do addPrecommit spc1
     addPrecommit spc2
     (Current{},[SendRX (RxPreCommit _)]) <- step $ EGossip $ GossipPreCommit $ signValue i3 k3 vote
     (Current{},[SendRX (RxPreCommit _)]) <- step $ EGossip $ GossipPreCommit $ signValue i4 k4 vote
     (Current{},[Push2Gossip (GossipPreCommit v1)]) <- step $ ETimeout TimeoutPrecommit 
     (Current{},[Push2Gossip (GossipPreCommit v2)]) <- step $ ETimeout TimeoutPrecommit
     liftIO $ sort [spc1,spc2] @=? sort [v1,v2]
  where
    block = mockchain !! 4
    bid   = blockHash block
    Just [i1,i2,i3,i4] = sequence $ indexByValidator valSet . publicKey <$> privK
    -- Proposals and votes
    prop  = signValue i1 k1 Proposal { propHeight    = Height 4
                                     , propRound     = Round 0
                                     , propTimestamp = Time 0
                                     , propPOL       = Nothing
                                     , propBlockID   = bid
                                     }
    vote = Vote { voteHeight  = Height 4
                , voteRound   = Round 0
                , voteTime    = Time 0
                , voteBlockID = Just bid
                }
    spv1 = signValue i1 k1 vote
    spv2 = signValue i2 k2 vote
    spc1 = signValue i1 k1 vote
    spc2 = signValue i2 k2 vote


testGossipPOL :: Bool -> IO ()
testGossipPOL isLocked = withGossip 3 $ do
  _ <- step =<< startConsensus
  (Current{},[]) <- step $ EGossip $ GossipAnn $ AnnStep $ FullStep (Height 4) (Round 0) (StepNewHeight 0)
  (Current{},[]) <- step $ EGossip $ GossipAnn $ AnnStep $ FullStep (Height 4) (Round 1) StepProposal
  -- Optionally peer announce that it's locked on R=0
  when isLocked $ do
    (Current{},[]) <- step $ EGossip $ GossipAnn $ AnnLock (Just (Round 0))
    return ()
  --
  addPrevote voteR0
  addPrevote voteR1
  -- When peer is locked we first gossip lock round votes
  (Current{},txs) <- step $ ETimeout TimeoutPrevote
  case isLocked of
    True  | [ Push2Gossip (GossipPreVote v)
            ] <- txs
          , v == voteR0
          -> return ()
    False | [ Push2Gossip (GossipPreVote v) ] <- txs
          , v == voteR1
          -> return ()
    _     -> error "Wrong vote set"

  where
    block   = mockchain !! 4
    bid     = blockHash block
    Just i1 = indexByValidator valSet (publicKey k1)
    -- Proposals and votes
    voteR0 = signValue i1 k1 Vote { voteHeight  = Height 4
                                  , voteRound   = Round 0
                                  , voteTime    = Time 0
                                  , voteBlockID = Just bid
                                  }
    voteR1 = signValue i1 k1 Vote { voteHeight  = Height 4
                                  , voteRound   = Round 1
                                  , voteTime    = Time 0
                                  , voteBlockID = Just bid
                                  }



----------------------------------------------------------------
-- Helpers
----------------------------------------------------------------

data Event a = ETX     (MessageTx a)
             | EGossip (GossipMsg a)
             | ETimeout GossipTimeout

type GossipM a = HSChainT a IO

type TestM   view
  = StateT (P2P.State (BlockType view))
    ( ReaderT ( P2P.Config view
              , TVar (Maybe (Height, TMState view)))
      (GossipM (BlockType view)))

-- Start gossip FSM all alone
withGossip :: Int -> TestM Mock.KeyValState a -> IO a
withGossip n action = do
  withDatabase "" $ \conn -> runHSChainT conn $ do
    mustQueryRW $ storeGenesis genesis
    consensusState <- liftIO $ newTVarIO Nothing
    let config = P2P.Config (readTVar consensusState)
    seedDatabase n
    flip runReaderT (config, consensusState)
      $ flip evalStateT (P2P.wrap P2P.UnknownState)
      $ action


-- Seed database with given number of blocks
seedDatabase :: Int -> GossipM Mock.BData ()
seedDatabase n = do
  mustQueryRW $ forM_ blockAndCmt $ \(b,Just cmt) -> do
    storeCommit cmt b
    storeValSet (succ $ blockHeight b) (merkled valSet)
  where
    blockAndCmt = take n
                $ tail
                $ mockchain `zip` (fmap merkleValue . blockPrevCommit <$> tail mockchain)

-- Start "consensus engine"
startConsensus :: TestM Mock.KeyValState (Event Mock.BData)
startConsensus = do
  h     <- queryRO blockchainHeight
  varSt <- lift $ asks snd
  atomicallyIO $ writeTVar varSt (Just (succ h, TMState
    { smRound          = Round 0
    , smStep           = StepNewHeight 0
    , smProposals      = mempty
    , smProposedBlocks = emptyProps
    , smPrevotesSet    = newHeightVoteSet valSet
    , smPrecommitsSet  = newHeightVoteSet valSet
    , smLockedBlock    = Nothing
    , smLastCommit     = Nothing
    }))
  return $ ETX $ TxAnn $ AnnStep $ FullStep (succ h) (Round 0) (StepNewHeight 0)

addProposal
  :: Signed 'Unverified (Alg Mock.BData) (Proposal Mock.BData)
  -> TestM Mock.KeyValState ()
addProposal p = do
  varSt <- lift $ asks snd
  atomicallyIO $ modifyTVar' varSt $ fmap $ second $ \tm -> tm
    { smProposals = Map.insert (propRound (signedValue p)) (coerce p) (smProposals tm)
    }

addPrevote
  :: Signed 'Unverified (Alg Mock.BData) (Vote 'PreVote Mock.BData)
  -> TestM Mock.KeyValState ()
addPrevote v = do
  varSt <- lift $ asks snd
  atomicallyIO $ modifyTVar' varSt $ fmap $ second $ \tm -> tm
    { smPrevotesSet =
        case addSignedValue (voteRound (signedValue v)) (coerce v) (smPrevotesSet tm) of
          InsertOK votes   -> votes
          InsertDup        -> error "InsertDup"
          InsertConflict _ -> error "InsertConflict"
          InsertUnknown    -> error "InsertUnknown"
    }

addPrecommit
  :: Signed 'Unverified (Alg Mock.BData) (Vote 'PreCommit Mock.BData)
  -> TestM Mock.KeyValState ()
addPrecommit v = do
  varSt <- lift $ asks snd
  atomicallyIO $ modifyTVar' varSt $ fmap $ second $ \tm -> tm
    { smPrecommitsSet =
        case addSignedValue (voteRound (signedValue v)) (coerce v) (smPrecommitsSet tm) of
          InsertOK votes   -> votes
          InsertDup        -> error "InsertDup"
          InsertConflict _ -> error "InsertConflict"
          InsertUnknown    -> error "InsertUnknown"
    }

-- Perform single step
step :: (BlockData (BlockType view))
     => Event (BlockType view) -> TestM view ( P2P.State (BlockType view)
                                             , [Command (BlockType view)])
step e = do
  cfg       <- lift $ asks fst
  st        <- get
  (st',cmd) <- lift $ lift $ case e of
    ETX      m -> P2P.handlerTx      cfg st m
    EGossip  m -> P2P.handlerGossip  cfg st m
    ETimeout m -> P2P.handlerTimeout cfg st m
  put st'
  return (st',cmd)
