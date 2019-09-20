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

import Control.Concurrent.STM.TVar
import Control.Monad
import Control.Monad.Trans.State.Strict
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Class
import Control.Monad.IO.Class

import HSChain.Blockchain.Internal.Engine.Types
import HSChain.Blockchain.Internal.Types
import HSChain.Control
import HSChain.Crypto
import HSChain.Logger
import HSChain.P2P
import HSChain.P2P.PeerState.Types
import HSChain.Run
import HSChain.Store
import HSChain.Store.Internal.BlockDB (storeCommit)
import HSChain.Store.STM
import HSChain.Types
import qualified HSChain.Mock.KeyVal as Mock

import qualified HSChain.P2P.PeerState.Types as P2P
import qualified HSChain.P2P.PeerState.Handle as P2P (handler)

import Test.Tasty
import Test.Tasty.HUnit

import TM.Util.Network
import TM.Util.MockChain

tests :: TestTree
tests = testGroup "eigen-gossip"
  [ testCase "silent peer"     testGossipUnknown
  , testCase "peer ahead"      testGossipAhead
  , testCase "peer lagging"    testGossipLagging
  , testCase "peer is current" testGossipCurrent
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
    _                     -> () <$ error "Should start from Unknown state"
  -- Start consensus. We changed state. Peer is silent so it's still unknown
  (_,cmds) <- step =<< startConsensus
  get >>= \case
    WrapState (Unknown _) -> return ()
    _                     -> () <$ error "Should start from Unknown state"
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
