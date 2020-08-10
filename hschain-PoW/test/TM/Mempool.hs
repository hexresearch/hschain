-- |
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE NumDecimals      #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE TypeApplications #-}
module TM.Mempool (tests) where

import Control.Concurrent.STM
import Control.Monad.State.Strict
import Control.Monad.Trans.Cont
import System.Timeout
import Test.Tasty
import Test.Tasty.HUnit

import HSChain.Control.Channels
import HSChain.Examples.Simple
import HSChain.Logger
import HSChain.Network.Mock
import HSChain.Network.Types
import HSChain.PoW.Consensus
import HSChain.PoW.Node
import HSChain.PoW.Mempool
import HSChain.PoW.P2P
import HSChain.PoW.P2P.Types
import HSChain.PoW.Types
import HSChain.Types.Merkle.Types
import TM.Util.Mockchain


tests :: TestTree
tests = testGroup "PoW consensus"
  [ testCase "Mempool rollbacks" testMempoolRollback
  ]

-- | Test that we can add transaction to mempool and its content is altered correctly
testMempoolRollback :: IO ()
testMempoolRollback = runNoLogsT $ evalContT $ do
  mocknet <- liftIO newMockNet
  let net   = createMockNode mocknet $ NetAddrV4 1 1000
      sView = kvMemoryView (blockID genesis)
  db <- lift $ inMemoryDB genesis
  c0 <- lift $ createConsensus db sView
  (pow,sinkBOX) <- startNodeTest netcfg net [] db c0
  let api@MempoolAPI{..} = mempoolAPI pow
  -- First post transaction into mempool
  sinkIO postTransaction tx1
  checkMempoolContent api [tx1]
  -- Mine first block & check that we removed transaction
  sendBlock sinkBOX b1
  checkMempoolContent api []
  -- Generate reorganization
  sendBlock sinkBOX b1'
  sendBlock sinkBOX b2'
  checkMempoolContent api [tx2]
  where
    tx1 = (1,"TX1")
    tx2 = (2,"TX2")
    tx3 = (3,"TX3")
    --
    b1  = mineBlock [tx1] genesis
    b1' = mineBlock [tx2] genesis
    b2' = mineBlock [tx3] b1'
    --
    netcfg = NetCfg { nKnownPeers     = 3
                    , nConnectedPeers = 3
                    }


checkMempoolContent
  :: (MonadIO n, Eq (Tx b), Show (Tx b))
  => MempoolAPI m b -> [Tx b] -> n ()
checkMempoolContent MempoolAPI{..} expected = liftIO $ do
  r <- timeout 1e6 $ atomically $ do
    txs <- mempoolContent
    unless (expected == txs) retry
  case r of
    Just () -> return ()
    Nothing -> do txs <- atomically mempoolContent
                  expected @=? txs

sendBlock :: (MerkleMap b, Monad m, MonadIO n) => Sink (BoxRX m b) -> GBlock b Identity -> n ()
sendBlock sinkBOX b = liftIO $ do
  sinkIO sinkBOX $ BoxRX $ \cnt -> cnt (RxHeaders [toHeader b]) >>= \case
    Peer'Noop         -> return ()
    Peer'EnterCatchup -> error "Shouldn't enter catchup"
    Peer'Punish e     -> error $ show e
  sinkIO sinkBOX $ BoxRX $ \cnt -> cnt (RxBlock b) >>= \case
    Peer'Noop         -> return ()
    Peer'EnterCatchup -> error "Shouldn't enter catchup"
    Peer'Punish e     -> error $ show e
