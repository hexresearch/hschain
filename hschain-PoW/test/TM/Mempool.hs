-- |
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE NumDecimals      #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE TypeApplications #-}
module TM.Mempool (tests) where

import Control.Concurrent.STM
import Control.Monad.State.Strict
import Control.Monad.Except
import Control.Monad.Trans.Cont
import Data.Foldable
import qualified Data.Map.Strict as Map
import qualified Data.Set        as Set
import Lens.Micro
import System.Timeout
import Test.Tasty
import Test.Tasty.HUnit

import HSChain.Control.Channels
import HSChain.Crypto
import HSChain.Crypto.SHA
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
  db       <- lift $ inMemoryDB genesis
  bIdx     <- lift $ buildBlockIndex db
  c0       <- lift $ createConsensus db bIdx sView
  (pow,ch) <- startNodeTest netcfg net [] db c0
  let MempoolAPI{..} = mempoolAPI pow
  -- First post transaction into mempool  
  sinkIO postTransaction tx1
  liftIO $ timeout 1e6 $ atomically $ do
    mempoolContent >>= \case
      [] -> retry
      txs | txs == [tx1] -> return txs
          | otherwise    -> error "Invalid mempool content"    
  --
  return ()
  where
    tx1 = (1,"TX1")
    netcfg = NetCfg { nKnownPeers     = 3
                    , nConnectedPeers = 3
                    }
