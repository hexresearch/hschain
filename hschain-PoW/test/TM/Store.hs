{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE RankNTypes         #-}
-- |
module TM.Store (tests) where

import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Catch
import Control.Monad.Trans.Class
import Control.Monad.Trans.Cont
import Test.Tasty
import Test.Tasty.HUnit

import HSChain.Control.Class
import HSChain.Control.Util
import HSChain.Control.Channels
import HSChain.PoW.Consensus
import HSChain.PoW.Types
import HSChain.PoW.Node
import HSChain.PoW.P2P
import HSChain.PoW.P2P.Types
import HSChain.Types.Merkle.Types
import HSChain.Examples.Coin   (miningLoop,Coin(..),coinStateView)
import HSChain.Network.Mock
import HSChain.Network.Types
import TM.Util.Mockchain

tests :: TestTree
tests = testGroup "Block store"
  [ testCase "Store:In-memory" $ testIdempotence mockchain =<< inMemoryDB genesis
  , testCase "Store:DB"   $ withHSChainT $ testIdempotence mockchain =<< blockDatabase genesis
  , testCase "Store:Coin" $ withHSChainT $ do (db,_) <- coinStateView $ head emptyCoinChain
                                              testIdempotence emptyCoinChain db
  , testCase "Restart" $ withHSChainT testRestart
  ]


-- | Test that storage works correctly
testIdempotence
  :: (MonadIO m, BlockData b, Eq (b Proxy), Eq (b Identity), Show (b Proxy), Show (b Identity))
  => [Block b] -> BlockDB m b -> m ()
testIdempotence chain db = do
  -- Genesis
  fetch "Genesis" (head chain)
  storeBlock db   (head chain)
  fetch "Genesis" (head chain)
  -- Rest of blocks
  forM_ (take 3 $ drop 1 chain) $ \b -> do
    nofetch (show (blockHeight b)) b
  -- Store and retrieve
  forM_ (take 3 $ drop 1 chain) $ \b -> do
    storeBlock db b
    fetch ("Stored: "++show (blockHeight b)) b
  -- Idempotency of store
  forM_ (take 3 $ drop 1 chain) $ \b -> do
    storeBlock db b
    fetch ("Idempotence: "++show (blockHeight b)) b
  -- Fetchall
  liftIO . assertEqual "All headers" (take 4 $ map toHeader $ chain) =<< retrieveAllHeaders db
  where
    fetch nm b = do
      liftIO . assertEqual ("Yes: " ++ nm) (Just b)            =<< retrieveBlock  db (blockID b)
      liftIO . assertEqual ("Yes: " ++ nm) (Just (toHeader b)) =<< retrieveHeader db (blockID b)
    nofetch nm b = do
      liftIO . assertEqual ("No: " ++ nm) (Nothing) =<< retrieveBlock  db (blockID b)
      liftIO . assertEqual ("No: " ++ nm) (Nothing) =<< retrieveHeader db (blockID b)


-- | Test that we're able to restart and to build correct block index
testRestart :: HSChainT IO ()
testRestart = do
  mocknet <- liftIO newMockNet
  -- First start of exception
  h <- catchAbort $ evalContT $ do
    let net   = createMockNode mocknet $ NetAddrV4 1 1000
    let sView = inMemoryView (blockID genesisCoin)
    db   <- lift $ blockDatabase genesisCoin
    c0   <- lift $ createConsensus db sView =<< buildBlockIndex db
    pow  <- startNode netcfg net [] db c0
    cforkLinked $ miningLoop pow True
    -- Await for new blocks
    ch   <- atomicallyIO $ chainUpdate pow
    lift $ forever $ do
      (BH{bhHeight=h},_) <- awaitIO ch
      when (h >= Height 10) $ throwM (Abort h)
  -- Reinitialize
  do let sView = inMemoryView (blockID genesisCoin)
     db <- blockDatabase genesisCoin
     c0 <- createConsensus db sView =<< buildBlockIndex db
     liftIO $ h @=? (c0 ^. bestHead . _1 . to bhHeight)
  where
    netcfg = NetCfg { nKnownPeers     = 3
                    , nConnectedPeers = 3
                    }


genesisCoin :: Block Coin
genesisCoin = GBlock
  { blockHeight = Height 0
  , blockTime   = Time 0
  , prevBlock   = Nothing
  , blockData   = Coin { coinData   = merkled []
                       , coinNonce  = 0
                       , coinTarget = Target $ 2^(256-4 :: Int)
                       }
  }
