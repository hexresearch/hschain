{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE RankNTypes         #-}
-- |
module TM.Store (tests) where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Catch
import Control.Monad.Trans.Class
import Control.Monad.Trans.Cont
import Test.Tasty
import Test.Tasty.HUnit
import Lens.Micro

import HSChain.Control.Class
import HSChain.Control.Util
import HSChain.Control.Channels
import HSChain.PoW.Consensus
import HSChain.PoW.Types
import HSChain.PoW.Node
import HSChain.PoW.P2P
import HSChain.PoW.P2P.Types
import HSChain.Types.Merkle.Types
import HSChain.Examples.Simple (KV)
import HSChain.Examples.Coin   (miningLoop,Coin(..))
import HSChain.Network.Mock
import HSChain.Network.Types
import TM.Util.Mockchain

tests :: TestTree
tests = testGroup "Block store"
  [ testCase "Store:In-memory" $ testIdempotence =<< inMemoryDB genesis
  , testCase "Store:DB" $ withHSChainT $ testIdempotence =<< blockDatabase genesis
  , testCase "Restart" $ withHSChainT testRestart
  ]


-- | Test that storage works correctly
testIdempotence :: MonadIO m => BlockDB m (KV MockChain) -> m ()
testIdempotence db = do
  -- Genesis
  fetch "Genesis" genesis
  storeBlock db genesis
  fetch "Genesis" genesis
  -- Rest of blocks
  forM_ (take 3 $ drop 1 mockchain) $ \b -> do
    nofetch (show (blockHeight b)) b
  -- Store and retrieve
  forM_ (take 3 $ drop 1 mockchain) $ \b -> do
    storeBlock db b
    fetch ("Stored: "++show (blockHeight b)) b
  -- Idempotency of store
  forM_ (take 3 $ drop 1 mockchain) $ \b -> do
    storeBlock db b
    fetch ("Idempotence: "++show (blockHeight b)) b
  -- Fetchall
  liftIO . assertEqual "All headers" (take 4 $ map toHeader $ mockchain) =<< retrieveAllHeaders db
  where
    fetch nm b = do
      liftIO . assertEqual ("Yes: " ++ nm) (Just b)            =<< retrieveBlock  db (blockID b)
      liftIO . assertEqual ("Yes: " ++ nm) (Just (toHeader b)) =<< retrieveHeader db (blockID b)
    nofetch nm b = do
      liftIO . assertEqual ("No: " ++ nm) (Nothing) =<< retrieveBlock  db (blockID b)
      liftIO . assertEqual ("No: " ++ nm) (Nothing) =<< retrieveHeader db (blockID b)


-- | Test that we're able to restart
testRestart :: HSChainT IO ()
testRestart = do
  mocknet <- liftIO newMockNet
  -- First start of exception
  h <- catchAbort $ evalContT $ do
    let net   = createMockNode mocknet $ NetAddrV4 1 1000
    let sView = inMemoryView (blockID genesisCoin)
    db   <- lift $ blockDatabase genesisCoin
    bIdx <- lift $ buildBlockIndex db
    c0   <- lift $ createConsensus db bIdx sView
    pow  <- startNode netcfg net [] db c0
    cforkLinked $ miningLoop pow True
    -- Await for new blocks
    ch   <- atomicallyIO $ chainUpdate pow
    lift $ forever $ do
      (BH{bhHeight=h},_) <- awaitIO ch
      when (h >= Height 10) $ throwM (Abort h)
  -- Reinitialize
  do let sView = inMemoryView (blockID genesisCoin)
     db   <- blockDatabase genesisCoin
     bIdx <- buildBlockIndex db
     c0   <- createConsensus db bIdx sView
     liftIO $ h @=? (c0 ^. bestHead . _1 . to bhHeight)
  where
    netcfg = NetCfg { nKnownPeers     = 3
                    , nConnectedPeers = 3
                    }


data Abort = Abort Height
  deriving stock    (Show)
  deriving anyclass (Exception)

catchAbort :: MonadCatch m => (forall a. m a) -> m Height
catchAbort action = handle (\(Abort h) -> return h) action


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
