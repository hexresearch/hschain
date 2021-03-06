{-# LANGUAGE TypeFamilies #-}
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
import HSChain.PoW.Tests
import HSChain.Types.Merkle.Types
import HSChain.Examples.Coin   (Coin(..),coinStateView)
import HSChain.Network.Mock
import HSChain.Network.Types
import TM.Util.Mockchain

tests :: TestTree
tests = testGroup "Block store"
  [ testCase "Store:In-memory" $ testIdempotence mockchain =<< inMemoryDB genesis
  , testCase "Store:DB"   $ withHSChainT $ testIdempotence mockchain =<< blockDatabase genesis
  , testCase "Store:Coin" $ withHSChainT $ do (db,_,_) <- coinStateView $ head emptyCoinChain
                                              testIdempotence emptyCoinChain db
  , testCase "Restart" $ testTimeout 5 $ withHSChainT testRestart
  ]


-- | Test that we're able to restart and to build correct block index
testRestart :: HSChainT IO ()
testRestart = do
  mocknet <- liftIO newMockNet
  -- First start of exception
  h <- catchAbort $ evalContT $ do
    let net = createMockNode mocknet $ NetAddrV4 1 1000
        mine st t _ = return $ createCandidateBlock bh t $ Coin
          { coinData   = merkled []
          , coinTarget = retarget bh
          , coinNonce  = 0
          }
          where bh = stateBH st
    db   <- lift $ blockDatabase genesisCoin
    bIdx <- lift $ buildBlockIndex db
    let Just bh = lookupIdx (blockID genesisCoin) bIdx
    c0   <- lift $ createConsensus db (startingState bh) bIdx
    pow  <- startNode netcfg net db c0
    cforkLinked $ genericMiningLoop mine pow
    -- Await for new blocks
    ch   <- atomicallyIO $ chainUpdate pow
    lift $ forever $ do
      h <- bhHeight . stateBH <$> awaitIO ch
      when (h >= Height 10) $ throwM (Abort h)
  -- Reinitialize
  do db   <- blockDatabase genesisCoin
     bIdx <- buildBlockIndex db
     let Just bh = lookupIdx (blockID genesisCoin) bIdx
     c0 <- createConsensus db (startingState bh) bIdx
     liftIO $ h @=? (c0 ^. bestHead . _1 . to stateBH . to bhHeight)
  where
    startingState :: BH Coin -> DummyState (HSChainT IO) Coin
    startingState bh = DummyState bh (error "No rewind past genesis")
    netcfg = NodeCfg { nKnownPeers     = 3
                     , nConnectedPeers = 3
                     , initialPeers    = []
                     }


genesisCoin :: Block Coin
genesisCoin = Block
  { blockHeight = Height 0
  , blockTime   = Time 0
  , prevBlock   = Nothing
  , blockData   = Coin { coinData   = merkled []
                       , coinNonce  = 0
                       , coinTarget = Target $ 2^(256-4 :: Int)
                       }
  }
