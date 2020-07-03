{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE NumDecimals         #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns        #-}
module TM.Integration ( tests) where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Runners

import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Cont
import Control.Monad.IO.Class
import Data.Default.Class

import HSChain.Blockchain.Internal.Engine.Types
import HSChain.Internal.Types.Config
import           HSChain.Types.Blockchain
import           HSChain.Store
import qualified HSChain.Mock.KeyVal as KeyVal
import qualified HSChain.Mock.Coin   as Coin
import           HSChain.Mock.Coin     (CoinSpecification(..))
import           HSChain.Mock.Types
import           TM.Util.Network (withTimeOut)


tests :: TestTree
tests = testGroup "generate blockchain and check on consistency"
  [ testGroup "blockhains"
    [ localOption (1 :: NumThreads) $ testCase "key-val db" $ withTimeOut 20e6 runKeyVal
    , localOption (1 :: NumThreads) $ testCase "Mock coin"  $ withTimeOut 20e6 runCoin
    ]
  ]

-- Run key-val blockchain mock
runKeyVal :: IO ()
runKeyVal  = evalContT $ do
    -- Run blockchain
    rnodes <- KeyVal.executeSpec spec (callbackAbortAtH (Height 10))
    -- Check that each blockchain is internally consistent
    checks <- forM rnodes $ \(_,d) -> KeyVal.runKeyValT d checkStorage
    liftIO $ assertEqual "Failed consistency check" [] (concat checks)
  where
    spec = MockClusterConfig
      { clusterTopology  = All2All
      , clusterCfg       =
          let c = def
          in  c { cfgConsensus = ConsensusCfg
                  { timeoutNewHeight  = 10
                  , timeoutProposal   = (50,50)
                  , timeoutPrevote    = (50,50)
                  , timeoutPrecommit  = (50,50)
                  , timeoutEmptyBlock = 100
                  , incomingQueueSize = 10
                  }
                } `asTypeOf` c
      , clusterNodes =
          let mk k = NodeSpec { nspecPrivKey     = Just (PrivValidator k)
                              , nspecDbName      = Nothing
                              , nspecLogFile     = []
                              }
          in mk <$> [ read "\"2K7bFuJXxKf5LqogvVRQjms2W26ZrjpvUjo5LdvPFa5Y\""
                    , read "\"4NSWtMsEPgfTK25tCPWqNzVVze1dgMwcUFwS5WkSpjJL\""
                    , read "\"3Fj8bZjKc53F2a87sQaFkrDas2d9gjzK57FmQwnNnSHS\""
                    , read "\"D2fpHM1JA8trshiUW8XPvspsapUvPqVzSofaK1MGRySd\""
                    ]
      , clusterBChData = ()
      }


-- Run coin blockchain mock
runCoin :: IO ()
runCoin = evalContT $ do
    rnodes <- Coin.executeNodeSpec
                spec
                (callbackAbortAtH (Height 10))
    -- Check that each blockchain is internally consistent
    checks <- forM rnodes $ \(_,d) -> Coin.runCoinT d checkStorage
    liftIO $ assertEqual "Failed consistency check" [] (concat checks)
    -- Check that block and validators identical on each node
    maxH <- fmap minimum
          $ forM rnodes $ \(_,d) ->
            Coin.runCoinT d $ queryRO $ blockchainHeight
    forM_ [Height 0 .. maxH] $ \h -> do
      -- Blocks match
      blocks <- forM rnodes $ \(_,d) -> do
        mb <- lift $ Coin.runCoinT d $ queryRO $ retrieveBlock h
        case mb of
          Nothing -> error ("Missing block at " <> show h)
          Just b  -> return b
      liftIO $ assertBool ("Block mismatch!" <> show h <> "\n" <> show blocks) (allEqual blocks)
      -- Check that validator set match
      vals <- forM rnodes $ \(_,d) -> do
        mv <- lift $ Coin.runCoinT d $ queryRO $ retrieveValidatorSet h
        case (h,mv) of
          (Height 0, Nothing) -> return Nothing
          (_       , Just v ) -> return (Just v)
          _                   -> error "Invalid validator!"
      liftIO $ assertBool ("Validators mismatch!" <> show h) (allEqual vals)
    -- Check that amount of coins didn't change
    -- FIXME: we need access to state!
    -- forM_ rnodes $ \n -> liftIO $ do
    --   let totalCoins = coinAirdrop coin * fromIntegral (coinWallets coin)
    --   (_, merkleValue -> Coin.CoinState utxos _) <- bchCurrentState $ Coin.rnodeState n
    --   assertEqual "Coins must be preserved" totalCoins (sum [ c | Coin.Unspent _ c <- toList utxos])
  where
    coin = CoinSpecification
      { coinAirdrop        = 1000
      , coinWallets        = 1000
      , coinWalletsSeed    = 1337
      , coinGeneratorDelay = Just 200
      , coinMaxMempoolSize = 1000
      }
    spec = MockClusterConfig
      { clusterTopology  = All2All
      , clusterCfg =
          let c = def
          in  c { cfgConsensus = ConsensusCfg
                  { timeoutNewHeight  = 10
                  , timeoutProposal   = (50,250)
                  , timeoutPrevote    = (50,250)
                  , timeoutPrecommit  = (50,250)
                  , timeoutEmptyBlock = 100
                  , incomingQueueSize = 10
                  }
                } `asTypeOf` c
      , clusterNodes =
          let mk k = NodeSpec { nspecPrivKey     = Just (PrivValidator k)
                              , nspecDbName      = Nothing
                              , nspecLogFile     = []
                              }
          in mk <$> [ read "\"2K7bFuJXxKf5LqogvVRQjms2W26ZrjpvUjo5LdvPFa5Y\""
                    , read "\"4NSWtMsEPgfTK25tCPWqNzVVze1dgMwcUFwS5WkSpjJL\""
                    , read "\"3Fj8bZjKc53F2a87sQaFkrDas2d9gjzK57FmQwnNnSHS\""
                    , read "\"D2fpHM1JA8trshiUW8XPvspsapUvPqVzSofaK1MGRySd\""
                    ]
      , clusterBChData = coin
      }


allEqual :: Eq a => [a] -> Bool
allEqual []     = error "Empty list impossible!"
allEqual (x:xs) = all (x==) xs
