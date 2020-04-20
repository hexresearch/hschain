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
import Data.Monoid      ((<>))
import Data.Foldable    (toList)
import Data.Traversable (forM)

import HSChain.Blockchain.Internal.Engine.Types
import           HSChain.Control
import           HSChain.Types.Blockchain
import           HSChain.Store
import qualified HSChain.Mock.KeyVal as KeyVal
import qualified HSChain.Mock.Coin   as Coin
import           HSChain.Mock.Types
import           HSChain.Types.Merkle.Types
import           TM.Util.Network (withTimeOut)


tests :: TestTree
tests = testGroup "generate blockchain and check on consistency"
  [ testGroup "blockhains"
    [ localOption (1 :: NumThreads) $ testCase "key-val db" $ withTimeOut 60e6 runKeyVal
    , localOption (1 :: NumThreads) $ testCase "Mock coin"  $ withTimeOut 60e6 runCoin
    ]
  ]

-- Run key-val blockchain mock
runKeyVal :: IO ()
runKeyVal  = evalContT $ do
    -- Run blockchain
    rnodes <- KeyVal.executeSpec spec
    -- Check that each blockchain is internally consistent
    checks <- forM rnodes $ \n -> runDBT (Coin.rnodeConn n) checkStorage
    liftIO $ assertEqual "Failed consistency check" [] (concat checks)
  where
    spec = NetSpec
      { netNodeList =
          [ NodeSpec
            { nspecPrivKey = Just $ PrivValidator $ read "\"2K7bFuJXxKf5LqogvVRQjms2W26ZrjpvUjo5LdvPFa5Y\""
            , nspecDbName = Nothing
            , nspecLogFile = []
            }
          , NodeSpec
            { nspecPrivKey = Just $ PrivValidator $ read "\"4NSWtMsEPgfTK25tCPWqNzVVze1dgMwcUFwS5WkSpjJL\""
            , nspecDbName = Nothing
            , nspecLogFile = []
            }
          , NodeSpec
            { nspecPrivKey = Just $ PrivValidator $ read "\"3Fj8bZjKc53F2a87sQaFkrDas2d9gjzK57FmQwnNnSHS\""
            , nspecDbName = Nothing
            , nspecLogFile = []
            }
          , NodeSpec
            { nspecPrivKey = Just $ PrivValidator $ read "\"D2fpHM1JA8trshiUW8XPvspsapUvPqVzSofaK1MGRySd\""
            , nspecDbName = Nothing
            , nspecLogFile = []
            }
          ]
      , netTopology = All2All
      , netNetCfg   =
        let c = defCfg
        in  c { cfgConsensus = ConsensusCfg
                { timeoutNewHeight  = 10
                , timeoutProposal   = (50,50)
                , timeoutPrevote    = (50,50)
                , timeoutPrecommit  = (50,50)
                , timeoutEmptyBlock = 100
                , incomingQueueSize = 10
                }
              } `asTypeOf` c        
      , netMaxH     = Just (Height 10)
      }


-- Run coin blockchain mock
runCoin :: IO ()
runCoin = evalContT $ do
    rnodes <- Coin.executeNodeSpec
            $  spec
           :*: coin { coinGeneratorDelay = Just 200 }
    -- Check that each blockchain is internally consistent
    checks <- forM rnodes $ \n -> runDBT (Coin.rnodeConn n) checkStorage
    liftIO $ assertEqual "Failed consistency check" [] (concat checks)
    -- Check that block and validators identical on each node
    maxH <- fmap minimum
          $ forM rnodes $ \n ->
            runDBT (Coin.rnodeConn n) $ queryRO $ blockchainHeight
    forM_ [Height 0 .. maxH] $ \h -> do
      -- Blocks match
      blocks <- forM rnodes $ \n -> do
        mb <- lift $ runDBT (Coin.rnodeConn n) $ queryRO $ retrieveBlock h
        case mb of
          Nothing -> error ("Missing block at " <> show h)
          Just b  -> return b
      liftIO $ assertBool ("Block mismatch!" <> show h <> "\n" <> show blocks) (allEqual blocks)
      -- Check that validator set match
      vals <- forM rnodes $ \n -> do
        mv <- lift $ runDBT (Coin.rnodeConn n) $ queryRO $ retrieveValidatorSet h
        case (h,mv) of
          (Height 0, Nothing) -> return Nothing
          (_       , Just v ) -> return (Just v)
          _                   -> error "Invalid validator!"
      liftIO $ assertBool ("Validators mismatch!" <> show h) (allEqual vals)
    -- Check that amount of coins didn't change
    forM_ rnodes $ \n -> liftIO $ do
      let totalCoins = coinAridrop coin * fromIntegral (coinWallets coin)
      (_, merkleValue -> Coin.CoinState utxos _) <- bchCurrentState $ Coin.rnodeState n
      assertEqual "Coins must be preserved" totalCoins (sum [ c | Coin.Unspent _ c <- toList utxos])
  where
    coin = CoinSpecification
      { coinAridrop        = 1000
      , coinWallets        = 1000
      , coinWalletsSeed    = 1337
      , coinGeneratorDelay = Just 100
      , coinMaxMempoolSize = 1000
      }
    spec = NetSpec
      { netNodeList =
        [ NodeSpec
          { nspecPrivKey = Just $ PrivValidator $ read "\"2K7bFuJXxKf5LqogvVRQjms2W26ZrjpvUjo5LdvPFa5Y\""
          , nspecDbName = Nothing
          , nspecLogFile = []
          }
        , NodeSpec
          { nspecPrivKey = Just $ PrivValidator $ read "\"4NSWtMsEPgfTK25tCPWqNzVVze1dgMwcUFwS5WkSpjJL\""
          , nspecDbName = Nothing
          , nspecLogFile = []}
        , NodeSpec
          { nspecPrivKey = Just $ PrivValidator $ read "\"3Fj8bZjKc53F2a87sQaFkrDas2d9gjzK57FmQwnNnSHS\""
          , nspecDbName = Nothing
          , nspecLogFile = []
          }
        , NodeSpec
          { nspecPrivKey = Just $ PrivValidator $ read "\"D2fpHM1JA8trshiUW8XPvspsapUvPqVzSofaK1MGRySd\""
          , nspecDbName = Nothing
          , nspecLogFile = []}
        ]
      , netTopology = All2All
      , netNetCfg =
        let c = defCfg
        in  c { cfgConsensus = ConsensusCfg
                { timeoutNewHeight  = 10
                , timeoutProposal   = (50,50)
                , timeoutPrevote    = (50,50)
                , timeoutPrecommit  = (50,50)
                , timeoutEmptyBlock = 100
                , incomingQueueSize = 10
                }
              } `asTypeOf` c      
      , netMaxH = Just (Height 10)
      }

allEqual :: Eq a => [a] -> Bool
allEqual []     = error "Empty list impossible!"
allEqual (x:xs) = all (x==) xs
