{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
module TM.Store ( tests) where

import Test.Tasty
import Test.Tasty.HUnit

import Control.Applicative
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Cont
import Control.Monad.IO.Class
import Data.Monoid      ((<>))
import Data.Traversable (forM)

import qualified Data.Aeson            as JSON
import qualified Data.ByteString.Char8 as BC8

import           Thundermint.Blockchain.Interpretation
import           Thundermint.Control
import           Thundermint.Types.Blockchain
import           Thundermint.Run
import           Thundermint.Store
import qualified Thundermint.Mock.KeyVal as KeyVal
import qualified Thundermint.Mock.Coin   as Coin
import           Thundermint.Mock.Types

maxHeight :: Height
maxHeight = Height 10

tests :: TestTree
tests = testGroup "generate blockchain and check on consistency"
  [ testGroup "blockhains"
    [ testCase "key-val db" $ runKeyVal (Just maxHeight) "./test-spec/simple-stm.json"
    , testCase "Mock coin"  $ runCoin   maxHeight "./test-spec/keyval-stm.json"
    ]
  ]

-- Run key-val blockchain mock
runKeyVal :: Maybe Height -> FilePath -> IO ()
runKeyVal maxH file = do
  -- read config
  blob <- BC8.readFile file
  spec <- case JSON.eitherDecodeStrict blob of
    Right s -> return s
    Left  e -> error e
  --
  evalContT $ do
    -- Run blockchain
    rnodes <- KeyVal.executeSpec spec
    -- Check that each blockchain is internally consistent
    checks <- forM rnodes $ \n -> runDBT (Coin.rnodeConn n) checkStorage
    liftIO $ assertEqual "Failed consistency check" [] (concat checks)


-- Run coin blockchain mock
runCoin :: Height -> FilePath -> IO ()
runCoin maxH file = do
  -- read config
  blob <- BC8.readFile file
  spec :*: coin <- case JSON.eitherDecodeStrict blob of
    Right s -> return s
    Left  e -> error e
  -- Run mock cluster
  evalContT $ do
    rnodes <- Coin.executeNodeSpec
            $  spec { netMaxH            = netMaxH spec <|> Just maxH }
           :*: coin { coinGeneratorDelay = Just 200 }
    -- Check that each blockchain is internally consistent
    checks <- forM rnodes $ \n -> runDBT (Coin.rnodeConn n) checkStorage
    liftIO $ assertEqual "Failed consistency check" [] (concat checks)
    -- Check that block and validators identical on each node
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
      Coin.CoinState utxos <- currentState $ Coin.rnodeState n
      assertEqual "Coins must be preserved" totalCoins (sum $ snd <$> utxos)

allEqual :: Eq a => [a] -> Bool
allEqual []     = error "Empty list impossible!"
allEqual (x:xs) = all (x==) xs
