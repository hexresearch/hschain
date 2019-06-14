{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
module TM.Store ( tests) where

import Test.Tasty
import Test.Tasty.HUnit

import Data.Int         (Int64)
import Data.Monoid      ((<>))
import Data.Traversable (forM)

import qualified Data.Aeson            as JSON
import qualified Data.ByteString.Char8 as BC8

import           Thundermint.Types.Blockchain
import           Thundermint.Run
import           Thundermint.Store
import qualified Thundermint.Mock.KeyVal as KeyVal
import qualified Thundermint.Mock.Coin   as Coin

maxHeight :: Int64
maxHeight = 10

tests :: TestTree
tests = testGroup "generate blockchain and check on consistency"
  [ testGroup "blockhains"
    [ testCase "key-val db" $ runKeyVal (Just maxHeight)  "./test-data/key-val" "./test-spec/simple-stm.json"
    , testCase "Mock coin"  $ runCoin   (Just maxHeight) "./test-spec/keyval-stm.json"
    ]
  ]

-- Run key-val blockchain mock
runKeyVal :: Maybe Int64 -> FilePath -> FilePath -> IO ()
runKeyVal maxH prefix file = do
      -- read config
      blob <- BC8.readFile file
      spec <- case JSON.eitherDecodeStrict blob of
        Right s -> return s
        Left  e -> error e
      storageList <- KeyVal.executeSpec maxH prefix spec
      -- Check result against consistency invariants
      checks <- forM storageList $ \c -> runDBT c checkStorage
      assertEqual "failed consistency check" [] (concat checks)


-- Run coin blockchain mock
runCoin :: Maybe Int64 -> FilePath -> IO ()
runCoin maxH file = do
  -- read config
  blob <- BC8.readFile file
  spec <- case JSON.eitherDecodeStrict blob of
    Right s -> return s
    Left  e -> error e
  storageList <- Coin.executeNodeSpec maxH 300 spec
  -- Check that each blockchain is internally consistent\
  checks <- forM storageList $ \c -> runDBT c checkStorage
  assertEqual "failed consistency check" [] (concat checks)
  -- Check that block and validators identical on each node
  let Just maximumH = maxH
      heights = map (Height . fromIntegral) [0 .. maximumH]
  coins <- forM heights $ \h -> do
    -- blocks
    blocks <- forM storageList $ \c -> do
      mb <- runDBT c $ queryRO $ retrieveBlock h
      case mb of
        Nothing -> error ("Missing block at " <> show h)
        Just b  -> return b
    assertBool ("Block mismatch!" <> show h <> "\n" <> show blocks) (allEqual blocks)
    -- Check that validator set match
    vals <- forM storageList $ \c -> do
      mv <- runDBT c $ queryRO $ retrieveValidatorSet h
      case (h,mv) of
        (Height 0, Nothing) -> return Nothing
        (_       , Just v ) -> return (Just v)
        _                   -> error "Invalid validator!"
    assertBool ("Validators mismatch!" <> show h) (allEqual vals)
    --
    -- FIXME: fix tests
    --
    -- utxos <- forM storageList $ \c ->
    --   runDBT c $ queryRO $ queryUserState h Coin.coinDict $ materializePMap Coin.unspentOutputsLens
    -- let amount = sum . fmap snd <$> utxos
    -- assertBool ("Coin amount mismatch") (allEqual amount)    
  assertBool ("Coin amount changed: " ++ show coins) (allEqual coins)


allEqual :: Eq a => [a] -> Bool
allEqual []     = error "Empty list impossible!"
allEqual (x:xs) = all (x==) xs
