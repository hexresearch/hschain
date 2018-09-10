{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-} {- allow useful top functions for GHCi -}

module TM.Store ( tests) where

import Test.Tasty
import Test.Tasty.HUnit

import Control.Monad    (unless, when)
import Data.Int         (Int64)
import Data.Monoid      ((<>))
import Data.Traversable (forM)
import System.Directory (doesFileExist)

import qualified Data.Aeson            as JSON
import qualified Data.ByteString.Char8 as BC8

import Thundermint.Mock.Coin
import Thundermint.Store
import Thundermint.Store.SQLite

import qualified Thundermint.Mock.KeyVal as KeyVal


maxHeight :: Int64
maxHeight = 10

tests :: TestTree
tests =
    testGroup "generate blockchain and check on consistency"
                  [ testGroup "blockhains"
                                  [ testCase "key-val db" $ runKeyVal (Just maxHeight)  "./test-data/key-val" "spec/simple.json"
                                  -- , testCase "key-val stm" $ runKeyVal (Just maxHeight)  "./test-data/key-val" "spec/keyval-stm.json"
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
      checks <- forM storageList checkStorage
      assertEqual "failed consistency check" [] (concat checks)



checkBlockchainInvariants :: FilePath -> IO ()
checkBlockchainInvariants dbName = do
  b <- doesFileExist dbName
  unless b $ print $ "file " <> dbName <>  " does not exist."
--  assert $ "file " <> dbName <>  " does not exist."
  when b $ do
    withSQLiteBlockStorageRO dbName $ \(storage :: BlockStorage 'RO IO Alg [Tx]) -> do
      bs <- checkBlocks storage
      cs <- checkCommits storage
      vs <- checkValidators storage
      cbs <- checkCommitsBlocks storage
      assertEqual "failed consistency check" [] $ bs <> cs <> vs <> cbs
--
