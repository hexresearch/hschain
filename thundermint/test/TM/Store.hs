{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-} {- allow useful top functions for GHCi -}

module TM.Store ( tests) where

import Test.Tasty
import Test.Tasty.HUnit

import Data.Int         (Int64)
import Data.Traversable (forM)

import qualified Data.Aeson            as JSON
import qualified Data.ByteString.Char8 as BC8

import           Thundermint.Store
import qualified Thundermint.Mock.KeyVal as KeyVal
import           Thundermint.Mock.Types (ExeConfig(..))


maxHeight :: Int64
maxHeight = 10

tests :: TestTree
tests = testGroup "generate blockchain and check on consistency"
  [ testGroup "blockhains"
    [ testCase "key-val db" $ runKeyVal (Just maxHeight)  "./test-data/key-val" "../spec/simple-stm.json"
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
      storageList <- KeyVal.executeSpec maxH prefix (netSpec spec)
      -- Check result against consistency invariants
      checks <- forM storageList checkStorage
      assertEqual "failed consistency check" [] (concat checks)
