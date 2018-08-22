{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}

module TM.Store ( tests) where

import Test.Tasty
import Test.Tasty.HUnit

import Control.Monad    (unless, when)
import Data.Monoid      ((<>))
import System.Directory (doesFileExist)

import Thundermint.Mock.Coin
import Thundermint.Store
import Thundermint.Store.SQLite

tests :: TestTree
tests =
    testGroup "blockchain validity test"
                  [ testGroup "existence"
                                  [ testCase "node-1" $ checkBlockchainInvariants "./db/node-1"
                                  , testCase "node-2" $ checkBlockchainInvariants "./db/node-2"
                                  , testCase "node-3" $ checkBlockchainInvariants "./db/node-3"
                                  , testCase "node-4" $ checkBlockchainInvariants "./db/node-4"
                                  ]
                  ]
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
