{-# LANGUAGE OverloadedStrings #-}
-- |
module TM.Crypto (tests) where

import Test.Tasty
import Test.Tasty.HUnit

import Thundermint.Crypto
import Thundermint.Crypto.Ed25519

tests :: TestTree
tests = testGroup "Crypto"
  [ testCase "read . show = id @ Address/PublicKey/PrivKey Ed25519_SHA512"
  $ do privK <- generatePrivKey
       let pubK = publicKey privK
           addr = address pubK
       privK @=? (read . show) privK
       pubK  @=? (read . show) pubK
       addr  @=? (read . show) addr
  , testCase "read . show = id @ Address Ed25519_SHA512"
  $ do let x = hashBlob "ABCD" :: Hash Ed25519_SHA512
       x @=? (read . show) x
  ]
