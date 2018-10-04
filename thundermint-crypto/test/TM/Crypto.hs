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
           blob = "ABCD"
           sign = signBlob privK blob
       privK @=? (read . show) privK
       pubK  @=? (read . show) pubK
       addr  @=? (read . show) addr
       sign  @=? (read . show) sign
    --
  , testCase "read . show = id @ Hash Ed25519_SHA512"
  $ do let x = hashBlob "ABCD" :: Hash Ed25519_SHA512
       x @=? (read . show) x
    --
  , testCase "Signature OK (roundtrip)"
  $ do privK <- generatePrivKey
       let pubK = publicKey privK
           blob = "ABCD"
           sign = signBlob privK blob
       assertBool "Signature check failed" $ verifyBlobSignature pubK blob sign
    --
  , testCase "Signature OK (golden)"
  $ do let privK = read "\"HsBRtVUZ8ewZq5giWZHwr8crJm1iTVNLQeeyQk1vEmM8\""  :: PrivKey   Ed25519_SHA512
           pubK  = read "\"9xeYWVLneMHJHEtewZe9X4nKbLAYjFEHR98q9CyhSxQN\""  :: PublicKey Ed25519_SHA512
           sign  = read
             "Signature \
             \\"3R3ShiGCBPZNaPhyAtfaVX3V23YPHQABDxNn2F6qPwFANj1gUCBic6oBLGpSBXqpq1ZCKekA95ociGfVEdKhZhU7\""
           blob  = "ABCD"
       sign @=? signBlob privK blob
       assertBool "Signature check failed" $ verifyBlobSignature pubK blob sign
  ]
