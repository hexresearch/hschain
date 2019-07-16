{-# LANGUAGE OverloadedStrings #-}
-- |
module Crypto (benchmarks) where

import Criterion.Main
import qualified Data.ByteString as BS

import Thundermint.Crypto
import Thundermint.Crypto.Ed25519

----------------------------------------------------------------
--
----------------------------------------------------------------

benchmarks :: Benchmark
benchmarks = bgroup "Crypto"
  [ bench "Signing  (Ed25519)" $ nf (signBlob privK) blob
  , bench "Checking (Ed25519)" $ nf (verifyBlobSignature pubK blob) sign
  ]


blob :: BS.ByteString
blob = BS.replicate 40 33

sign :: Signature Ed25519
sign = signBlob privK blob

privK :: PrivKey Ed25519
privK = read "\"7xgYfYCgUzcWDBtfL4xjp842SNoy3pE75s4ojzuJjaR3\""

pubK :: PublicKey Ed25519
pubK = publicKey privK
