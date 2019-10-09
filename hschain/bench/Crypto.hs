{-# LANGUAGE OverloadedStrings #-}
-- |
module Crypto (benchmarks) where

import Criterion.Main
import qualified Data.ByteString as BS

import HSChain.Crypto
import HSChain.Crypto.Ed25519
import HSChain.Crypto.BLS

----------------------------------------------------------------
--
----------------------------------------------------------------

benchmarks :: Benchmark
benchmarks = bgroup "Crypto"
  [ bench "Signing  (Ed25519)"    $ nf (signBlob privK) blob
  , bench "Checking (Ed25519)"    $ nf (verifyBlobSignature pubK blob) sign
  , bench "Signing  (BLS)"        $ nf (signBlob privKeyBLS) blob
  , bench "Checking (BLS)"        $ nf (verifyBlobSignature pubKeyBLS blob) signBLS
  , bench "Signing hashed  (BLS)" $ nf (signHash privKeyBLS) blobHash
  , bench "Checking hashed (BLS)" $ nf (verifyHashSignature pubKeyBLS blobHash) signBLS
  ]


blob :: BS.ByteString
blob = BS.replicate 40 33

blobHash :: Hash BLS
blobHash = hashBlob blob

sign :: Signature Ed25519
sign = signBlob privK blob

signBLS :: Signature BLS
signBLS = signBlob privKeyBLS blob

privK :: PrivKey Ed25519
privK = read "\"7xgYfYCgUzcWDBtfL4xjp842SNoy3pE75s4ojzuJjaR3\""

pubK :: PublicKey Ed25519
pubK = publicKey privK

privKeyBLS :: PrivKey BLS
privKeyBLS = read "\"3fDbEdCy2bauoADeCGZwB8ANT4r58274dpE2TKoGq3JV\""

pubKeyBLS :: PublicKey BLS
pubKeyBLS = publicKey privKeyBLS

