-- |
module Ben.Ed25519 where

import Criterion.Main
import qualified Data.ByteString as BS

import HSChain.Crypto
import HSChain.Crypto.Ed25519


benchmarks :: Benchmark
benchmarks = bgroup "Ed25519"
  [ bench "Signing"   $ nf (signBlob privK) blob
  , bench "Checking"  $ nf (verifyBlobSignature pubK blob) sign
  ]

blob :: BS.ByteString
blob = BS.replicate 40 33

sign :: Signature Ed25519
sign = signBlob privK blob

privK :: PrivKey Ed25519
privK = read "\"7xgYfYCgUzcWDBtfL4xjp842SNoy3pE75s4ojzuJjaR3\""

pubK :: PublicKey Ed25519
pubK = publicKey privK
