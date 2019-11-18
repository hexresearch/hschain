{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
-- |
module Ben.Hash (benchmarks) where

import Criterion.Main
import qualified Data.ByteString as BS

import HSChain.Crypto
import HSChain.Crypto.SHA


benchmarks :: Benchmark
benchmarks = bgroup "Hash"
  [ bgroup "SHA1"   (benchHash @SHA1)
  , bgroup "SHA256" (benchHash @SHA256)
  , bgroup "SHA384" (benchHash @SHA384)
  , bgroup "SHA512" (benchHash @SHA512)
  ]


benchHash :: forall alg. (CryptoHash alg) => [Benchmark]
benchHash =
  [ bench ("blob " ++ show i) $ nf (hash @alg) bs
  | (i,bs) <- blobs
  ]

blobs :: [(Int, BS.ByteString)]
{-# NOINLINE blobs #-}
blobs = [ (i, BS.replicate i 33)
        | i <- [ 0
               , 1,3
               , 10,30
               , 100,300
               , 1000,3000
               , 10000,30000
               , 100000]
        ]
