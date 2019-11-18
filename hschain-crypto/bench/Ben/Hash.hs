{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
-- |
module Ben.Hash (benchmarks) where

import Criterion.Main
import qualified Data.ByteString as BS
import qualified Data.Vector     as V

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
  [ bgroup "blob" (benchHashBlob @alg)
  , bgroup "vec"  (benchVec      @alg)
  ]

benchHashBlob :: forall alg. (CryptoHash alg) => [Benchmark]
benchHashBlob =
  [ bench (' ':show i)
  $ nf (hashBlob @alg) bs
  | (i,bs) <- blobs
  ]

benchVec :: forall alg. (CryptoHash alg) => [Benchmark]
benchVec =
  [ bench (' ':show (2 + 4 + 10*i))
  $ nf (hash @alg) xs
  | (i,xs) <- vectors
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
               , 100000
               ]
        ]

vectors :: [(Int, V.Vector Int)]
{-# NOINLINE vectors #-}
vectors = [ (i, V.replicate i 33)
        | i <- [ 0
               , 1,3
               , 10,30
               , 100,300
               , 1000,3000
               , 10000,30000
               , 100000
               ]
        ]
