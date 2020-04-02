{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
-- |
module Ben.Hash (benchmarks) where

import Control.DeepSeq
import Criterion.Main
import GHC.Generics
import qualified Data.ByteString as BS
import qualified Data.Vector     as V

import HSChain.Crypto
import HSChain.Crypto.Classes.Hash
import HSChain.Crypto.SHA


benchmarks :: Benchmark
benchmarks = bgroup "Hash"
  [ bgroup "SHA1"   (benchHash @SHA1)
  , bgroup "SHA256" (benchHash @SHA256)
  , bgroup "SHA384" (benchHash @SHA384)
  , bgroup "SHA512" (benchHash @SHA512)
  , bgroup "helpers"
    [ benchNullTerminatedString
    ]
  ]

data Data1 = D1 Int deriving (Generic, NFData)

data Data2 = D2 Int deriving (Generic, NFData)

data Data1Looooooooooooooooong = D3 Int deriving (Generic, NFData)

data Data2Looooooooooooooooong = D4 Int deriving (Generic, NFData)

benchReplChr :: Char
benchReplChr = '鎖'

shortLibName :: String
shortLibName = replicate 5 benchReplChr

longLibName :: String
longLibName = replicate 20 benchReplChr

instance CryptoHashable Data1 where
  hashStep = genericHashStep shortLibName

instance CryptoHashable Data2 where
  hashStep = genericHashStep longLibName

instance CryptoHashable Data1Looooooooooooooooong where
  hashStep = genericHashStep shortLibName

instance CryptoHashable Data2Looooooooooooooooong where
  hashStep = genericHashStep longLibName

benchNullTerminatedString :: Benchmark
benchNullTerminatedString =
  bgroup "nullTerminatedString"
    [ bench1
    , bench2
    , bench3
    , bench4
    ]
  where
    bench1 = bench "short libname / short typename"  $ nf (map (hash @SHA512)) (map D1 [1..100])
    bench2 = bench "long  libname / short typename"  $ nf (map (hash @SHA512)) (map D2 [1..100])
    bench3 = bench "short libname / long  typename"  $ nf (map (hash @SHA512)) (map D3 [1..100])
    bench4 = bench "long  libname / long  typename"  $ nf (map (hash @SHA512)) (map D4 [1..100])


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
