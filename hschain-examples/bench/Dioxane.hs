{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
-- |
module Dioxane (benchmarks) where

import Codec.Serialise
import Control.Arrow ((&&&))
import Criterion.Main
import Data.Functor.Identity
import qualified Data.ByteString.Lazy as BL
import qualified Data.Vector          as V

import HSChain.Crypto
import HSChain.Internal.Types.Consensus
import HSChain.Mock.Dioxane
import HSChain.Mock.KeyList
import HSChain.Types
import HSChain.Types.Merkle.Types


benchmarks :: Benchmark
benchmarks
  = bgroup "Dioxane"
  [ bench "generate"    $ nf dioGenerate newBlock
  , bench "process"     $ whnf dioProcess block1
  , bench "serialize"   $ nf serialise block1
  , bench "deserialize" $ nf (deserialise :: BL.ByteString -> Block (BData Tag)) (serialise block1)
  ]

----------------------------------------------------------------
--
----------------------------------------------------------------

data Tag

instance Dio Tag where
  dioDict = DioDict
    { dioUserKeys       = V.fromList
                        $ take 10000
                        $ map (id &&& publicKey)
                        $ makePrivKeyStream 1337
    , dioInitialBalance = 1000000
    , dioValidators     = 4
    }

----------------------------------------------------------------
-- Constants
----------------------------------------------------------------

genesis :: Genesis (BData Tag)
genesis = dioGenesis

newBlock :: NewBlock (BData Tag)
newBlock = NewBlock
  { newBlockHeight   = Height 1
  , newBlockLastBID  = BlockID (Hashed (Hash ""))
  , newBlockCommit   = Nothing
  , newBlockEvidence = []
  , newBlockValSet   = genesisValSet genesis
  }

block1 :: Block (BData Tag)
block1 = Block
  { blockHeight        = Height 1
  , blockPrevBlockID   = Nothing
  , blockValidators    = valHash
  , blockNewValidators = valHash
  , blockPrevCommit    = Nothing
  , blockEvidence      = merkled []
  , blockData          = merkled dat
  }
  where
    dat     = dioGenerate newBlock
    valHash = hashed $ genesisValSet genesis

viewH0 :: StateView Identity (BData Tag)
viewH0 = st1
  where
    Right st1 = runIdentity
              $ validatePropBlock st0 (genesisBlock genesis) (genesisValSet genesis)
    st0 = inMemoryStateView $ genesisValSet genesis

dioGenerate :: NewBlock (BData Tag) -> BData Tag
dioGenerate nb
  = fst $ runIdentity
  $ generateCandidate viewH0 nb

dioProcess :: Block (BData Tag) -> Either (BChError (BData Tag)) (StateView Identity (BData Tag))
dioProcess b
  = runIdentity
  $ validatePropBlock viewH0 b (genesisValSet genesis)
