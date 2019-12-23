{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
-- |
module Dioxane (benchmarks) where

import Codec.Serialise
import Control.Arrow ((&&&))
import Criterion.Main
import qualified Data.ByteString.Lazy as BL
import qualified Data.Vector          as V

import HSChain.Crypto
import HSChain.Mock.Dioxane
import HSChain.Mock.KeyList
import HSChain.Types
import HSChain.Types.Merkle.Types


benchmarks :: Benchmark
benchmarks
  = bgroup "Dioxane"
  [ bench "generate"    $ nf dioGenerate newBlock
  , bench "process"     $ nf dioProcess (block1 <$ genesis)
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
  , newBlockState    = blockchainState genesis
  , newBlockValSet   = merkleValue $ validatorSet genesis
  }

block1 :: Block (BData Tag)
block1 = Block
  { blockHeight        = Height 1
  , blockPrevBlockID   = Nothing
  , blockValidators    = merkleHashed $ validatorSet genesis
  , blockNewValidators = merkleHashed $ validatorSet genesis
  , blockPrevCommit    = Nothing
  , blockEvidence      = merkled []
  , blockData          = merkled $ bchValue dat
  , blockStateHash     = merkleHashed $ blockchainState genesis
  }
  where Just dat = dioGenerate newBlock

dioGenerate :: NewBlock (BData Tag) -> Maybe (ProposedBlock (BData Tag))
dioGenerate nb = generateBlock bchLogic nb []

dioProcess :: BlockValidation (BData Tag) -> Maybe (EvaluationResult (BData Tag))
dioProcess = processBlock bchLogic
