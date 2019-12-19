{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
-- |
module Dioxane (benchmarks) where

import Criterion.Main

import HSChain.Crypto
import HSChain.Mock.Dioxane
import HSChain.Types
import HSChain.Types.Merkle.Types


benchmarks :: Benchmark
benchmarks
  = bgroup "Dioxane"
  [ bench "generate" $ nf dioGenerate newBlock
  , bench "process"
  $ nf dioProcess (let Just dat = dioGenerate newBlock
                       block    = Block
                         { blockHeight        = Height 1
                         , blockPrevBlockID   = Nothing
                         , blockValidators    = hashed $ validatorSet genesis
                         , blockNewValidators = hashed $ validatorSet genesis
                         , blockPrevCommit    = Nothing
                         , blockEvidence      = merkled []
                         , blockData          = merkled $ bchValue dat
                         , blockStateHash     = merkled $ blockchainState genesis
                         }
                    in block <$ genesis
                   )
  ]

type Tag = DioTag 10000 4

newBlock :: NewBlock (BData Tag)
newBlock = NewBlock
  { newBlockHeight   = Height 1
  , newBlockLastBID  = BlockID (Hashed (Hash ""))
  , newBlockCommit   = Nothing
  , newBlockEvidence = []
  , newBlockState    = blockchainState genesis
  , newBlockValSet   = validatorSet    genesis
  }

genesis :: Genesis (BData Tag)
genesis = dioGenesis

dioGenerate :: NewBlock (BData Tag) -> Maybe (ProposedBlock (BData Tag))
dioGenerate nb = generateBlock bchLogic nb []

dioProcess :: BlockValidation (BData Tag) -> Maybe (EvaluationResult (BData Tag))
dioProcess = processBlock bchLogic
