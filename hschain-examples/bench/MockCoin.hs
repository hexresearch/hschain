{-# LANGUAGE OverloadedStrings #-}
-- |
module MockCoin (benchmarks) where

import Data.Either (fromRight)
import Control.Monad
import Criterion.Main

import HSChain.Crypto
import HSChain.Mock.Coin
import HSChain.Mock.KeyList
import HSChain.Mock.Types
import HSChain.Types
import HSChain.Types.Merkle.Types

----------------------------------------------------------------
-- Benchmarks
----------------------------------------------------------------

benchmarks :: Benchmark
benchmarks
  = bgroup "Mock coin"
  [ bgroup "create block"
    [ env (generateTxList size)
    $ bench (show size)
    . nf makeBlock
    | size <- [0, 1, 10, 100, 1000]
    ]
  , bgroup "react"
    [ env (makeBlock <$> generateTxList size) $ \bdata ->
        bench (show (length (blockTransactions (bchValue bdata))) ++ " of " ++ show size) $
        nf (\dat -> let b = Block { blockHeight        = Height 0
                                  , blockPrevBlockID   = Nothing
                                  , blockValidators    = hashed emptyValidatorSet
                                  , blockNewValidators = hashed emptyValidatorSet
                                  , blockData          = merkled $ bchValue dat
                                  , blockPrevCommit    = Nothing
                                  , blockEvidence      = merkled []
                                  , blockStateHash     = Hashed (Hash "")
                                  }
                    in processBlock coinLogic $ b <$ bdata
           ) bdata
    | size <- [0, 1, 10, 100, 1000]
    ]
  ]
  where
    makeBlock = fromRight (error "Block shall be generated")
              . generateBlock coinLogic (error "We don't actually use block there")


----------------------------------------------------------------
-- Wallets and initial state
----------------------------------------------------------------

privKeys :: [PrivKey Alg]
privKeys = take 1000 $ makePrivKeyStream 1337

pubKeys :: [PublicKey Alg]
pubKeys = publicKey <$> privKeys

txGen   :: HSChain.Mock.Coin.TxGenerator
genesis :: Genesis Alg BData
(Just txGen,genesis) = mintMockCoin
  [ Validator k 1 | k <- take 4 pubKeys ]
  CoinSpecification
    { coinAridrop        = 1000
    , coinWallets        = 1000
    , coinWalletsSeed    = 1337
    , coinGeneratorDelay = Just 0
    , coinMaxMempoolSize = 1000
    }

state :: EvaluationResult Alg BData
Right state = processBlock coinLogic genesis

generateTxList :: Int -> IO [Tx]
generateTxList n
  = replicateM n
  $ generateTransaction txGen
  $ blockchainState state
