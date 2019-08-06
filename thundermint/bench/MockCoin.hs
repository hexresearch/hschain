{-# LANGUAGE OverloadedStrings #-}
-- |
module MockCoin (benchmarks) where

import Control.Monad
import Criterion.Main

import Thundermint.Crypto
import Thundermint.Blockchain.Interpretation
import Thundermint.Mock.Coin
import Thundermint.Mock.KeyList
import Thundermint.Mock.Types
import Thundermint.Types

----------------------------------------------------------------
-- Benchmarks
----------------------------------------------------------------

benchmarks :: Benchmark
benchmarks
  = bgroup "Mock coin"
  [ bgroup "create block"
    [ env (generateTxList size)
    $ bench (show size)
    . nf (transactionsToBlock transitions (Height 1) state)
    | size <- [0, 1, 10, 100, 1000]
    ]
  , bgroup "react"
    [ env (transactionsToBlock transitions (Height 1) state <$> generateTxList size)
    $ bench (show size)
    . nf (\(_,b) -> processBlock transitions CheckSignature
           (Block { blockData       = b
                  , blockHeader     = Header
                    { headerHeight         = Height 0
                    , headerTime           = Time 0
                    , headerChainID        = ""
                    , headerLastBlockID    = Nothing
                    , headerValidatorsHash = Hashed (Hash "")
                    , headerDataHash       = Hashed (Hash "")
                    , headerValChangeHash  = Hashed (Hash "")
                    , headerLastCommitHash = Hashed (Hash "")
                    , headerEvidenceHash   = Hashed (Hash "")
                    }
                  , blockValChange  = mempty
                  , blockLastCommit = Nothing
                  , blockEvidence   = []
                  } :: Block Alg BData)
           state
         )
    | size <- [0, 1, 10, 100, 1000]
    ]
  ]

----------------------------------------------------------------
-- Wallets and initial state
----------------------------------------------------------------

privKeys :: [PrivKey Alg]
privKeys = take 1000 $ makePrivKeyStream 1337

pubKeys :: [PublicKey Alg]
pubKeys = publicKey <$> privKeys

txGen   :: Thundermint.Mock.Coin.TxGenerator
genesis :: Block Alg BData
(Just txGen,genesis) = mintMockCoin
  [ Validator k 1 | k <- take 4 pubKeys ]
  CoinSpecification
    { coinAridrop        = 1000
    , coinWallets        = 1000
    , coinWalletsSeed    = 1337
    , coinGeneratorDelay = Just 0
    , coinMaxMempoolSize = 1000
    }
   
state :: CoinState
Just state = processBlock transitions AlreadyChecked genesis (CoinState mempty mempty)

generateTxList :: Int -> IO [Tx]
generateTxList n = replicateM n $ generateTransaction txGen state

