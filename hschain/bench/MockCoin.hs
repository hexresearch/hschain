{-# LANGUAGE OverloadedStrings #-}
-- |
module MockCoin (benchmarks) where

import Data.Functor.Identity
import Control.Monad
import Criterion.Main

import HSChain.Crypto
import HSChain.Blockchain.Interpretation
import HSChain.Mock.Coin
import HSChain.Mock.KeyList
import HSChain.Mock.Types
import HSChain.Types

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
    [ env (makeBlock <$> generateTxList size) $ \( ~(Just (bdata,_)) ) ->
        bench (show (length (blockTransactions bdata)) ++ " of " ++ show size) $
        nf (\b -> runIdentity
                $ interpretBCh runner state
                $ processBlock transitions
                ( Block { blockData       = b
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
                        } :: Block Alg BData
                )
           ) (bdata :: BData)
    | size <- [0, 1, 10, 100, 1000]
    ]
  ]
  where
    makeBlock = runIdentity
              . interpretBCh runner state
              . generateBlock transitions (error "We don't actually use block there")


----------------------------------------------------------------
-- Wallets and initial state
----------------------------------------------------------------

privKeys :: [PrivKey Alg]
privKeys = take 1000 $ makePrivKeyStream 1337

pubKeys :: [PublicKey Alg]
pubKeys = publicKey <$> privKeys

txGen   :: HSChain.Mock.Coin.TxGenerator
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
   
state :: BlockchainState Alg BData
Identity (Just ((), state))
  = interpretBCh runner (BlockchainState (CoinState mempty mempty) emptyValidatorSet)
  $ processBlock transitions genesis

generateTxList :: Int -> IO [Tx]
generateTxList n
  = replicateM n
  $ generateTransaction txGen
  $ blockchainState state
