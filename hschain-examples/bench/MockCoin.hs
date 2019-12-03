{-# LANGUAGE OverloadedStrings #-}
-- |
module MockCoin (benchmarks) where

import Data.Functor.Identity
import Control.Monad
import Criterion.Main

import HSChain.Crypto
import HSChain.Mock.Coin
import HSChain.Mock.KeyList
import HSChain.Mock.Types
import HSChain.Run
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
    [ env (makeBlock <$> generateTxList size) $ \( ~(Just (bdata,_)) ) ->
        bench (show (length (blockTransactions bdata)) ++ " of " ++ show size) $
        nf (\b -> runIdentity
                $ interpretBCh runner state
                $ processBlock transitions
                ( Block { blockHeight        = Height 0
                        , blockPrevBlockID   = Nothing
                        , blockValidators    = hashed emptyValidatorSet
                        , blockNewValidators = hashed emptyValidatorSet
                        , blockData          = merkled b
                        , blockPrevCommit    = Nothing
                        , blockEvidence      = merkled []
                        , blockStateHash     = Hashed (Hash "")
                        }
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
(Just txGen,genesis,_) = mintMockCoin
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
