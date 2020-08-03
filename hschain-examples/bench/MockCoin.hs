{-# LANGUAGE OverloadedStrings #-}
-- |
module MockCoin (benchmarks) where

-- import Data.Either (fromRight)
-- import Control.Monad
import Criterion.Main

-- import HSChain.Crypto
-- import HSChain.Internal.Types.Consensus
-- import HSChain.Mock.Coin
-- import HSChain.Mock.KeyList
-- import HSChain.Mock.Types
-- import HSChain.Types
-- import HSChain.Types.Merkle.Types

----------------------------------------------------------------
-- Benchmarks
----------------------------------------------------------------

benchmarks :: Benchmark
benchmarks
  = bgroup "Mock coin"
  [ {-bgroup "create block"
    [ env (generateTxList size) $ \(st,txs) ->
        bench (show size) $ nfIO $ makeBlock st txs
    | size <- [0, 1, 10, 100, 1000]
    ]
  , bgroup "react"
    [ env (makeBlock <$> generateTxList size) $ \bdata ->
        bench (show (length (let BData txs = bchValue bdata in txs)) ++ " of " ++ show size) $
        nf (\dat -> let b = Block { blockHeight        = Height 0
                                  , blockPrevBlockID   = Nothing
                                  , blockValidators    = hashed emptyValidatorSet
                                  , blockNewValidators = hashed emptyValidatorSet
                                  , blockData          = merkled $ bchValue dat
                                  , blockPrevCommit    = Nothing
                                  , blockEvidence      = merkled []
                                  }
                    in processBlock coinLogic $ b <$ bdata
           ) bdata
    | size <- [0, 1, 10, 100, 1000]
    ]-}
  ]
  where
    -- makeBlock st txs = do
    --   generateCandidate st txs
    -- fromRight (error "Block shall be generated")
    --           . generateBlock coinLogic (error "We don't actually use block there")


----------------------------------------------------------------
-- Wallets and initial state
----------------------------------------------------------------
{-
privKeys :: [PrivKey (Alg BData)]
privKeys = take 1000 $ makePrivKeyStream 1337

pubKeys :: [PublicKey (Alg BData)]
pubKeys = publicKey <$> privKeys


genesis :: Genesis BData
genesis = coinGenesis validators coinSpec

txGen :: TxGenerator
Just txGen = makeCoinGenerator coinSpec

coinSpec :: CoinSpecification
coinSpec = CoinSpecification
  { coinAirdrop        = 1000
  , coinWallets        = 1000
  , coinWalletsSeed    = 1337
  , coinGeneratorDelay = Just 0
  , coinMaxMempoolSize = 1000
  }

validators = [ Validator k 1 | k <- take 4 pubKeys ]

generateTxList :: Int -> IO (StateView IO BData, [Tx])
generateTxList n = do
  (st,_,readST) <- inMemoryStateView valSet
  Right stH0    <- validatePropBlock st (genesisBlock genesis) valSet
  txs <- replicateM n $ generateTransaction txGen =<< readST
  return (stH0, txs)
  where
    Right valSet = makeValidatorSet validators

-}
