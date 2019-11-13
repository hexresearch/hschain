-- |
-- Data for mock blockchain used in tests for consensus and gossip
module TM.Util.MockChain where

import           Data.List (sortOn)
import qualified Data.Map.Strict    as Map
import qualified Data.List.NonEmpty as NE

import HSChain.Crypto
import HSChain.Types
import HSChain.Types.Merkle.Types
import HSChain.Mock.KeyList
import HSChain.Mock.KeyVal  (BData(..),BState,genesisBlock)

import TM.Util.Network


----------------------------------------------------------------
-- Validators for mockchain
----------------------------------------------------------------

privK       :: [PrivKey TestAlg]
k1,k2,k3,k4 :: PrivKey TestAlg
privK@[k1,k2,k3,k4] = sortOn publicKey $ take 4 $ makePrivKeyStream 1337

valSet :: ValidatorSet TestAlg
Right valSet = makeValidatorSet [Validator (publicKey k) 1 | k <- privK]


----------------------------------------------------------------
-- Mockchain itself
----------------------------------------------------------------

-- | Genesis block of BCh
genesis :: Block TestAlg BData
genesis = genesisBlock valSet

block1, block1' :: Block TestAlg BData
block1  = mintFirstBlock $ BData [("K1",100)]
block1' = mintFirstBlock $ BData [("K1",101)]

mockchain :: [Block TestAlg BData]
mockchain
  = fmap fst
  $ scanl step (genesis,mempty) [BData [("K"++show i,i)] | i <- [100..]]
  where
    step (b,st) dat@(BData txs) = let st' = st <> Map.fromList txs
                                  in ( mintBlock b st' dat, st' )


----------------------------------------------------------------
-- Utils
----------------------------------------------------------------

mintFirstBlock :: BData -> Block TestAlg BData
mintFirstBlock dat@(BData txs)
  = mintBlock genesis (Map.fromList txs) dat

mintBlock :: Block TestAlg BData -> BState -> BData -> Block TestAlg BData
mintBlock b st dat = Block
  { blockHeight         = succ hPrev
  , blockPrevBlockID    = Just bid
  , blockValidatorsHash = hashed valSet
  , blockData           = merkled dat
  , blockValChange      = merkled mempty
  , blockPrevCommit     = merkled <$> commit
  , blockEvidence       = merkled []
  , blockStateHash      = hashed st
  }
  where
    hPrev  = blockHeight b
    r      = Round 0
    bid    = blockHash b
    commit | hPrev == Height 0 = Nothing
           | otherwise         = Just Commit
             { commitBlockID    = bid
             , commitPrecommits = NE.fromList
                 [ signValue i k $ Vote hPrev r (Time 0) (Just bid)
                 | k <- privK
                 , let Just i = indexByValidator valSet (publicKey k)
                 ]
             }
