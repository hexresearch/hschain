-- |
-- Data for mock blockchain used in tests for consensus and gossip
module TM.Util.MockChain where

import qualified Data.List.NonEmpty as NE

import HSChain.Crypto
import HSChain.Types
import HSChain.Types.Merklized
import HSChain.Mock.KeyList
import HSChain.Mock.KeyVal  (BData(..))
import HSChain.Mock.Types   (makeGenesis)

import TM.Util.Network


----------------------------------------------------------------
-- Validators for mockchain
----------------------------------------------------------------

privK       :: [PrivKey TestAlg]
k1,k2,k3,k4 :: PrivKey TestAlg
privK@[k1,k2,k3,k4] = take 4 $ makePrivKeyStream 1337

valSet :: ValidatorSet TestAlg
Right valSet = makeValidatorSet [Validator (publicKey k) 1 | k <- privK]


----------------------------------------------------------------
-- Mockchain itself
----------------------------------------------------------------

-- | Genesis block of BCh
genesis :: Block TestAlg BData
genesis = makeGenesis (BData []) valSet

block1, block1' :: Block TestAlg BData
block1  = mintBlock genesis $ BData [("K1",100)]
block1' = mintBlock genesis $ BData [("K1",101)]

mockchain :: [Block TestAlg BData]
mockchain = scanl mintBlock genesis [BData [("K"++show i,i)] | i <- [100..]]

----------------------------------------------------------------
-- Utils
----------------------------------------------------------------

mintBlock :: Block TestAlg BData -> BData -> Block TestAlg BData
mintBlock b dat  = Block
  { blockHeight         = succ hPrev
  , blockPrevBlockID    = Just bid
  , blockValidatorsHash = hashed valSet
  , blockData           = merkled dat
  , blockValChange      = merkled mempty
  , blockPrevCommit     = merkled <$> commit
  , blockEvidence       = merkled []
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
