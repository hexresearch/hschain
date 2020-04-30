-- |
module TM.Util.Mockchain where

import Data.Maybe (fromJust)
import Data.List  (unfoldr)

import HSChain.PoW.Types
import HSChain.Types.Merkle.Types
import HSChain.Examples.Simple

import HSChain.Crypto
import HSChain.Crypto.SHA

----------------------------------------------------------------
--
----------------------------------------------------------------

mockchain :: [Block KV]
mockchain = gen : unfoldr (Just . (\b -> (b,b)) . mineBlock "VAL") gen
  where
    gen = GBlock { blockHeight = Height 0
                 , blockTime   = Time 0
                 , prevBlock   = Nothing
                 , blockData   = KV { kvData       = merkled []
                                    , kvNonce      = 0
                                    , kvDifficulty = 256
                                    }
                 }

mineBlock :: String -> Block KV -> Block KV
mineBlock val b = fromJust $ mine $ GBlock
  { blockHeight = succ $ blockHeight b
  , blockTime   = Time 0
  , prevBlock   = Just $! blockID b
  , blockData   = KV { kvData = merkled [ let Height h = blockHeight b
                                          in (fromIntegral h, val)
                                        ]
                     , kvNonce      = 0
                     , kvDifficulty = kvDifficulty (blockData b)
                     }
  }


genesis,block1,block2,block3,block2' :: Block KV
genesis:block1:block2:block3:_ = mockchain
block2' = mineBlock "Z" block1


header1,header2,header3,header2' :: Header KV
header1  = toHeader block1
header2  = toHeader block2
header3  = toHeader block3
header2' = toHeader block2'
