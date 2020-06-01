{-# LANGUAGE FlexibleContexts             #-}
{-# LANGUAGE TypeFamilies                 #-}
{-# LANGUAGE UndecidableInstances         #-}

-- |
module TM.Util.Mockchain where

import Codec.Serialise
import Control.Applicative
import Data.Maybe (fromJust)
import Data.Word
import Data.List  (unfoldr)

import System.IO.Unsafe (unsafePerformIO)

import HSChain.PoW.Types
import HSChain.Types.Merkle.Types
import HSChain.Examples.Simple

----------------------------------------------------------------
--
----------------------------------------------------------------

mockchain :: (Num (Nonce cfg), KVConfig cfg) => [Block (KV cfg)]
mockchain = gen : unfoldr (Just . (\b -> (b,b)) . mineBlock "VAL") gen
  where
    gen = GBlock { blockHeight = Height 0
                 , blockTime   = Time 0
                 , prevBlock   = Nothing
                 , blockData   = KV { kvData       = merkled []
                                    , kvNonce      = 0
                                    , kvTarget     = Target $ 1 ^ (256 :: Int) - 1
                                    }
                 }

mineBlock :: (Num (Nonce cfg), KVConfig cfg) => String -> Block (KV cfg) -> Block (KV cfg)
mineBlock val b = unsafePerformIO $ do
  r <- mine $ GBlock
    { blockHeight = succ $ blockHeight b
    , blockTime   = Time 0
    , prevBlock   = Just $! blockID b
    , blockData   = KV { kvData = merkled [ let Height h = blockHeight b
                                            in (fromIntegral h, val)
                                          ]
                       , kvNonce      = 0
                       , kvTarget     = kvTarget (blockData b)
                       }
    }
  case r of
    Just b' -> return b
    Nothing -> error "haven't figured out what to do."


data MockChain

instance Serialise (Nonce MockChain) => KVConfig MockChain where
  type Nonce MockChain = Word64
  kvAdjustInterval = Const 100
  kvBlockTimeInterval  = Const (Time 1000)

genesis,block1,block2,block3,block2' :: Block (KV MockChain)
genesis:block1:block2:block3:_ = mockchain
block2' = mineBlock "Z" block1


header1,header2,header3,header2' :: Header (KV MockChain)
header1  = toHeader block1
header2  = toHeader block2
header3  = toHeader block3
header2' = toHeader block2'

