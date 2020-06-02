{-# LANGUAGE FlexibleContexts             #-}
{-# LANGUAGE TypeFamilies                 #-}
{-# LANGUAGE UndecidableInstances         #-}

-- |
module TM.Util.Mockchain where

import Codec.Serialise
import Control.Applicative
import Data.Bits
import Data.Word
import Data.List  (unfoldr)

import System.IO.Unsafe (unsafePerformIO)

import HSChain.PoW.Types
import HSChain.Types.Merkle.Types
import HSChain.Examples.Simple

----------------------------------------------------------------
--
----------------------------------------------------------------

mockchain :: (Num (Nonce cfg), Show (Nonce cfg), KVConfig cfg)
          => [Block (KV cfg)]
mockchain = gen : unfoldr (Just . (\b -> (b,b)) . mineBlock "VAL") gen
  where
    gen = GBlock { blockHeight = Height 0
                 , blockTime   = Time 0
                 , prevBlock   = Nothing
                 , blockData   = KV { kvData       = merkled []
                                    , kvNonce      = 0
                                    , kvTarget     = Target $ shiftL 1 256 - 1
                                    }
                 }

mineBlock :: (Num (Nonce cfg), Show (Nonce cfg), KVConfig cfg)
          => String -> Block (KV cfg) -> Block (KV cfg)
mineBlock val b = unsafePerformIO $ do
  find $ GBlock
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
  where
    find blk = do
      r <- mine blk
      case r of
        Just b' -> return b'
        Nothing -> let Time t = blockTime blk
                   in find (blk { blockTime = Time (t+1)})

data MockChain

instance Serialise (Nonce MockChain) => KVConfig MockChain where
  type Nonce MockChain = Word64
  kvAdjustInterval = Const 100
  kvBlockTimeInterval  = Const (Time 1000)
  kvSolvePuzzle blk = case solved of
    blk' : _ -> return (Just blk')
    _ -> return Nothing
    where
      nonces = map (+ kvNonce (blockData blk)) [0..2^(24 :: Int) - 1]
      solved = [ blk'
               | nonce <- nonces
               , let blk' = blk { blockData = (blockData blk) { kvNonce = nonce } }
               , let hdr' = toHeader blk'
               , let tgt' = hash256AsTarget hdr'
               , tgt' <= tgt
               ]
      tgt = blockTargetThreshold blk
  kvCheckPuzzle hdr = return $ blockTargetThreshold hdr >= resultTgt
    where
      resultTgt = hash256AsTarget hdr

genesis,block1,block2,block3,block2' :: Block (KV MockChain)
genesis:block1:block2:block3:_ = mockchain
block2' = mineBlock "Z" block1


header1,header2,header3,header2' :: Header (KV MockChain)
header1  = toHeader block1
header2  = toHeader block2
header3  = toHeader block3
header2' = toHeader block2'

