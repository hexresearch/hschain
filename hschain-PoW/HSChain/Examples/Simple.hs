{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}
-- |
--
module HSChain.Examples.Simple where

import Codec.Serialise      (Serialise)
import Data.Bits
import Data.Functor.Classes (Show1)
import Data.List            (find)
import Data.Word
import qualified Data.Aeson      as JSON
import qualified Data.ByteString as BS
import Numeric.Natural
import GHC.Generics         (Generic)

import HSChain.Crypto
import HSChain.Crypto.Classes.Hash
import HSChain.Crypto.SHA
import HSChain.Types.Merkle.Types
import HSChain.PoW.Types

import Debug.Trace

----------------------------------------------------------------
--

data KV f = KV
  { kvData       :: !(MerkleNode f SHA256 [(Int,String)])
  , kvNonce      :: !Word32
  , kvDifficulty :: !Natural
  }
  deriving stock (Generic)
deriving stock instance Show1    f => Show (KV f)
deriving stock instance IsMerkle f => Eq   (KV f)
instance Serialise (KV Identity)
instance Serialise (KV Proxy)


instance IsMerkle f => CryptoHashable (KV f) where
  hashStep = genericHashStep "hschain"

instance MerkleMap KV where
  merkleMap f KV{..} = KV { kvData = mapMerkleNode f kvData
                          , ..
                          }

instance BlockData KV where
  newtype BlockID KV = KV'BID (Hash SHA256)
    deriving newtype (Show,Eq,Ord,CryptoHashable,Serialise, JSON.ToJSON, JSON.FromJSON)
  data ChainConfig KV = KVCfg
    { kvAdjustInterval :: Height
    , kvBlockDelay     :: Natural
    }
    deriving stock (Show)
  --
  blockID b = let Hashed h = hashed b in KV'BID h
  validateHeader cfg bh (Time now) header
    = return
    $ and
    [ hash256 header <= blockTarget header
    , kvDifficulty (blockData header) == retarget cfg bh
    -- Time checks
    , t <= now + (2*60*60*1000)
    -- FIXME: Check that we're ahead of median time of N prev block
    ]
    where
      Time t = blockTime header
  --
  validateBlock  _ _ = return True
  blockWork      b = Work $ kvDifficulty $ blockData b


blockTarget :: GBlock KV f -> Natural
blockTarget b = 2^(256::Int) `div` kvDifficulty (blockData b)

-- FIXME: correctly compute rertargeting
retarget :: ChainConfig KV -> BH KV -> Natural
retarget KVCfg{..} bh
  -- Retarget
  | bhHeight bh `mod` kvAdjustInterval == 0
  , Just old <- goBack kvAdjustInterval bh
  , bhHeight old /= 0
  =   let Time t1 = bhTime old
          Time t2 = bhTime bh
          tgt     = 2^(256::Int) `div` kvDifficulty (bhData bh)
          tgt'    = (tgt * fromIntegral (t2 - t1)) `div` (fromIntegral kvAdjustInterval * kvBlockDelay)
      in traceShowId $ 2^(256::Int) `div` tgt'
  | otherwise
    = kvDifficulty $ bhData bh

hash256 :: CryptoHashable a => a -> Natural
hash256 a
  = BS.foldl' (\i w -> (i `shiftL` 8) + fromIntegral  w) 0 bs
  where
    Hash bs = hash a :: Hash SHA256

mine :: Block KV -> Maybe (Block KV)
mine b0 = find (\b -> hash256 b <= tgt)
  [ let GBlock{..} = b0
    in  GBlock{ blockData = blockData { kvNonce = nonce }
              , ..
              }
  | nonce <- [minBound .. maxBound]
  ]
  where
    tgt = blockTarget b0


goBack :: Height -> BH b -> Maybe (BH b)
goBack (Height 0) bh = Just bh
goBack h          bh = goBack (pred h) =<< bhPrevious bh
