{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}
-- |
-- Simple block which implements write only key-value storage. It's
-- only use is to test and debug PoW algorithms.
module HSChain.Examples.Simple
  ( KV(..)
  , KVConfig(..)
  , retarget
  , mine
  ) where

import Codec.Serialise      (Serialise)
import Control.Applicative
import Control.Monad
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


----------------------------------------------------------------
--
----------------------------------------------------------------

-- | Simple block which contains key-value pairs. Work function is
--   simple SHA256 a la bitcoin
data KV cfg f = KV
  { kvData       :: !(MerkleNode f SHA256 [(Int,String)])
  -- ^ List of key-value pairs
  , kvNonce      :: !Word32
  -- ^ Nonce which is used to get
  , kvDifficulty :: !Natural
  -- ^ Current difficulty of mining. It means that
  --   @SHA256(block) < 2^256 / kvDifficulty@
  }
  deriving stock (Generic)
deriving stock instance Show1    f => Show (KV cfg f)
deriving stock instance IsMerkle f => Eq   (KV cfg f)
instance Serialise (KV cfg Identity)
instance Serialise (KV cfg Proxy)

instance IsMerkle f => CryptoHashable (KV cfg f) where
  hashStep = genericHashStep "hschain"

instance MerkleMap (KV cfg) where
  merkleMap f KV{..} = KV { kvData = mapMerkleNode f kvData
                          , ..
                          }


-- | We may need multiple chains (main chain, test chain(s)) which may
--   use different difficulty adjustment algorithms etc.
class KVConfig cfg where
  -- | Difficulty adjustment is performed every N of blocks
  kvAdjustInterval :: Const Height  cfg
  -- | Expected interval between blocks in milliseconds
  kvBlockInterval  :: Const Natural cfg


instance KVConfig cfg => BlockData (KV cfg) where
  newtype BlockID (KV cfg) = KV'BID (Hash SHA256)
    deriving newtype (Show,Eq,Ord,CryptoHashable,Serialise, JSON.ToJSON, JSON.FromJSON)
  --
  blockID = KV'BID . hash
  --
  validateHeader bh (Time now) header
    = return
    $ and
    [ hash256 header <= blockTarget header
    , kvDifficulty (blockData header) == retarget bh
    -- Time checks
    , t <= now + (2*60*60*1000)
    -- FIXME: Check that we're ahead of median time of N prev block
    ]
    where
      Time t = blockTime header
  --
  validateBlock  _ = return True
  blockWork      b = Work $ kvDifficulty $ blockData b


blockTarget :: GBlock (KV cfg) f -> Natural
blockTarget b = 2^(256::Int) `div` kvDifficulty (blockData b)

-- | Compute difficulty of next block
retarget :: forall cfg. KVConfig cfg => BH (KV cfg) -> Natural
retarget bh
  | bhHeight bh `mod` interval == 0
  , Just old <- goBack interval bh
  , bhHeight old /= 0
  =   let Time t1 = bhTime old
          Time t2 = bhTime bh
          tgt     = 2^(256::Int) `div` kvDifficulty (bhData bh)
          tgt'    = (tgt * fromIntegral (t2 - t1)) `div` (fromIntegral interval * delay)
      in 2^(256::Int) `div` tgt'
  | otherwise
    = kvDifficulty $ bhData bh
  where
    interval = getConst (kvAdjustInterval @cfg)
    delay    = getConst (kvBlockInterval  @cfg)

-- SHA256 as 256-bit number (not terrbly efficient)
hash256 :: CryptoHashable a => a -> Natural
hash256 a
  = BS.foldl' (\i w -> (i `shiftL` 8) + fromIntegral  w) 0 bs
  where
    Hash bs = hash a :: Hash SHA256

-- | Mine new block. Not very efficicent but will do for debugging
mine :: Block (KV cfg) -> Maybe (Block (KV cfg))
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
goBack (Height 0) = Just
goBack h          = goBack (pred h) <=< bhPrevious
