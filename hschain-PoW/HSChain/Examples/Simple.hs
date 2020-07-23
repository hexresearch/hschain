{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FlexibleContexts           #-}
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
  , hash256AsTarget
  ) where

import Codec.Serialise      (Serialise)
import Control.Monad.IO.Class
import Control.Applicative
import Data.Functor.Classes (Show1)
import qualified Data.Aeson           as JSON
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
  , kvTarget     :: !Target
  -- ^ Nonce which is used to get
  , kvNonce      :: !(Nonce cfg)
  -- ^ Current difficulty of mining. It means a complicated thing
  -- right now.
  }
  deriving stock (Generic)
deriving stock instance (Show (Nonce cfg), Show1 f)  => Show (KV cfg f)
deriving stock instance (Eq (Nonce cfg), IsMerkle f) => Eq   (KV cfg f)
instance Serialise (Nonce cfg) => Serialise (KV cfg Identity)
instance Serialise (Nonce cfg) => Serialise (KV cfg Proxy)

instance (CryptoHashable (Nonce cfg), IsMerkle f) => CryptoHashable (KV cfg f) where
  hashStep = genericHashStep "hschain"

instance MerkleMap (KV cfg) where
  merkleMap f KV{..} = KV { kvData = mapMerkleNode f kvData
                          , ..
                          }

-- | We may need multiple chains (main chain, test chain(s)) which may
--   use different difficulty adjustment algorithms etc.
class ( CryptoHashable (Nonce cfg)
      , Serialise (Nonce cfg)) => KVConfig cfg where
  -- | Type of nonce. It depends on configuration.
  type Nonce cfg

  -- | Difficulty adjustment is performed every N of blocks
  kvAdjustInterval :: Const Height  cfg
  -- | Expected interval between blocks in milliseconds
  kvBlockTimeInterval  :: Const Time cfg

  -- |How to compute a solved puzzle. May fail.
  kvSolvePuzzle :: MonadIO m => Block (KV cfg) -> m (Maybe (Block (KV cfg)))

  -- |How to check solution of a puzzle.
  kvCheckPuzzle :: MonadIO m => Header (KV cfg) -> m Bool

instance KVConfig cfg => BlockData (KV cfg) where
  newtype BlockID (KV cfg) = KV'BID (Hash SHA256)
    deriving newtype (Show,Eq,Ord,CryptoHashable,Serialise, JSON.ToJSON, JSON.FromJSON)

  type Tx (KV cfg) = (Int, String)
  blockID b = let Hashed h = hashed b in KV'BID h
  validateHeader bh (Time now) header
    | blockHeight header == 0 = return True -- skip genesis check.
    | otherwise = do
      answerIsGood <- kvCheckPuzzle header
      return
        $ and
              [ answerIsGood
              , kvTarget (blockData header) == retarget bh
              -- Time checks
              , t <= now + (2*60*60*1000)
              -- FIXME: Check that we're ahead of median time of N prev block
              ]
    where
      Time t = blockTime header
  --
  validateBlock  _ = return True
  blockWork      b = Work $ fromIntegral $ ((2^(256 :: Int)) `div`)
                          $ targetInteger $ kvTarget $ blockData b
  blockTargetThreshold b = Target $ targetInteger (kvTarget (blockData b))
  targetAdjustmentInfo (_ :: BH (KV cfg)) = (adjustInterval, blockMineTime)
    where
      Const adjustInterval = kvAdjustInterval :: Const Height cfg
      Const blockMineTime = kvBlockTimeInterval :: Const Time cfg

instance KVConfig cfg => Mineable (KV cfg) where
  adjustPuzzle = fmap (flip (,) (Target 0)) . kvSolvePuzzle

