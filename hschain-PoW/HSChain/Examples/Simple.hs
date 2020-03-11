{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeFamilies               #-}
-- |
--
module HSChain.Examples.Simple where

import Data.Monoid (Sum(..))
import GHC.Generics (Generic)

import HSChain.Crypto
import HSChain.Crypto.Classes.Hash
import HSChain.Crypto.SHA
import HSChain.Types.Merkle.Types
import HSChain.PoW.Types


----------------------------------------------------------------
--

data KV f = KV
  { kvData :: MerkleNode f SHA256 [(Int,String)]
  }
  deriving stock (Generic)


instance IsMerkle f => CryptoHashable (KV f) where
  hashStep = genericHashStep "hschain"


instance BlockData KV where
  newtype Work    KV = KV'Work Int
    deriving newtype (Eq,Ord)
    deriving         (Semigroup,Monoid) via Sum Int
  newtype BlockID KV = KV'BID (Hash SHA256)
    deriving newtype (Eq,Ord,CryptoHashable)
  --
  blockID b = let Hashed h = hashed b in KV'BID h
  validateHeader _ = True
  blockWork      _ = KV'Work 1
