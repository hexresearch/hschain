{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}
-- |
--
module HSChain.Examples.Simple where

import Data.Monoid (Sum(..))
import Data.Functor.Classes (Show1)
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
deriving stock instance Show1 (f SHA256) => Show (KV f)

instance IsMerkle f => CryptoHashable (KV f) where
  hashStep = genericHashStep "hschain"

instance MerkleMap KV where
  merkleMap f (KV (MerkleNode a)) = KV (MerkleNode (f a))

instance BlockData KV where
  newtype Work    KV = KV'Work Int
    deriving newtype (Eq,Ord,Show)
    deriving         (Semigroup,Monoid) via Sum Int
  newtype BlockID KV = KV'BID (Hash SHA256)
    deriving newtype (Show,Eq,Ord,CryptoHashable)
  --
  blockID b = let Hashed h = hashed b in KV'BID h
  validateHeader _ = True
  validateBlock  _ = True
  blockWork      _ = KV'Work 1
