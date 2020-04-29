{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}
-- |
--
module HSChain.Examples.Simple where

import Codec.Serialise      (Serialise)
import Data.Functor.Classes (Show1)
import qualified Data.Aeson as JSON
import GHC.Generics         (Generic)

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
deriving stock instance Show1    f => Show (KV f)
deriving stock instance IsMerkle f => Eq   (KV f)
instance Serialise (KV Identity)
instance Serialise (KV Proxy)


instance IsMerkle f => CryptoHashable (KV f) where
  hashStep = genericHashStep "hschain"

instance MerkleMap KV where
  merkleMap f (KV d) = KV $ mapMerkleNode f d

instance BlockData KV where
  newtype BlockID KV = KV'BID (Hash SHA256)
    deriving newtype (Show,Eq,Ord,CryptoHashable,Serialise, JSON.ToJSON, JSON.FromJSON)

  newtype Solution KV = S'KV Height
    deriving newtype (Show, Eq, Ord, CryptoHashable, Serialise, JSON.ToJSON, JSON.FromJSON)
  --
  blockID b = let Hashed h = hashed b in KV'BID h
  validateHeader _ = True
  validateBlock  _ = True
  blockWork      _ = Work 1
