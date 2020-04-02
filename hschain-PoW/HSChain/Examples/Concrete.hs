{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}
-- |Concrete.hs
--
-- Concrete implementation of a PoW-based blockchain with UTXO.
--

module HSChain.Examples.Concrete where

import Control.Applicative

import Data.Monoid (Sum(..))
import Data.Functor.Classes (Show1)
import GHC.Generics (Generic)

import HSChain.Crypto
import HSChain.Crypto.Classes.Hash
import HSChain.Crypto.SHA
import HSChain.Types.Merkle.Types


----------------------------------------------------------------
-- We abstract over transactions.

data KV f = KV
  { kvData :: MerkleNode f SHA256 [(Int,String)]
  }
  deriving stock (Generic)
deriving stock instance Show1 (f SHA256) => Show (KV f)

data MerkleTree container alg a =
  Node (Hashed alg a)
       (container (Either
                            (MerkleTree container alg a, MerkleTree container alg a)
                            a
                    ))

data None a = None
data One a = One a

-- |A tree that knows nothing but hash of the root.
type MerkleRoot alg a = MerkleTree None alg a

-- |A partially complete tree.
type PartialTree alg a = MerkleTree Maybe alg a

-- |A completely built tree.
type CompleteTree alg a = MerkleTree One alg a

-- |A conversion function from partially built tree into one that is complete.
toCompleteTree :: PartialTree alg a -> Maybe (CompleteTree alg a)
toCompleteTree (Node nhash (Just (Left (l, r)))) =
  (\l' r' -> Node nhash $ One $ Left (l', r')) <$> toCompleteTree l <*> toCompleteTree r
toCompleteTree (Node nhash (Just (Right a))) =
  Just $ Node nhash $ One $ Right $ a
toCompleteTree (Node _hash Nothing) = Nothing

main :: IO ()
main = return ()

