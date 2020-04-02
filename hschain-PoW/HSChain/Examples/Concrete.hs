{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}
-- |Concrete.hs
--
-- Concrete implementation of a PoW-based blockchain with UTXO.
--

module HSChain.Examples.Concrete where

import qualified Data.Map as Map

--import Data.Monoid (Sum(..))
--import Data.Functor.Classes (Show1)
--import GHC.Generics (Generic)

import HSChain.Crypto
--import HSChain.Crypto.Classes.Hash
--import HSChain.Crypto.SHA
--import HSChain.Types.Merkle.Types


----------------------------------------------------------------
-- We abstract over transactions.

data MerkleTree container alg a =
  Node (Hashed alg a)
       (container (Either
                            (MerkleTree container alg a, MerkleTree container alg a)
                            a
                    ))

-- |Obtain a hash of a node.
getHash :: MerkleTree container alg a -> Hashed alg a
getHash (Node thash _) = thash

instance Eq (Hashed alg a) => Eq (MerkleTree container alg a) where
  a == b = getHash a == getHash b

instance Ord (Hashed alg a) => Ord (MerkleTree container alg a) where
  compare a b = compare (getHash a) (getHash b)

data None a = None
data One a = One a

-- |A tree that knows nothing but the hash of the root.
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

-------------------------------------------------------------------------------
-- Tree (block) database.

-- |A database of trees.
--
-- There are a map from hashes to complete trees and map of incomplete trees
-- to the hashes of their children.
data TreeDB alg a = TreeDB
  { treeDBCompleted     :: Map.Map (MerkleRoot alg a) (CompleteTree alg a)
  , treeDBPartials      :: Map.Map (MerkleRoot alg a) (PartialTree alg a)
  , treeDBWaitsFor      :: Map.Map (MerkleRoot alg a) (PartialTree alg a)
  }


-- |Convert to tree root.
toTreeRoot :: MerkleTree container alg a -> MerkleRoot alg a
toTreeRoot (Node rhash _) = Node rhash None

-- |Add a set of complete trees into database and propagate completeness
-- upward.
addCompleted :: TreeDB alg a -> [CompleteTree alg a] -> TreeDB alg a
addCompleted treeDB@TreeDB{..} newComplete'
  | Map.null unseenCompleteMap = treeDB
  | otherwise = error "TDB"
  where
    newCompleteMap = Map.fromList $ map (\c -> (toTreeRoot c, c)) newComplete'
    unseenCompleteMap = Map.difference newCompleteMap treeDBCompleted

main :: IO ()
main = return ()

