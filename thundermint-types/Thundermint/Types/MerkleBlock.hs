{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
module Thundermint.Types.MerkleBlock
  (
    -- * Tree data types
    MerkleBlockRoot(..)
  , MerkleBlockTree(..)
  -- * Root hash
  , computeMerkleRoot
  -- * Building tree
  , createMerkleTree
  ) where


import Codec.Serialise
import GHC.Generics    (Generic)

import Thundermint.Crypto

----------------------------------------------------------------
-- Data types
----------------------------------------------------------------

data MerkleBlockRoot alg = MerkleBlockRoot
  { rootHash :: !(Hash alg) }
  deriving (Show, Eq, Generic)
instance Serialise (MerkleBlockRoot alg)

-- | Complete tree.
--
data MerkleBlockTree alg a = MerkleBlockTree
  { merkleBlockRoot :: !(MerkleBlockRoot alg)
  , merkleBlockTree :: !(Node alg a)
  }
  deriving (Show, Generic)

-- | Single node of tree
data Node alg a
  = Branch (Hash alg) (Node alg a) (Node alg a)
  | Leaf   (Hash alg)
  | Empty
  deriving (Show, Generic)

{-
data MerklePath alg a
  = R (Hash alg) (MerklePath alg a)
  | L (MerklePath alg a) (Hash alg)
  | Init (Hash alg)
-}


nullHash :: CryptoHash alg => Hash alg
nullHash = Hash "0"


nullRoot :: CryptoHash alg => MerkleBlockRoot alg
nullRoot  = MerkleBlockRoot nullHash

-- | Create Merkle tree
-- similar to compute merkle root hash, but use intermediate calculatio of hashes to construct merkle tree
createMerkleTree
  :: forall alg a.  (CryptoHash alg, Serialise a)
  => [a]
  -> MerkleBlockTree alg a
createMerkleTree []     = MerkleBlockTree nullRoot Empty
createMerkleTree tx = let dtx = (duplicateLast tx)
                          leaves :: [Node alg a]= map  (Leaf . computeLeafHash) dtx
                          tree@(Branch h _ _) = go leaves
                      in MerkleBlockTree (MerkleBlockRoot h) tree
  where
    go [b] = b
    go hs  = go (combine hs)

    -- If number of hashes is odd, duplicate last hash in the list.
    -- this last function is O(n) we should go with
    -- Data.Sequence which supports O(1) insertion and removal of items at both ends.
    -- this procedure for the list with duplicate elements will resulting in a vulnerability (CVE-2012-2459).
    duplicateLast xs | odd (length xs) = xs ++ [last xs]
                    | otherwise = xs

    -- combine hashes bye pair and get hashes of next level of tree
    combine hs = map concatNodes (chunksOf2 hs)

    concatNodes [l@(Leaf a), r@(Leaf b)]             = Branch (hash (1::Int, [a,b])) l r
    concatNodes [l@(Branch a _ _), r@(Branch b _ _)] = Branch (hash (1::Int, [a,b])) l r
    concatNodes [b@(Branch a _ _)]                   = Branch (hash (1::Int, [a,a])) b b
    concatNodes []                                   = error "The chunk of 2 of non empty has empty item"
    concatNodes _                                    = error "impossible case in createMerkletree for block' transactions"



-- | calculate Merkle root of given sequence
-- to calculate merkle root hash we do not need explicit tree data structure
computeMerkleRoot
  :: (CryptoHash alg, Serialise a)
  => [a]
  -> Hash alg
computeMerkleRoot []     = nullHash -- Here we can put (Hash "0") and remove Maybe
computeMerkleRoot tx = let dtx = (duplicateLast tx)
                           hashes = map computeLeafHash dtx
                       in go hashes
  where
   go [h] = h
   go hs  = go (combine hs)

   -- If number of hashes is odd, duplicate last hash in the list.
   -- this last function is O(n) we should go with
   -- Data.Sequence which supports O(1) insertion and removal of items at both ends.
   -- this procedure for the list with duplicate elements will resulting in a vulnerability
   duplicateLast xs | odd (length xs) = xs ++ [last xs]
                    | otherwise = xs

   -- combine hashes bye pair and get hashes of next level of tree
   combine hs = map (\case
                          [a,b] -> hash (1::Int, [a,b])
                          [a]   -> hash (1::Int, [a,a]) -- cheap way to duplicate If number of elements is odd
                          _     -> error "The chunk of 2 of non empty has empty item"
                     )
                    (chunksOf2 hs)


-- |
-- chunks of 2
chunksOf2 :: [a] -> [[a]]
chunksOf2 xs = case splitAt 2 xs of
  (c,[]) -> [c]
  (c,cs) -> c : chunksOf2 cs


computeLeafHash
  :: (CryptoHash alg, Serialise a)
  => a
  -> Hash alg
computeLeafHash a = hash (0::Int, a)

