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
  , treeLeaves
  -- * Check tree
  , isBalanced
  , isBalanced'
  -- * Merkle proof
  , merklePath
  , merkleProof
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
  , merkleBlockTree :: !(Maybe (Node alg a))
  }
  deriving (Show, Generic)

-- | Single node of tree
data Node alg a
  = Branch (Hash alg) (Node alg a) (Node alg a)
  | Leaf   (Hash alg) a
  deriving (Show, Generic)



instance Foldable (Node alg) where
  foldr f acc (Leaf _ x)     = f x acc
  foldr f acc (Branch _ l r) = foldr f (foldr f acc r) l

  foldMap f x = case x of
    Leaf _ a     -> f a
    Branch _ l r -> foldMap f l `mappend` foldMap f r

nullHash :: CryptoHash alg => Hash alg
nullHash = hash (2:: Int)


nullRoot :: CryptoHash alg => MerkleBlockRoot alg
nullRoot  = MerkleBlockRoot nullHash


----------------------------------------------------------------
-- Build tree, compute rooth hash
----------------------------------------------------------------


createMerkleTree
  :: forall alg a.  (CryptoHash alg, Serialise a)
  => [a]
  -> MerkleBlockTree alg a
createMerkleTree []     = MerkleBlockTree nullRoot Nothing
createMerkleTree tx = let leaves :: [Node alg a]= map  (\x -> Leaf ( computeLeafHash x) x) tx
                          tree = go leaves
                      in MerkleBlockTree (MerkleBlockRoot (getHash tree)) (Just tree)
  where
    go [b] = b
    go hs  = go (combine hs)

    -- combine hashes bye pair and get hashes of next level of tree
    combine []         = []
    combine [x]        = [x]
    combine (l:r:rest) = Branch (hash (1::Int, [getHash l, getHash r])) l r : (combine rest)


-- | calculate Merkle root of given sequence
-- to calculate merkle root hash we do not need explicit tree data structure
computeMerkleRoot
  :: (CryptoHash alg, Serialise a)
  => [a]
  -> Hash alg
computeMerkleRoot []     = nullHash
computeMerkleRoot tx = let hashes = map computeLeafHash tx
                       in go hashes
  where
   go [h] = h
   go hs  = go (combine hs)


   combine []           = []
   combine [x]          = [x]
   combine (n1:n2:rest) = (hash (1::Int, [n1, n2])) : (combine rest)


computeLeafHash
  :: (CryptoHash alg, Serialise a)
  => a
  -> Hash alg
computeLeafHash a = hash (0::Int, a)

treeLeaves :: (CryptoHash alg, Serialise a)  => Node alg a -> [a]
treeLeaves (Leaf _ x)     = [x]
treeLeaves (Branch _ l r) = treeLeaves l ++ treeLeaves r

----------------------------------------------------------------
-- Merkel Proof
----------------------------------------------------------------

-- |
-- return the path from leaf to root of branch if any
merklePath :: (CryptoHash alg, Serialise a) => Maybe (Node alg a) -> Hash alg -> [Either (Hash alg) (Hash alg)]
merklePath Nothing _ = []
merklePath (Just tree) nodeHash = recur [] tree
  where
    recur acc (Leaf h _)
        | h == nodeHash = acc
        | otherwise = []

    recur acc (Branch _ l r) = recur (Right (getHash r) : acc) l ++ recur (Left (getHash l) : acc) r

getHash :: Node alg a -> Hash alg
getHash (Leaf h _)     = h
getHash (Branch h _ _) = h


merkleProof :: CryptoHash alg => Hash alg -> [Either (Hash alg) (Hash alg)] -> Hash alg -> (Hash alg, Bool)
merkleProof rootHash proofPath leafHash = (computeHash, computeHash == rootHash)
  where
    computeHash = foldl (\acc x -> case x of
                                   Left s  ->  hash (1::Int, [s, acc])
                                   Right s ->  hash (1::Int, [acc, s])) leafHash proofPath



----------------------------------------------------------------
-- Balance checker
----------------------------------------------------------------
-- |
-- optimal algorithm
isBalanced
  :: forall alg a. (CryptoHash alg, Serialise a)
  => Maybe (Node alg a)
  -> Bool
isBalanced Nothing = True
isBalanced (Just tree) | go tree > 0 = True
                       | otherwise = False
  where
   go :: Node alg a -> Int
   go (Leaf {}) = 0
   go (Branch _ l r) | lH == (-1) = (-1)
                     | rH == (-1) = (-1)
                     | abs(lH - rH) > 1 = (-1)
                     | otherwise = 1 + (max lH rH)
       where
         lH = go l
         rH = go r

-- |
-- more intuitive way to check
isBalanced'
  :: forall alg a. (CryptoHash alg, Serialise a)
  => Maybe (Node alg a)
  -> Bool
isBalanced' Nothing          = True
isBalanced' (Just tree)      = snd $ go tree
  where
    go :: Node alg a -> (Int, Bool)
    go (Leaf {})   = (1,True)
    go (Branch _ l r) = let (lH, lB) = go l
                            (rH, rB) = go r
                        in (1 + max lH rH, abs (lH - rH) <= 1 && lB && rB)



