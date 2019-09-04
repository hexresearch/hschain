{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE DeriveFoldable        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
-- |
module HSChain.Types.MerkleBlock
  ( -- * Tree data types
    MerkleBlockTree(..)
    -- * Building tree
  , createMerkleTree
  , computeMerkleRoot
    -- * Check tree
  , isBalanced
  , isConsistent
    -- * Merkle proof
  , MerkleProof(..)
  , checkMerkleProof
  , createMerkleProof
  ) where

import Codec.Serialise
import Control.Applicative
import Control.Monad
import Data.Bits
import GHC.Generics  (Generic)

import HSChain.Crypto
import HSChain.Types.Merklized


----------------------------------------------------------------
-- Data types
----------------------------------------------------------------

-- | Binary Merkle tree. Constructors in this module generate
--   perfectly balanced binary tree.
data MerkleBlockTree alg a = MerkleBlockTree
  { merkleBlockRoot :: !(Hash alg)
  -- ^ Hash of complete tree
  , merkleBlockTree :: !(Maybe (Node alg a))
  -- ^ Tree itself
  }
  deriving (Show, Foldable, Generic)

-- | Single node of tree
data Node alg a
  = Branch (Hash alg) (Node alg a) (Node alg a)
  | Leaf   (Hash alg) a
  deriving (Show, Foldable, Generic)


instance (CryptoHash alg, alg ~ alg') => MerkleValue alg' (MerkleBlockTree alg a) where
  merkleHash = merkleBlockRoot

instance (CryptoHash alg, alg ~ alg') => MerkleValue alg' (Node alg a) where
  merkleHash (Leaf h _)     = h
  merkleHash (Branch h _ _) = h


----------------------------------------------------------------
-- Build tree, compute rooth hash
----------------------------------------------------------------

-- Utility function to process list of items as balanced tree
buildMerkleTree :: (a -> a -> a) -> [a] -> a
buildMerkleTree f leaves = balance $ preBalance nPairs leaves
  where
    -- In order to build a perfectly balanced tree we need to know
    -- numbrer of leaves
    size = length leaves
    p    = nextPow2 size
    -- Number of pairs of nodes at the depth p. Rest of nodes will be
    -- at the depth p-1
    nPairs = (2*size - p) `div` 2
    -- Group leaves which will appear at maximum depth
    preBalance 0 xs       = xs
    preBalance n (x:y:xs) = f x y : preBalance (n - 1) xs
    preBalance _ _        = err "internal error in preBalance"
    -- Now tree is perfectly balanced we can use simple recursive
    -- algorithm to balance it perfectly
    pair (x:y:xs) =  f x y : pair xs
    pair []       = []
    pair [_]      = err "Odd-length list passed to pair algorithm"
    --
    balance []  = err "Empty list passed to balance"
    balance [x] = x
    balance xs  = balance $ pair xs
    --
    err s = error $ "HSChain.Types.MerkleBlock.buildMerkleTree: " ++ s

-- Find smallest power of 2 larger than number
nextPow2 :: Int -> Int
nextPow2 n = 1 `shiftL` (finiteBitSize n - countLeadingZeros n)

-- | Create perfectly balanced Merkle tree. In order to make
--   construction deterministic (there are many perfectly balanced
--   binary trees is number of leaves is not power of two) depth of
--   leaves is nonincreasing.
createMerkleTree
  :: (CryptoHash alg, Serialise a)
  => [a]                        -- ^ Leaves of tree
  -> MerkleBlockTree alg a
createMerkleTree leaves = MerkleBlockTree
  { merkleBlockRoot = merkleHash tree
  , merkleBlockTree = tree
  }
  where
    tree = case leaves of
      [] -> Nothing
      _  -> Just $ buildMerkleTree mkBranch $ mkLeaf <$> leaves

-- | Calculate Merkle root of given sequence without constructin
--   complete tree.
computeMerkleRoot
  :: forall alg a. (CryptoHash alg, Serialise a)
  => [a]
  -> Hash alg
computeMerkleRoot leaves
  = (hash :: Maybe (Hash alg) -> Hash alg)
  $ case leaves of
      [] -> Nothing
      _  -> Just $ buildMerkleTree concatHash $ map computeLeafHash leaves

mkLeaf :: (CryptoHash alg, Serialise a) => a -> Node alg a
mkLeaf x = Leaf (computeLeafHash x) x

mkBranch :: (CryptoHash alg, Serialise a) => Node alg a -> Node alg a -> Node alg a
mkBranch x y = Branch (concatHash (merkleHash x) (merkleHash y)) x y

computeLeafHash :: (CryptoHash alg, Serialise a) => a -> Hash alg
computeLeafHash a = hash (0::Int, a)

concatHash :: (CryptoHash alg) => Hash alg -> Hash alg -> Hash alg
concatHash x y = hash (1::Int, x, y)



----------------------------------------------------------------
-- Merkle Proof
----------------------------------------------------------------

-- | Compact proof of inclusion of value in Merkle tree.
data MerkleProof alg a = MerkleProof
  { merkleProofLeaf :: !a
  , merkleProofPath :: [Either (Hash alg) (Hash alg)]
  }
  deriving (Show, Eq, Generic)

-- | Check proof of inclusion
checkMerkleProof
  :: (Serialise a, CryptoHash alg)
  => Hash alg          -- ^ Root hash of Merkle Tree
  -> MerkleProof alg a -- ^ Proof
  -> Bool
checkMerkleProof rootH (MerkleProof a path)
  = rootH == hash (Just (foldr step (computeLeafHash a) path))
  where
    step (Left  g) h = concatHash h g
    step (Right g) h = concatHash g h


-- | Create proof of inclusion. Implementation is rather inefficient
createMerkleProof
  :: (CryptoHash alg, Serialise a, Eq a)
  => MerkleBlockTree alg a
  -> a
  -> Maybe (MerkleProof alg a)
createMerkleProof (MerkleBlockTree _ mtree) a = do
  path <- search =<< mtree
  return $ MerkleProof a path
  where
    search (Leaf   _ b)   = [] <$ guard (a == b)
    search (Branch _ b c) =  (Left  (merkleHash c):) <$> search b
                         <|> (Right (merkleHash b):) <$> search c



----------------------------------------------------------------
-- Balance checker
----------------------------------------------------------------

-- | Check whether Merkle tree is balanced and in canonical form:
--   depth of leaves does not decrease.
isBalanced :: MerkleBlockTree alg a -> Bool
isBalanced (MerkleBlockTree _ Nothing)     = True
isBalanced (MerkleBlockTree _ (Just tree))
  = isCanonical $ calcDepth (0::Int) tree []
  where
    -- Check that node depths are nonincreasing and don't differ more
    -- that 1 from first one (tree is balanced)
    isCanonical []      = True
    isCanonical (n0:xs) = go xs
      where
        ok x = x <= n0 && (n0-x) <= 1
        go []       = True
        go [x]      = ok x
        go (x:y:ys) = ok x && x >= y && go (y:ys)
    -- Build list of node depths using diff lists
    calcDepth !n Leaf{}         = (n :)
    calcDepth !n (Branch _ a b) = calcDepth (n+1) a . calcDepth (n+1) b

-- | Check whether all hashes in the tree are consistent
isConsistent
  :: forall alg a. (Serialise a, CryptoHash alg)
  => MerkleBlockTree alg a -> Bool
isConsistent (MerkleBlockTree rootH tree) =
  case tree of
    Nothing -> rootH == hash (Nothing :: Maybe (Hash alg))
    Just tr -> case check tr of
      Nothing  -> False
      h@Just{} -> rootH == hash h
  where
    check (Leaf   h a)   = do guard $ h == computeLeafHash a
                              return h
    check (Branch h a b) = do guard $ h == concatHash (merkleHash a) (merkleHash b)
                              return h
