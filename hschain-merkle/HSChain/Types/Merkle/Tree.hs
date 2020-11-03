{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE DeriveFoldable        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
module HSChain.Types.Merkle.Tree
  ( -- * Type class for Merkle trees
    MerkleTree(..)
    -- * Tree data types
  , MerkleBlockTree(..)
  , createMerkleTree
  , MerkleBlockTree1(..)
  , createMerkleTree1
  , Node(..)
  , MerkleProof(..)
  ) where

import Control.Applicative
import Control.Monad
import Data.Bits
import Data.Function
import GHC.Generics  (Generic)

import HSChain.Crypto
import HSChain.Crypto.Classes.Hash
import HSChain.Types.Merkle.Types


----------------------------------------------------------------
-- Type class
----------------------------------------------------------------

-- | Type class for various Merkle trees. It provides generic API for
--   the proofs of inclusion.
class CryptoHashable a => MerkleTree t a where
  type Proof t :: * -> * -> *
  -- | Obtain root hash of the tree.
  rootHash :: t alg f a -> Hash alg
  -- | Create proof-of-inclusion for element of Merkle tree.
  createMerkleProof :: t alg Identity a -> a -> Maybe (Proof t alg a)
  -- | Verify that proof is indeed correct.
  verifyMerkleProof :: CryptoHash alg => t alg f a -> Proof t alg a -> Bool
  -- | Check that Merkle tree satisfy internal invariants
  checkMerkleInvariants :: t alg Identity a -> Bool


----------------------------------------------------------------
-- Concrete trees
----------------------------------------------------------------

-- | Balanced binary Merkle tree.
newtype MerkleBlockTree alg f a = MerkleBlockTree
  { merkleBlockTree :: MerkleNode f alg (Maybe (Node alg f a))
  }
  deriving (Show, Foldable, Generic)

-- | Nonempty balanced binary Merkle tree.
newtype MerkleBlockTree1 alg f a = MerkleBlockTree1
  { merkleBlockTree1 :: MerkleNode f alg (Node alg f a)
  }
  deriving (Show, Foldable, Generic)

-- | Single node of tree
data Node alg f a
  = Branch (MerkleNode f alg (Node alg f a))
           (MerkleNode f alg (Node alg f a))
  | Leaf   !a
  deriving (Show, Foldable, Generic)

-- | Compact proof of inclusion of value in Merkle tree.
data MerkleProof alg a = MerkleProof
  { merkleProofLeaf :: !a
  , merkleProofPath :: [Either (Hash alg) (Hash alg)]
  }
  deriving (Show, Eq, Generic)

instance (CryptoHashable a, Eq a) => MerkleTree MerkleBlockTree a where
  type Proof MerkleBlockTree = MerkleProof
  rootHash = merkleHash . merkleBlockTree
  --
  createMerkleProof (MerkleBlockTree mtree) a = do
    path <- searchBinTree a =<< merkleValue mtree
    return $ MerkleProof a path
  --
  verifyMerkleProof t p = rootHash t == hash (Just (calcRootNode p))
  --
  checkMerkleInvariants = maybe True isBalanced . merkleValue . merkleBlockTree


instance (CryptoHashable a, Eq a) => MerkleTree MerkleBlockTree1 a where
  type Proof MerkleBlockTree1 = MerkleProof
  rootHash = merkleHash . merkleBlockTree1
  --
  createMerkleProof (MerkleBlockTree1 mtree) a = do
    path <- searchBinTree a $ merkleValue mtree
    return $ MerkleProof a path
  --
  verifyMerkleProof t p = rootHash t == hash (calcRootNode p)
  --
  checkMerkleInvariants = isBalanced . merkleValue . merkleBlockTree1



instance (CryptoHash alg) => CryptoHashable (MerkleBlockTree alg f a) where
  hashStep = hashStep . merkleBlockTree

instance (CryptoHash alg) => CryptoHashable (MerkleBlockTree1 alg f a) where
  hashStep = hashStep . merkleBlockTree1

instance (CryptoHash alg, CryptoHashable a) => CryptoHashable (Node alg f a) where
  hashStep node
    = hashStep (UserType "hschain" "Merkle.Tree.Node")
   <> case node of
        Branch a b -> hashStep (ConstructorIdx 0)
                   <> hashStep a
                   <> hashStep b
        Leaf   a   -> hashStep (ConstructorIdx 1)
                   <> hashStep a



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
    pair (x:y:xs) = f x y : pair xs
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
  :: (CryptoHash alg, CryptoHashable a, IsMerkle f)
  => [a]                        -- ^ Leaves of tree
  -> MerkleBlockTree alg f a
createMerkleTree leaves
  = MerkleBlockTree
  $ merkled
  $ case leaves of
      [] -> Nothing
      _  -> Just $ buildMerkleTree (Branch `on` merkled) $ Leaf <$> leaves

-- | Create perfectly balanced Merkle tree. In order to make
--   construction deterministic (there are many perfectly balanced
--   binary trees is number of leaves is not power of two) depth of
--   leaves is nonincreasing.
createMerkleTree1
  :: (CryptoHash alg, CryptoHashable a, IsMerkle f)
  => [a]                        -- ^ Leaves of tree
  -> Maybe (MerkleBlockTree1 alg f a)
createMerkleTree1 []     = Nothing
createMerkleTree1 leaves = Just
  $ MerkleBlockTree1
  $ merkled
  $ buildMerkleTree (Branch `on` merkled) $ Leaf <$> leaves


----------------------------------------------------------------
-- Merkle Proof
----------------------------------------------------------------

-- | Check proof of inclusion
calcRootNode
  :: forall alg a. (CryptoHashable a, CryptoHash alg)
  => MerkleProof alg a -- ^ Proof
  -> Node alg Proxy a
calcRootNode (MerkleProof a path)
  = foldr step (Leaf a) path
  where
    step :: Either (Hash alg) (Hash alg) -> Node alg Proxy a -> Node alg Proxy a
    step (Left  g) h = Branch (fromHashed (hashed h)) (fromHashed $ Hashed g)
    step (Right g) h = Branch (fromHashed (Hashed g)) (fromHashed $ hashed h)

searchBinTree
  :: (Eq a)
  => a -> Node alg Identity a -> Maybe [Either (Hash alg) (Hash alg)]
searchBinTree a = go
  where
    go (Leaf   b)   = [] <$ guard (a == b)
    go (Branch b c) =  (Left  (merkleHash c):) <$> go (merkleValue b)
                   <|> (Right (merkleHash b):) <$> go (merkleValue c)



----------------------------------------------------------------
-- Balance checker
----------------------------------------------------------------


-- | Check whether Merkle tree is balanced and in canonical form:
--   depth of leaves does not decrease.
isBalanced :: Node alg Identity a -> Bool
isBalanced tree
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
    calcDepth !n Leaf{}       = (n :)
    calcDepth !n (Branch a b) = calcDepth (n+1) (merkleValue a)
                              . calcDepth (n+1) (merkleValue b)
