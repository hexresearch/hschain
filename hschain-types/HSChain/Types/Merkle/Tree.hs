{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE DeriveFoldable        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE ViewPatterns          #-}
module HSChain.Types.Merkle.Tree
  ( -- * Tree data types
    MerkleBlockTree(..)
    -- * Building tree
  , createMerkleTree
    -- * Check tree
  , isBalanced
    -- * Merkle proof
  , MerkleProof(..)
  , checkMerkleProof
  , createMerkleProof
  ) where

import Control.Applicative
import Control.Category ((>>>))
import Control.Monad
import Data.Bits
import Data.Function
import GHC.Generics  (Generic)

import HSChain.Crypto
import HSChain.Crypto.Classes.Hash
import HSChain.Types.Merkle.Types


----------------------------------------------------------------
-- Data types
----------------------------------------------------------------

-- | Balanced binary Merkle tree. Constructors in this module generate
--   balanced binary tree.
newtype MerkleBlockTree f alg a = MerkleBlockTree
  { merkleBlockTree :: MerkleNode f alg (Maybe (Node f alg a))
  }
  deriving (Show, Foldable, Generic)

-- | Single node of tree
data Node f alg a
  = Branch (MerkleNode f alg (Node f alg a))
           (MerkleNode f alg (Node f alg a))
  | Leaf   !a
  deriving (Show, Foldable, Generic)


instance MerkleHash f => MerkleHash (MerkleBlockTree f) where
  merkleHash = merkleHash . merkleBlockTree

instance (IsMerkle f, CryptoHash alg) => CryptoHashable (MerkleBlockTree f alg a) where
  hashStep = hashStep . merkleBlockTree

instance (IsMerkle f, CryptoHash alg, CryptoHashable a) => CryptoHashable (Node f alg a) where
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
  -> MerkleBlockTree f alg a
createMerkleTree leaves
  = MerkleBlockTree
  $ merkled
  $ case leaves of
      [] -> Nothing
      _  -> Just $ buildMerkleTree (Branch `on` merkled) $ Leaf <$> leaves


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
  :: forall alg a. (CryptoHashable a, CryptoHash alg)
  => Hash alg          -- ^ Root hash of Merkle Tree
  -> MerkleProof alg a -- ^ Proof
  -> Bool
checkMerkleProof rootH (MerkleProof a path)
  = rootH == calcH
  where
    calcH = hash
          $ Just
          $ foldr step (Leaf a) path
    step :: Either (Hash alg) (Hash alg) -> Node Hashed alg a -> Node Hashed alg a
    step (Left  g) h = Branch (MerkleNode $ hashed h) (MerkleNode $ Hashed g)
    step (Right g) h = Branch (MerkleNode $ Hashed g) (MerkleNode $ hashed h)


-- | Create proof of inclusion. Implementation is rather inefficient
createMerkleProof
  :: (CryptoHash alg, CryptoHashable a, Eq a)
  => MerkleBlockTree IdNode alg a
  -> a
  -> Maybe (MerkleProof alg a)
createMerkleProof (MerkleBlockTree mtree) a = do
  path <- search =<< merkleValue mtree
  return $ MerkleProof a path
  where
    search (Leaf   b)   = [] <$ guard (a == b)
    search (Branch b c) =  (Left  (merkleHash c):) <$> search (merkleValue b)
                       <|> (Right (merkleHash b):) <$> search (merkleValue c)



----------------------------------------------------------------
-- Balance checker
----------------------------------------------------------------


-- | Check whether Merkle tree is balanced and in canonical form:
--   depth of leaves does not decrease.
isBalanced :: MerkleBlockTree IdNode alg a -> Bool
isBalanced =
  merkleBlockTree >>> merkleValue >>> \case
    Nothing   -> True
    Just tree -> isCanonical $ calcDepth (0::Int) tree []
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
