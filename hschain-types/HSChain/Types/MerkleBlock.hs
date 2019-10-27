{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE DeriveFoldable        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
-- |
module HSChain.Types.MerkleBlock where
  -- ( -- * Tree data types
  --   MerkleBlockTree(..)
  --   -- * Building tree
  -- , createMerkleTree
  -- , computeMerkleRoot
  --   -- * Check tree
  -- , isBalanced
  -- , isConsistent
  --   -- * Merkle proof
  -- , MerkleProof(..)
  -- , checkMerkleProof
  -- , createMerkleProof
  -- ) where

import Codec.Serialise
import Control.Applicative
import Control.Monad
import Data.Bits
import Data.Function
import GHC.Generics  (Generic)

import HSChain.Crypto
import HSChain.Types.Merklized


----------------------------------------------------------------
-- Data types
----------------------------------------------------------------

-- | Binary Merkle tree. Constructors in this module generate
--   perfectly balanced binary tree.
newtype MerkleBlockTree alg a = MerkleBlockTree
  { merkleBlockTree :: Merkled alg (Maybe (Node alg a))
  }
  deriving (Show, Foldable, Generic)

-- | Single node of tree
data Node alg a
  = Branch (Merkled alg (Node alg a)) (Merkled alg (Node alg a))
  | Leaf   !a
  deriving (Show, Foldable, Generic)

-- instance CryptoHashable (MerkleBlockTree alg a) where
--   hashStep s = hashStep s . merkleBlockRoot

instance CryptoHashable a => CryptoHashable (Node alg a) where
  hashStep s node = do
    hashStep s $ UserType "Node"
    case node of
      Branch a b -> do hashStep s $ ConstructorIdx 0
                       hashStep s   a
                       hashStep s   b
      Leaf   a   -> do hashStep s $ ConstructorIdx 1
                       hashStep s   a
                           


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
  :: (CryptoHash alg, CryptoHashable a)
  => [a]                        -- ^ Leaves of tree
  -> MerkleBlockTree alg a
createMerkleTree leaves
  = MerkleBlockTree
  $ merkled
  $ case leaves of
      [] -> Nothing
      _  -> Just $ buildMerkleTree (Branch `on` merkled) $ Leaf <$> leaves

-- FIXME: ZZZ
-- -- | Calculate Merkle root of given sequence without constructin
-- --   complete tree.
-- computeMerkleRoot
--   :: forall alg a. (CryptoHash alg, CryptoHashable a)
--   => [a]
--   -> Hash alg
-- computeMerkleRoot leaves
--   = (hash :: Maybe (Hash alg) -> Hash alg)
--   $ case leaves of
--       [] -> Nothing
--       _  -> Just $ buildMerkleTree concatHash $ map computeLeafHash leaves

mkLeaf :: (CryptoHash alg, CryptoHashable a) => a -> Node alg a
mkLeaf x = Leaf x

mkBranch :: (CryptoHash alg, CryptoHashable a) => Node alg a -> Node alg a -> Node alg a
mkBranch = undefined


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
  :: (CryptoHashable a, CryptoHash alg)
  => Hash alg          -- ^ Root hash of Merkle Tree
  -> MerkleProof alg a -- ^ Proof
  -> Bool
checkMerkleProof rootH (MerkleProof a path)
  = undefined
    -- FIXME: ZZZ
  -- = rootH == hash (Just (foldr step (hash $ Leaf a) path))
  -- where
  --   step (Left  g) h = undefined -- concatHash h g
  --   step (Right g) h = undefined -- concatHash g h


-- | Create proof of inclusion. Implementation is rather inefficient
createMerkleProof
  :: (CryptoHash alg, CryptoHashable a, Eq a)
  => MerkleBlockTree alg a
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
isBalanced :: MerkleBlockTree alg a -> Bool
isBalanced (MerkleBlockTree (Merkled _ Nothing))     = True
isBalanced (MerkleBlockTree (Merkled _(Just tree)))
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

-- | Check whether all hashes in the tree are consistent
isConsistent
  :: forall alg a. (CryptoHashable a, CryptoHash alg)
  => MerkleBlockTree alg a -> Bool
isConsistent (MerkleBlockTree tree)
  =  checkMerkled tree
  && case merkleValue tree of
       Nothing -> True
       Just n  -> check n
  where
    check (Leaf _) = True
    check (Branch a b) = checkMerkled a
                      && checkMerkled b
                      && check (merkleValue a)
                      && check (merkleValue b)
