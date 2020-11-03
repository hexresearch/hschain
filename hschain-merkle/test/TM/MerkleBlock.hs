{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module TM.MerkleBlock (tests) where

import Data.Maybe
import Data.Typeable
import Data.Foldable
import Test.Tasty
import Test.Tasty.QuickCheck
import HSChain.Crypto
import HSChain.Crypto.SHA (SHA512)
import HSChain.Types.Merkle.Tree
import HSChain.Types.Merkle.Types

----------------------------------------------------------------
-- Data types
----------------------------------------------------------------

-- | Merkle tree tests
tests :: TestTree
tests = testGroup "Binary Merkle tree"
  [ prop_tree @MerkleBinTree
  , prop_tree @MerkleBinTree1
  , testProperty "IdNode & Hashed"    prop_computeMerkleHashed
  , testProperty "IdNode & OptNode"   prop_computeMerkleOpt
  , testProperty "IdNode & Hashed 1"  prop_computeMerkleHashed1
  , testProperty "IdNode & OptNode 1" prop_computeMerkleOpt1
  ]

----------------------------------------------------------------
-- Generic properties
----------------------------------------------------------------

prop_tree
  :: forall t. ( Typeable t
               , MerkleTree t Integer
               , Arbitrary (t SHA512 Identity Integer)
               , Foldable (t SHA512 Identity)
               , Show (t SHA512 Identity Integer)
               )
  => TestTree
prop_tree = testGroup (show (typeRep (Proxy @t)))
  [ testProperty "Invariants"     $ prop_invariants @t
  , testProperty "Proof creation" $ prop_proof      @t
  ]

prop_invariants :: (MerkleTree t Integer) => t SHA512 Identity Integer -> Bool
prop_invariants = checkMerkleInvariants

prop_proof :: (MerkleTree t Integer, Foldable (t SHA512 Identity)) => t SHA512 Identity Integer -> Bool
prop_proof tree
  = and [ maybe False (verifyMerkleProof tree) p
        | p <- proofs
        ]
  where
    proofs = createMerkleProof tree <$> leaves
    leaves = toList tree


----------------------------------------------------------------
-- Specialized properties
----------------------------------------------------------------

-- Computation with different wrappers give same result
prop_computeMerkleOpt :: [Integer] -> Bool
prop_computeMerkleOpt leaves
  = merkleHash t1 == merkleHash t2
  where
    t1 = createMerkleTree leaves :: MerkleBinTree SHA512 Identity Integer
    t2 = createMerkleTree leaves :: MerkleBinTree SHA512 Maybe    Integer

-- Computation with different wrappers give same result
prop_computeMerkleHashed :: [Integer] -> Bool
prop_computeMerkleHashed leaves
  = merkleHash t1 == merkleHash t2
  where
    t1 = createMerkleTree leaves :: MerkleBinTree SHA512 Identity Integer
    t2 = createMerkleTree leaves :: MerkleBinTree SHA512 Proxy    Integer

-- Computation with different wrappers give same result
prop_computeMerkleOpt1 :: [Integer] -> Bool
prop_computeMerkleOpt1 leaves
  = fmap merkleHash t1 == fmap merkleHash t2
  where
    t1 = createMerkleTree1 leaves :: Maybe (MerkleBinTree1 SHA512 Identity Integer)
    t2 = createMerkleTree1 leaves :: Maybe (MerkleBinTree1 SHA512 Maybe    Integer)

-- Computation with different wrappers give same result
prop_computeMerkleHashed1 :: [Integer] -> Bool
prop_computeMerkleHashed1 leaves
  = fmap merkleHash t1 == fmap merkleHash t2
  where
    t1 = createMerkleTree1 leaves :: Maybe (MerkleBinTree1 SHA512 Identity Integer)
    t2 = createMerkleTree1 leaves :: Maybe (MerkleBinTree1 SHA512 Proxy    Integer)
  

----------------------------------------------------------------
-- Orphans
----------------------------------------------------------------

instance ( CryptoHash alg, CryptoHashable a, Arbitrary a
         ) => Arbitrary (MerkleBinTree alg Identity a) where
  arbitrary = createMerkleTree <$> arbitrary
  shrink    = fmap createMerkleTree . shrink . toList

instance ( CryptoHash alg, CryptoHashable a, Arbitrary a
         ) => Arbitrary (MerkleBinTree1 alg Identity a) where
  arbitrary = do as <- listOf1 arbitrary
                 case createMerkleTree1 as of
                   Nothing -> error "Arbitrary: MerkleBinTree1: Should mot happen"
                   Just t  -> pure t
  shrink = catMaybes . fmap createMerkleTree1 . shrink . toList
