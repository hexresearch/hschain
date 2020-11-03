{-# LANGUAGE OverloadedStrings    #-}
module TM.MerkleBlock where

import Data.Maybe
import Test.Tasty
import Test.Tasty.QuickCheck
import HSChain.Crypto.SHA (SHA512)
import HSChain.Types.Merkle.Tree
import HSChain.Types.Merkle.Types

----------------------------------------------------------------
-- Data types
----------------------------------------------------------------

-- | Merkle tree tests
tests :: TestTree
tests = testGroup "Binary Merkle tree"
  [ testProperty "createMerkleTree"  prop_MerkleBlockTree
  , testProperty "createMerkleTree1" prop_MerkleBlockTree1
  , testProperty "IdNode & Hashed"   prop_computeMerkleHashed
  , testProperty "IdNode & OptNode"  prop_computeMerkleOpt
  , testProperty "Merkle proof of inclusion is correct" prop_MerkleProofCorrect
  ]

-- Tree is balanced and all hashes are internally consistent
prop_MerkleBlockTree :: [Integer] -> Bool
prop_MerkleBlockTree leaves
  = checkMerkleInvariants tree
  where
    tree :: MerkleBlockTree SHA512 Identity Integer
    tree = createMerkleTree leaves

-- Tree is balanced and all hashes are internally consistent
prop_MerkleBlockTree1 :: [Integer] -> Bool
prop_MerkleBlockTree1 leaves =
  case leaves of
    [] -> isNothing tree
    _  -> maybe False checkMerkleInvariants tree
  where
    tree :: Maybe (MerkleBlockTree1 SHA512 Identity Integer)
    tree = createMerkleTree1 leaves

-- Computation with different wrappers give same result
prop_computeMerkleOpt :: [Integer] -> Bool
prop_computeMerkleOpt leaves
  = rootHash t1 == rootHash t2
  where
    t1 = createMerkleTree leaves :: MerkleBlockTree SHA512 Identity Integer
    t2 = createMerkleTree leaves :: MerkleBlockTree SHA512 Maybe    Integer

-- Computation with different wrappers give same result
prop_computeMerkleHashed :: [Integer] -> Bool
prop_computeMerkleHashed leaves
  = rootHash t1 == rootHash t2
  where
    t1 = createMerkleTree leaves :: MerkleBlockTree SHA512 Identity Integer
    t2 = createMerkleTree leaves :: MerkleBlockTree SHA512 Proxy    Integer

  
-- Check that merkle proofs are correct
prop_MerkleProofCorrect :: [Integer] -> Property
prop_MerkleProofCorrect leaves
  = property
  $ and [ maybe False (verifyMerkleProof tree) p
        | p <- proofs
        ]
  where
    tree :: MerkleBlockTree SHA512 Identity Integer
    tree   = createMerkleTree leaves
    proofs = createMerkleProof tree <$> leaves

-- Check that merkle proofs are correct
prop_MerkleProofCorrect1 :: [Integer] -> Property
prop_MerkleProofCorrect1 leaves
  = not (null leaves)
  ==> and [ maybe False (verifyMerkleProof tree) p
          | p <- proofs
          ]
  where
    tree :: MerkleBlockTree1 SHA512 Identity Integer
    Just tree = createMerkleTree1 leaves
    proofs = createMerkleProof tree <$> leaves
