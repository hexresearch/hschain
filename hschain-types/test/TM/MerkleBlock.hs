{-# LANGUAGE OverloadedStrings    #-}
module TM.MerkleBlock where

import Test.Tasty
import Test.Tasty.QuickCheck
import HSChain.Crypto     (Hashed)
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
  , testProperty "IdNode & Hashed"   prop_computeMerkleHashed
  , testProperty "IdNode & OptNode"  prop_computeMerkleOpt
  , testProperty "Merkle proof of inclusion is correct" prop_MerkleProofCorrect
  ]

-- Tree is balanced and all hashes are internally consistent
prop_MerkleBlockTree :: [Integer] -> Bool
prop_MerkleBlockTree leaves
  = isBalanced tree
  where
    tree :: MerkleBlockTree IdNode SHA512 Integer
    tree = createMerkleTree leaves

-- Computation with different wrappers give same result
prop_computeMerkleOpt :: [Integer] -> Bool
prop_computeMerkleOpt leaves
  = merkleHash t1 == merkleHash t2
  where
    t1 = createMerkleTree leaves :: MerkleBlockTree IdNode  SHA512 Integer
    t2 = createMerkleTree leaves :: MerkleBlockTree OptNode SHA512 Integer

-- Computation with different wrappers give same result
prop_computeMerkleHashed :: [Integer] -> Bool
prop_computeMerkleHashed leaves
  = merkleHash t1 == merkleHash t2
  where
    t1 = createMerkleTree leaves :: MerkleBlockTree IdNode  SHA512 Integer
    t2 = createMerkleTree leaves :: MerkleBlockTree Hashed SHA512 Integer

  
-- Check that merkle proofs are correct
prop_MerkleProofCorrect :: [Integer] -> Property
prop_MerkleProofCorrect leaves
  = property
  $ and [ maybe False (checkMerkleProof (merkleHash tree)) p
        | p <- proofs
        ]
  where
    tree :: MerkleBlockTree IdNode SHA512 Integer
    tree   = createMerkleTree leaves
    proofs = createMerkleProof tree <$> leaves
