{-# LANGUAGE OverloadedStrings    #-}
module TM.MerkleBlock where

import Test.Tasty
import Test.Tasty.QuickCheck
import Thundermint.Crypto.SHA (SHA512)
import Thundermint.Types.MerkleBlock

----------------------------------------------------------------
-- Data types
----------------------------------------------------------------

-- | Merkle tree tests
tests :: TestTree
tests = testGroup "Binary Merkle tree"
  [ testProperty "createMerkleTree"  prop_MerkleBlockTree
  , testProperty "computeMerkleRoot" prop_computeMerkleRoot
  , testProperty "Merkle proof of inclusion is correct" prop_MerkleProofCorrect
  ]

-- Tree is balanced and all hashes are internally consistent
prop_MerkleBlockTree :: [Integer] -> Bool
prop_MerkleBlockTree leaves
  = and [ isConsistent tree
        , isBalanced   tree
        ]
  where
    tree :: MerkleBlockTree SHA512 Integer
    tree = createMerkleTree leaves

-- merkleBlockTree && computeMerkleRoot give same hash
prop_computeMerkleRoot :: [Integer] -> Bool
prop_computeMerkleRoot leaves
  = merkleBlockRoot tree == computeMerkleRoot leaves
  where
    tree :: MerkleBlockTree SHA512 Integer
    tree = createMerkleTree leaves
 
-- Check that merkle proofs are correct
prop_MerkleProofCorrect :: [Integer] -> Property
prop_MerkleProofCorrect leaves
  = property
  $ and [ maybe False (checkMerkleProof (merkleBlockRoot tree)) p
        | p <- proofs
        ]
  where
    tree :: MerkleBlockTree SHA512 Integer
    tree   = createMerkleTree leaves
    proofs = createMerkleProof tree <$> leaves
