{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

module TM.MerkleBlock where

import Data.List (nub)

import Test.Tasty
import Test.Tasty.QuickCheck

import Data.ByteString (ByteString)
import System.Random   (randomRIO)

import qualified Data.ByteString as BS

import Thundermint.Crypto
import Thundermint.Crypto.SHA (SHA512)

import Thundermint.Types.MerkleBlock

----------------------------------------------------------------
-- Data types
----------------------------------------------------------------

-- | Merkle tree tests
tests :: TestTree
tests = testGroup "Block Merkle tree (fanout=2)"
        [ testProperty "Constructed tree is correct" prop_MerkleBlockTree
        , testProperty "Merkle proof of inclusion is correct" prop_MerkleProofCorrect
        ]

newtype BS = BS ByteString
  deriving (Show)

newtype BSList = BSList [ByteString]
  deriving (Show)

instance Arbitrary BS where
  arbitrary = do
    n <- choose (1,4000)
    BS . BS.pack <$> vectorOf n arbitrary
  shrink (BS bs) = [ BS (BS.pack x)
                   | x <- shrink (BS.unpack bs)
                   , not (null x)
                   ]

-- instance Arbitrary BSList where
--   arbitrary = do
--     n <- choose (1,4000)
--     BSList . map  BS.pack <$> vectorOf n arbitrary
--   shrink (BSList bs) = [BSList [x | x <- shrink bs
--                               , not (null x)
--                               ]]

-- |
-- test tree root hash, is balanced
prop_MerkleBlockTree :: [BS] -> Property
prop_MerkleBlockTree bs = property $
  and [ rootH == (computeMerkleRoot $ unWrap bs)
      , isBalanced  tree
      , isBalanced' tree
      ]
  where
    mTree   :: MerkleBlockTree SHA512 ByteString
    mTree = createMerkleTree $ unWrap bs
    tree = merkleBlockTree  mTree
    rootH = rootHash . merkleBlockRoot $ mTree
    unWrap :: [BS] -> [ByteString]
    unWrap xs = nub [blob | BS blob <- xs]


-- |
-- Merkle tree proof tests
prop_MerkleProofCorrect :: BS  -> Property
prop_MerkleProofCorrect (BS bs) = ioProperty $ do
  index <- randomRIO (0, n-1)
  let leafBS = leaves !! index
      leafHash = (hash (0::Int, leafBS))

      path = merklePath tree leafHash

  return $ snd $ merkleProof rootH path leafHash
  where
    blockTree :: MerkleBlockTree SHA512 ByteString
    blockTree = createMerkleTree $ nub $ BS.group bs

    tree = merkleBlockTree blockTree
    rootH = rootHash $ merkleBlockRoot blockTree

    (n, leaves) = case tree of
                          Just t -> let xs = (nub . treeLeaves) t
                                    in (length xs, xs)
                          Nothing -> (0, [])


