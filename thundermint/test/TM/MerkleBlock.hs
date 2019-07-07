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

import Data.Maybe
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

import Data.ByteString (ByteString)

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
        [testProperty "Constructed tree is correct" prop_MerkleBlockTree]

newtype BS = BS ByteString
  deriving (Show)

instance Arbitrary BS where
  arbitrary = do
    n <- choose (1,4000)
    BS . BS.pack <$> vectorOf n arbitrary
  shrink (BS bs) = [ BS (BS.pack x)
                   | x <- shrink (BS.unpack bs)
                   , not (null x)
                   ]


prop_MerkleBlockTree :: [BS] -> Property
prop_MerkleBlockTree bs = property $
  and [ ((rootHash . merkleBlockRoot) tree) == (computeMerkleRoot $ unWrap bs)
      , isBalanced (merkleBlockTree tree)
      ]
  where
    tree   :: (MerkleBlockTree SHA512 ByteString)
    tree = createMerkleTree $ unWrap bs
    unWrap :: [BS] -> [ByteString]
    unWrap xs = [blob | BS blob <- xs]



