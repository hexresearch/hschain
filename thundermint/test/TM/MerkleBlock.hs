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
import Thundermint.Crypto.Ed25519 (Ed25519)
import Thundermint.Crypto.SHA     (SHA512)

import Codec.Serialise
import GHC.Generics    (Generic)

import Thundermint.Types.MerkleBlock

----------------------------------------------------------------
-- Data types
----------------------------------------------------------------

-- | Merkle tree tests
tests :: TestTree
tests = testGroup "Block Merkle tree (fanout=2)"
        [testProperty "Root of tree and computed root equals" prop_MerkleBlockTree]

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
  ((rootHash . merkleBlockRoot) <$> tree) == (computeMerkleRoot $ unWrap bs)
  where
    tree   :: Maybe (MerkleBlockTree SHA512 ByteString)
    tree = createMerkleTree $ unWrap bs
    unWrap :: [BS] -> [ByteString]
    unWrap bs = [blob | BS blob <- bs]



