{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns      #-}
-- |
module TM.Merkle (tests) where

import Data.ByteString (ByteString)
import Data.Functor.Identity
import Data.Word
import qualified Data.ByteString as BS
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

import Thundermint.Crypto.Ed25519
import Thundermint.Types.Merkle


tests :: TestTree
tests = testGroup "Merkle tree"
  [ testProperty "Constructed tree is correct" prop_TreeCorrect
  , testCase     "Null string" $ do
      case merklize 128 "" :: MerkleTree Ed25519_SHA512 Identity of
        MerkleTree { merkleRoot = MerkleRoot { blobSize = 0 }
                   , merkleTree = Leaf ""
                   } -> return ()
        _ -> assertFailure "Invalid tree"
  ]


prop_TreeCorrect :: [Word8] -> Bool
prop_TreeCorrect (BS.pack . (0:) -> blob)
  = and [ chunk == partSize root
        , fromIntegral (BS.length blob) == blobSize root
          --
        , BS.concat leaves == blob
        , leafLengthOK chunk leaves
        ]
  where
    chunk  :: Num a => a
    chunk  = 128
    tree   :: MerkleTree Ed25519_SHA512 Identity
    tree   = merklize chunk blob
    root   = merkleRoot tree
    leaves = treeLeaves tree

leafLengthOK :: Int -> [ByteString] -> Bool
leafLengthOK _ []     = True
leafLengthOK n [b]    = let m = BS.length b in m > 0 && m <= n
leafLengthOK n (b:bs) = n == BS.length b && leafLengthOK n bs

treeLeaves :: MerkleTree alg Identity -> [ByteString]
treeLeaves = go . merkleTree
  where
    go (Leaf   bs) = [bs]
    go (Branch ns) = go . runIdentity . merkleChild =<< ns
