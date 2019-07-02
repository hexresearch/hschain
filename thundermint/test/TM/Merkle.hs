{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}

module TM.Merkle (tests) where

import Data.Functor.Identity
import Data.Maybe
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

import Data.ByteString (ByteString)

import qualified Data.ByteString as BS

import Thundermint.Crypto         ((:&))
import Thundermint.Crypto.Ed25519 (Ed25519)
import Thundermint.Crypto.SHA     (SHA512)

import Thundermint.Types.Merkle


-- | Merkle tree tests
tests :: TestTree
tests = testGroup "Merkle tree"
  [ testProperty "Constructed tree is correct" prop_TreeCorrect
  , testCase     "Null string" $ do
      case merklize 128 "" :: MerkleTree SHA512 Identity of
        MerkleTree { merkleRoot = MerkleRoot { blobSize = 0 }
                   , merkleTree = Leaf ""
                   } -> return ()
        _ -> assertFailure "Invalid tree"
  ]

prop_TreeCorrect :: BS -> Property
prop_TreeCorrect (BS blob)
  = counterexample ("length = " ++ show (BS.length blob))
  $ counterexample ("chunks = " ++ show (BS.length <$> leaves))
  $ counterexample ("Reconstructed = " ++ show (isJust reco))
  $ and [ chunk == partSize root
        , fromIntegral (BS.length blob) == blobSize root
          --
        , reco == Just blob
        , leafLengthOK chunk leaves
        ]
  where
    chunk  :: Num a => a
    chunk  = 128
    tree   :: MerkleTree (Ed25519 :& SHA512) Identity
    tree   = merklize chunk blob
    root   = merkleRoot tree
    leaves = treeLeaves tree
    reco   = concatTree tree

leafLengthOK :: Int -> [ByteString] -> Bool
leafLengthOK _ []     = True
leafLengthOK n [b]    = let m = BS.length b in m > 0 && m <= n
leafLengthOK n (b:bs) = n == BS.length b && leafLengthOK n bs

treeLeaves :: MerkleTree alg Identity -> [ByteString]
treeLeaves = go . merkleTree
  where
    go (Leaf   bs) = [bs]
    go (Branch ns) = go . runIdentity . merkleChild =<< ns


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

