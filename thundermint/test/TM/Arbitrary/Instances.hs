{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Arbitrary instances for QuickTest
--
module TM.Arbitrary.Instances where

import Data.ByteString.Arbitrary as Arb
import Data.Maybe
import qualified Data.ByteString as BS
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Arbitrary.Generic
import Test.QuickCheck.Gen

import Thundermint.Types
import Thundermint.Crypto
import Thundermint.Crypto.Ed25519


instance Arbitrary (Hash alg) where
    arbitrary = Hash <$> Arb.fastRandBs 100

instance Arbitrary (Hashed alg a) where
    arbitrary = Hashed <$> arbitrary

instance Arbitrary Height where
    arbitrary = Height <$> arbitrary


instance Arbitrary Time where
    arbitrary = Time <$> arbitrary


instance Arbitrary Round where
    arbitrary = Round <$> arbitrary


instance Arbitrary (Address alg) where
  arbitrary = Address <$> Arb.fastRandBs 256


instance Arbitrary (Signature alg) where
  arbitrary = Signature <$> Arb.fastRandBs 256


instance (Arbitrary a) => Arbitrary (Header alg a) where
  arbitrary = Header <$> Arb.fastRandBs 1024
                     <*> arbitrary
                     <*> arbitrary
                     <*> arbitrary
                     <*> arbitrary
                     <*> arbitrary
                     <*> arbitrary
                     <*> arbitrary
                     <*> arbitrary

instance (Arbitrary a) => Arbitrary (Signed sign alg a) where
  arbitrary = genericArbitrary
  shrink = genericShrink


instance (Arbitrary a) => Arbitrary (Commit alg a) where
  arbitrary = Commit <$> arbitrary
                     <*> resize 4 arbitrary
  shrink = genericShrink


instance (Arbitrary a) => Arbitrary (BlockID alg a) where
  arbitrary = BlockID <$> arbitrary
  shrink = genericShrink


instance Arbitrary VoteType where
  arbitrary = genericArbitrary
  shrink = genericShrink


instance (Arbitrary a, Crypto alg, Arbitrary (PublicKey alg)) => Arbitrary (Block alg a) where
  arbitrary = Block <$> arbitrary
                    <*> arbitrary
                    <*> resize 4 arbitrary
                    <*> arbitrary
                    <*> resize 4 arbitrary
  shrink = genericShrink

instance (Arbitrary a) => Arbitrary (ByzantineEvidence alg a) where
  arbitrary = genericArbitrary
  shrink = genericShrink

instance (Arbitrary a) => Arbitrary (Vote ty alg a) where
  arbitrary = genericArbitrary
  shrink = genericShrink

instance (Arbitrary a) => Arbitrary (Proposal alg a) where
  arbitrary = genericArbitrary
  shrink = genericShrink

instance (Arbitrary (PublicKey alg), Crypto alg) => Arbitrary (ValidatorChange alg) where
  arbitrary = genericArbitrary
  shrink = genericShrink

instance Arbitrary (PublicKey Ed25519_SHA512) where
  arbitrary = do
    bs <- vectorOf 32 arbitrary
    return $ fromJust $ pubKeyFromBS $ BS.pack bs
  shrink _ = []
