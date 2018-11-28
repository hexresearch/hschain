{-# OPTIONS_GHC -Wno-orphans #-}

-- | Arbitrary instances for QuickTest
--
module TM.Arbitrary.Instances where


import Data.ByteString.Arbitrary as Arb
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Arbitrary.Generic

import Thundermint.Blockchain.Types
import Thundermint.Crypto


instance Arbitrary (Hash alg) where
    arbitrary = Hash <$> Arb.fastRandBs 100


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

instance (Arbitrary a) => Arbitrary (Signed sign alg a) where
  arbitrary = genericArbitrary
  shrink = genericShrink


instance (Arbitrary a) => Arbitrary (Commit alg a) where
  arbitrary = genericArbitrary
  shrink = genericShrink


instance (Arbitrary a) => Arbitrary (BlockHash alg a) where
  arbitrary = genericArbitrary
  shrink = genericShrink


instance Arbitrary VoteType where
  arbitrary = genericArbitrary
  shrink = genericShrink


instance (Arbitrary a) => Arbitrary (Block alg a) where
  arbitrary = genericArbitrary
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


