{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Arbitrary instances for QuickTest
--
module TM.Arbitrary.Instances where

import Data.ByteString.Arbitrary as Arb
import Data.Maybe
import Data.SafeCopy
import Data.Proxy
import qualified Data.ByteString    as BS
import qualified Data.List.NonEmpty as NE
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Arbitrary.Generic
import Test.QuickCheck.Gen

import Thundermint.Types
import Thundermint.Crypto



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


instance Arbitrary (Fingerprint alg) where
  arbitrary = Fingerprint <$> Arb.fastRandBs 256


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

instance (Arbitrary a, SafeCopy a) => Arbitrary (Signed sign alg a) where
  arbitrary = genericArbitrary
  shrink = genericShrink


instance Arbitrary (Commit alg a) where
  arbitrary = Commit <$> arbitrary
                     <*> ((NE.:|) <$> arbitrary <*> resize 3 arbitrary)


instance Arbitrary (BlockID alg a) where
  arbitrary = BlockID <$> arbitrary
  shrink = genericShrink


instance Arbitrary VoteType where
  arbitrary = genericArbitrary
  shrink = genericShrink


instance (Arbitrary a, SafeCopy a, Crypto alg, Arbitrary (PublicKey alg)) => Arbitrary (Block alg a) where
  arbitrary = Block <$> arbitrary
                    <*> arbitrary
                    <*> resize 4 arbitrary
                    <*> arbitrary
                    <*> resize 4 arbitrary

  shrink = genericShrink

instance Arbitrary (ByzantineEvidence alg a) where
  arbitrary = genericArbitrary
  shrink = genericShrink

instance Arbitrary (Vote ty alg a) where
  arbitrary = genericArbitrary
  shrink = genericShrink

instance Arbitrary (Proposal alg a) where
  arbitrary = genericArbitrary
  shrink = genericShrink

instance (Arbitrary (PublicKey alg)) => Arbitrary (ValidatorChange alg) where
  arbitrary = genericArbitrary
  shrink = genericShrink

instance CryptoSign alg => Arbitrary (PublicKey alg) where
  arbitrary = do
    bs <- vectorOf (privKeySize (Proxy @alg)) arbitrary
    return $ fromJust $ decodeFromBS $ BS.pack bs
  shrink _ = []

instance (SafeCopy a, Arbitrary a) => Arbitrary (Pet a) where
  arbitrary = petrify <$> arbitrary
  shrink    = map petrify . shrink . pet
