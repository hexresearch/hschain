{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Arbitrary instances for QuickTest
--
module Thundermint.Arbitrary.Instances where

import Data.Maybe
import Data.Proxy
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Arbitrary.Generic
import Test.QuickCheck.Gen

import Thundermint.Types
import Thundermint.Crypto
import Thundermint.Types.Network (NetAddr(..))

import qualified Data.ByteString           as BS
import           Data.ByteString.Arbitrary as Arb
import qualified Data.List.NonEmpty        as NE

----------------------------------------------------------------
-- Crypto inctances
----------------------------------------------------------------

instance CryptoHash alg => Arbitrary (Hash alg) where
  arbitrary = Hash <$> Arb.fastRandBs (hashSize (Proxy @alg))
  shrink _ = []

instance Arbitrary (Hash alg) => Arbitrary (Hashed alg a) where
  arbitrary = Hashed <$> arbitrary
  shrink  _ = []

instance Arbitrary (Fingerprint alg) where
  arbitrary = Fingerprint <$> Arb.fastRandBs 256

instance CryptoSign alg => Arbitrary (Signature alg) where
  arbitrary = Signature <$> Arb.fastRandBs (signatureSize (Proxy @alg))
  shrink _ = []

instance (CryptoSign alg, Arbitrary a, Arbitrary key) => Arbitrary (Signed key sign alg a) where
  arbitrary = genericArbitrary
  shrink = genericShrink

instance CryptoSign alg => Arbitrary (PublicKey alg) where
  arbitrary = do
    bs <- vectorOf (privKeySize (Proxy @alg)) arbitrary
    return $ fromJust $ decodeFromBS $ BS.pack bs
  shrink _ = []

instance CryptoSign alg => Arbitrary (PrivKey alg) where
  arbitrary = do
    bs <- vectorOf (privKeySize (Proxy @alg)) arbitrary
    return $ fromJust $ decodeFromBS $ BS.pack bs
  shrink _ = []

instance Arbitrary (KDFOutput alg) where
  arbitrary = KDFOutput <$> Arb.fastRandBs 256
  shrink  _ = []

instance CryptoDH alg => Arbitrary (DHSecret alg) where
  arbitrary = do
    bs <- vectorOf (dhSecretSize (Proxy @alg)) arbitrary
    return $ fromJust $ decodeFromBS $ BS.pack bs
  shrink _ = []

instance StreamCypher alg => Arbitrary (CypherNonce alg) where
  arbitrary = do
    bs <- vectorOf (cypherNonceSize (Proxy @alg)) arbitrary
    return $ fromJust $ decodeFromBS $ BS.pack bs
  shrink _ = []

instance StreamCypher alg => Arbitrary (CypherKey alg) where
  arbitrary = do
    bs <- vectorOf (cypherKeySize (Proxy @alg)) arbitrary
    return $ fromJust $ decodeFromBS $ BS.pack bs
  shrink _ = []

instance StreamCypher cypher => Arbitrary (SecretBox cypher) where
  arbitrary = SecretBox <$> Arb.fastRandBs 256 <*> arbitrary
  shrink  _ = []

instance StreamCypher cypher => Arbitrary (PubKeyBox key kdf cypher) where
  arbitrary = PubKeyBox <$> Arb.fastRandBs 256 <*> arbitrary
  shrink  _ = []

----------------------------------------------------------------
-- Blockchain inctances
----------------------------------------------------------------

instance Arbitrary Height where
  arbitrary = Height <$> arbitrary
  shrink = genericShrink


instance Arbitrary Time where
    arbitrary = Time <$> arbitrary
    shrink = genericShrink


instance Arbitrary Round where
    arbitrary = Round <$> arbitrary
    shrink = genericShrink

instance (CryptoHash alg, Arbitrary a) => Arbitrary (Header alg a) where
  arbitrary = Header <$> Arb.fastRandBs 1024
                     <*> arbitrary
                     <*> arbitrary
                     <*> arbitrary
                     <*> arbitrary
                     <*> arbitrary
                     <*> arbitrary
                     <*> arbitrary
                     <*> arbitrary

instance (CryptoSign alg, CryptoHash alg) => Arbitrary (Commit alg a) where
  arbitrary = Commit <$> arbitrary
                     <*> ((NE.:|) <$> arbitrary <*> resize 3 arbitrary)


instance (CryptoHash alg) => Arbitrary (BlockID alg a) where
  arbitrary = BlockID <$> arbitrary
  shrink = genericShrink


instance Arbitrary VoteType where
  arbitrary = genericArbitrary
  shrink = genericShrink

instance (CryptoSign alg, CryptoHash alg, Arbitrary a, Arbitrary (PublicKey alg)) => Arbitrary (Block alg a) where
  arbitrary = Block <$> arbitrary
                    <*> arbitrary
                    <*> resize 4 arbitrary
                    <*> arbitrary
                    <*> resize 4 arbitrary
  shrink = genericShrink

instance (CryptoSign alg, CryptoHash alg) => Arbitrary (ByzantineEvidence alg a) where
  arbitrary = genericArbitrary
  shrink = genericShrink

instance (CryptoHash alg) => Arbitrary (Vote ty alg a) where
  arbitrary = genericArbitrary
  shrink = genericShrink

instance (CryptoHash alg) => Arbitrary (Proposal alg a) where
  arbitrary = genericArbitrary
  shrink = genericShrink

instance (Arbitrary (PublicKey alg)) => Arbitrary (ValidatorChange alg) where
  arbitrary = genericArbitrary
  shrink = genericShrink

instance Arbitrary (ValidatorIdx alg) where
  arbitrary = ValidatorIdx <$> arbitrary

----------------------------------------------------------------
-- Network inctances
----------------------------------------------------------------

instance Arbitrary NetAddr where
  arbitrary = oneof
    [ NetAddrV4   <$> arbitrary <*> arbitrary
    , NetAddrV6   <$> arbitrary <*> arbitrary
    ]

