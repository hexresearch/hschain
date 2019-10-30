{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Arbitrary instances for QuickTest
--
module HSChain.Arbitrary.Instances where

import Control.Monad
import Data.Maybe
import Data.Proxy
import Data.List     (nubBy)
import Data.Function (on)
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Arbitrary.Generic
import Test.QuickCheck.Gen

import HSChain.Types
import HSChain.Types.Merklized
import HSChain.Crypto
import HSChain.Types.Network (NetAddr(..))

import qualified Data.Map.Strict           as Map
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

instance CryptoHash hash => Arbitrary (Fingerprint hash alg) where
  arbitrary = Fingerprint . Hashed . Hash <$> Arb.fastRandBs (hashSize (Proxy @hash))

instance CryptoSign alg => Arbitrary (Signature alg) where
  arbitrary = Signature <$> Arb.fastRandBs (signatureSize (Proxy @alg))
  shrink _ = []

instance (CryptoSign alg, Arbitrary a) => Arbitrary (Signed sign alg a) where
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

instance (CryptoHash alg, CryptoHashable a, IsMerkle f, Arbitrary a) => Arbitrary (MerkleNode f alg a) where
  arbitrary = merkled <$> arbitrary
  shrink a  = case toOptNode a of
    OptNode _ Nothing  -> []
    OptNode _ (Just x) -> merkled <$> shrink x

instance Arbitrary Height where
  arbitrary = Height <$> arbitrary
  shrink = genericShrink


instance Arbitrary Time where
    arbitrary = Time <$> arbitrary
    shrink = genericShrink


instance Arbitrary Round where
    arbitrary = Round <$> arbitrary
    shrink = genericShrink

instance (CryptoSign alg, CryptoHash alg) => Arbitrary (Commit alg a) where
  arbitrary = Commit <$> arbitrary
                     <*> ((NE.:|) <$> arbitrary <*> resize 3 arbitrary)


instance (CryptoHash alg) => Arbitrary (BlockID alg a) where
  arbitrary = BlockID <$> arbitrary
  shrink = genericShrink


instance Arbitrary VoteType where
  arbitrary = genericArbitrary
  shrink = genericShrink

instance ( Crypto alg
         , IsMerkle f
         , CryptoHashable a
         , Arbitrary a
         ) => Arbitrary (GBlock f alg a) where
  arbitrary = Block <$> arbitrary
                    <*> arbitrary
                    <*> arbitrary
                    <*> resize 4 arbitrary
                    <*> arbitrary
                    <*> resize 4 arbitrary
                    <*> arbitrary
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

instance (Ord (PublicKey alg), Arbitrary (PublicKey alg)) => Arbitrary (ValidatorChange alg) where
  arbitrary = do
    n     <- choose (0,5)
    pairs <- replicateM n $ do
      k <- arbitrary
      p <- frequency [ (3, pure 0) , (1, pure 1) , (1, pure 2) , (1, pure 3) ]
      return (k,p)
    return $ ValidatorChange $ Map.fromList pairs

instance (Eq (PublicKey alg), CryptoSign alg) => Arbitrary (ValidatorSet alg) where
  arbitrary = do
    n     <- choose (0,10)
    pairs <- replicateM n arbitrary
    let Right vset = makeValidatorSet $ nubBy ((==) `on` validatorPubKey) pairs
    return vset
  --
  shrink vset =
    [ let Right v = makeValidatorSet vset' in v
    | vset' <-  shrink $ asValidatorList vset
    ]

instance CryptoSign alg => Arbitrary (Validator alg) where
  arbitrary = Validator <$> arbitrary <*> choose (1,10)

instance Arbitrary (ValidatorIdx alg) where
  arbitrary = ValidatorIdx . abs <$> arbitrary

----------------------------------------------------------------
-- Network inctances
----------------------------------------------------------------

instance Arbitrary NetAddr where
  arbitrary = oneof
    [ NetAddrV4   <$> arbitrary <*> arbitrary
    , NetAddrV6   <$> arbitrary <*> arbitrary
    ]
