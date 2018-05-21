{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

module Thundermint.Crypto.Ed25519 where

import Thundermint.Crypto

import Crypto.Error        (throwCryptoError)
import Crypto.Hash         (Digest, SHA512, hash)
import Crypto.Random.Types (MonadRandom)
import Data.ByteArray      (ByteArrayAccess, convert)
import Data.ByteString     (ByteString)

import qualified Crypto.PubKey.Ed25519 as Ed

sha512 :: ByteString -> ByteString
sha512 = convert . asSHA512 . hash
 where
  asSHA512 :: Digest SHA512 -> Digest SHA512
  asSHA512 = id

data Ed25519_SHA512

newtype instance PrivKey   Ed25519_SHA512 = PrivKey Ed.SecretKey
newtype instance PublicKey Ed25519_SHA512 = PublicKey Ed.PublicKey
--newtype instance Signature Ed25519_SHA512 = Ed25519_SHA512Sig Ed.Signature

-- | We assume that there's
instance Crypto Ed25519_SHA512 where
  signBlob (PrivKey k)  = Signature . convert . Ed.sign k pubKey
   where pubKey = Ed.toPublic k

  verifyBlobSignature (PublicKey pubKey) blob (Signature s) = Ed.verify pubKey blob (throwCryptoError $ Ed.signature s)
  publicKey (PrivKey k)  = PublicKey $ Ed.toPublic k
  address   (PublicKey  k)  = Address $ convert k
  hashBlob                  = Hash . sha512

generatePrivKey :: MonadRandom m => m (PrivKey Ed25519_SHA512)
generatePrivKey = fmap PrivKey Ed.generateSecretKey

privateKey :: ByteArrayAccess ba => ba -> PrivKey Ed25519_SHA512
privateKey = PrivKey . throwCryptoError . Ed.secretKey
