{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies       #-}

module Thundermint.Crypto.Ed25519 where

import Thundermint.Crypto

import Crypto.Error          (CryptoFailable(..), eitherCryptoError, throwCryptoError)
import Crypto.Hash           (Digest, SHA512, hash)
import Crypto.Random.Types   (MonadRandom)
import Data.Aeson            (FromJSON, ToJSON, Value(..), parseJSON, toJSON)
import Data.ByteArray        (ByteArrayAccess, convert)
import Data.ByteString       (ByteString)
import Data.ByteString.Char8 (unpack)

import qualified Codec.Serialise       as CBOR
import qualified Crypto.PubKey.Ed25519 as Ed
import qualified Data.Text.Encoding    as T

sha512 :: ByteString -> ByteString
sha512 = convert . asSHA512 . hash
 where
  asSHA512 :: Digest SHA512 -> Digest SHA512
  asSHA512 = id

data Ed25519_SHA512

newtype instance PrivKey   Ed25519_SHA512 = PrivKey Ed.SecretKey
newtype instance PublicKey Ed25519_SHA512 = PublicKey Ed.PublicKey

instance Crypto Ed25519_SHA512 where
  signBlob (PrivKey k)  = Signature . convert . Ed.sign k pubKey
   where pubKey = Ed.toPublic k

  verifyBlobSignature (PublicKey pubKey) blob (Signature s)
    = Ed.verify pubKey blob (throwCryptoError $ Ed.signature s)

  publicKey (PrivKey k)   = PublicKey $ Ed.toPublic k
  address   (PublicKey k) = Address $ convert k
  hashBlob                = Hash . sha512
  


generatePrivKey :: MonadRandom m => m (PrivKey Ed25519_SHA512)
generatePrivKey = fmap PrivKey Ed.generateSecretKey

privateKey :: ByteArrayAccess ba => ba -> PrivKey Ed25519_SHA512
privateKey = PrivKey . throwCryptoError . Ed.secretKey

----------------------------------------------------------------
-- Private key instances
----------------------------------------------------------------

deriving instance Eq (PrivKey Ed25519_SHA512)

instance CBOR.Serialise (PublicKey Ed25519_SHA512) where
  encode (PublicKey pk) = CBOR.encode (convert pk :: ByteString)
  decode = do bs <- CBOR.decode
              case Ed.publicKey (bs :: ByteString) of
                CryptoPassed pk -> return (PublicKey pk)
                CryptoFailed e  -> fail (show e)


instance Show (PrivKey Ed25519_SHA512) where
  show (PrivKey k) = show $ unpack $ encodeBase58 $ convert k

instance ToJSON (PrivKey Ed25519_SHA512) where
  toJSON (PrivKey k) = String
                     $ T.decodeUtf8
                     $ encodeBase58
                     $ convert k

instance FromJSON (PrivKey Ed25519_SHA512) where
  parseJSON (String s) =
    case decodeBase58 $ T.encodeUtf8 s of
      Nothing -> fail  "Incorrect Base58 encoding for bs"
      Just bs -> either (fail.show) (return.PrivKey)
                 $ eitherCryptoError
                 $ Ed.secretKey bs

  parseJSON _          = fail "Expect PrivKey"


----------------------------------------------------------------
-- Public key instances
----------------------------------------------------------------

deriving instance Eq (PublicKey Ed25519_SHA512)

instance Show (PublicKey Ed25519_SHA512) where
  show (PublicKey k) = show $ unpack $ encodeBase58 $ convert k

instance ToJSON (PublicKey Ed25519_SHA512) where
  toJSON (PublicKey k) = String
                     $ T.decodeUtf8
                     $ encodeBase58
                     $ convert k

instance FromJSON (PublicKey Ed25519_SHA512) where
  parseJSON (String s) =
    case decodeBase58 $ T.encodeUtf8 s of
      Nothing -> fail  "Incorrect Base58 encoding for bs"
      Just bs -> either (fail . show) (return . PublicKey)
                 $ eitherCryptoError
                 $ Ed.publicKey bs

  parseJSON _          = fail "Expect PrivKey"
