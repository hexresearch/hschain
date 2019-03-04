{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}
module Thundermint.Crypto.Ed25519 where

import Thundermint.Crypto
import Control.Monad.IO.Class
import Control.DeepSeq       (NFData(..))
import Crypto.Error          (CryptoFailable(..), throwCryptoError)
import Crypto.Hash           (Digest)
import Data.ByteArray        (convert)
import Data.ByteString       (ByteString)
import Data.Ord              (comparing)
import System.Entropy        (getEntropy)

import qualified Crypto.PubKey.Ed25519 as Ed
import qualified Crypto.Hash           as Crypto


----------------------------------------------------------------
-- 
----------------------------------------------------------------

sha512 :: ByteString -> ByteString
sha512 = convert . id @(Digest Crypto.SHA512) . Crypto.hash

sha256 :: ByteString -> ByteString
sha256 = convert . id @(Digest Crypto.SHA256) . Crypto.hash

data Ed25519
data SHA512

type Ed25519_SHA512 = Ed25519 :& SHA512

newtype instance PrivKey   (Ed25519 :& hash) = PrivKey   Ed.SecretKey
newtype instance PublicKey (Ed25519 :& hash) = PublicKey Ed.PublicKey

instance CryptoSignPrim (Ed25519 :& hash) where
  type FingerprintSize (Ed25519 :& hash) = 32
  type PublicKeySize   (Ed25519 :& hash) = 32
  type PrivKeySize     (Ed25519 :& hash) = 32
  type SignatureSize   (Ed25519 :& hash) = 64

instance CryptoSign (Ed25519 :& hash) where
  signBlob (PrivKey k)  = Signature . convert . Ed.sign k pubKey
   where pubKey = Ed.toPublic k

  verifyBlobSignature (PublicKey pubKey) blob (Signature s)
    = Ed.verify pubKey blob (throwCryptoError $ Ed.signature s)

  publicKey   (PrivKey k)   = PublicKey $ Ed.toPublic k
  fingerprint (PublicKey k) = Fingerprint $ sha256 . sha512 $ convert k

instance CryptoHash (sign :& SHA512) where

  hashBlob                = Hash . sha512

  hashEquality (Hash hbs) bs = hbs == bs

  type HashSize     (sign :& SHA512) = 64

generatePrivKey :: (MonadIO m) => m (PrivKey (Ed25519 :& hash))
generatePrivKey = do
  bs <- liftIO $ getEntropy Ed.secretKeySize
  case Ed.secretKey bs of
    CryptoPassed k -> return $! PrivKey k
    CryptoFailed e -> error (show e)


deriving instance Eq (PrivKey (Ed25519 :& hash))

instance Ord (PrivKey (Ed25519 :& hash)) where
  compare = comparing encodeToBS

deriving instance NFData (PrivKey (Ed25519 :& hash))

instance (Ord (PrivKey (Ed25519 :& hash))) => ByteRepr (PrivKey (Ed25519 :& hash)) where
  decodeFromBS        bs = case Ed.secretKey bs of
    CryptoPassed k -> Just (PrivKey k)
    CryptoFailed _ -> Nothing
  encodeToBS (PrivKey k) = convert k

deriving instance Eq  (PublicKey (Ed25519 :& hash))

instance Ord (PublicKey (Ed25519 :& hash)) where
  compare = comparing encodeToBS

deriving instance NFData (PublicKey (Ed25519 :& hash))

instance (Ord (PublicKey (Ed25519 :& hash))) => ByteRepr (PublicKey (Ed25519 :& hash)) where
  decodeFromBS        bs = case Ed.publicKey bs of
    CryptoPassed k -> Just (PublicKey k)
    CryptoFailed _ -> Nothing
  encodeToBS (PublicKey k) = convert k

