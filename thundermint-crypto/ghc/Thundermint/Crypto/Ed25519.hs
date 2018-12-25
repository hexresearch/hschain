{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
module Thundermint.Crypto.Ed25519 where

import Thundermint.Crypto
import Control.Monad.IO.Class
import Control.DeepSeq       (NFData(..))
import Crypto.Error          (CryptoFailable(..), throwCryptoError)
import Crypto.Hash           (Digest, SHA256, SHA512)
import qualified Crypto.Hash as Crypto
import Data.ByteArray        (convert)
import Data.ByteString       (ByteString)
import Data.Ord              (comparing)
import System.Entropy        (getEntropy)

import qualified Crypto.PubKey.Ed25519 as Ed


----------------------------------------------------------------
-- 
----------------------------------------------------------------

sha512 :: ByteString -> ByteString
sha512 = convert . id @(Digest SHA512) . Crypto.hash

sha256 :: ByteString -> ByteString
sha256 = convert . id @(Digest SHA256) . Crypto.hash

data Ed25519_SHA512

newtype instance PrivKey   Ed25519_SHA512 = PrivKey   Ed.SecretKey
newtype instance PublicKey Ed25519_SHA512 = PublicKey Ed.PublicKey

instance Crypto Ed25519_SHA512 where
  signBlob (PrivKey k)  = Signature . convert . Ed.sign k pubKey
   where pubKey = Ed.toPublic k

  verifyBlobSignature (PublicKey pubKey) blob (Signature s)
    = Ed.verify pubKey blob (throwCryptoError $ Ed.signature s)

  publicKey (PrivKey k)   = PublicKey $ Ed.toPublic k
  address   (PublicKey k) = Address $ sha256 . sha512 $ convert k
  hashBlob                = Hash . sha512
  privKeyFromBS bs = case Ed.secretKey bs of
    CryptoPassed k -> Just (PrivKey k)
    CryptoFailed _ -> Nothing
  pubKeyFromBS  bs = case Ed.publicKey bs of
    CryptoPassed k -> Just (PublicKey k)
    CryptoFailed _ -> Nothing
  privKeyToBS (PrivKey   k) = convert k
  pubKeyToBS  (PublicKey k) = convert k

generatePrivKey :: MonadIO m => m (PrivKey Ed25519_SHA512)
generatePrivKey = do
  bs <- liftIO $ getEntropy Ed.secretKeySize
  case Ed.secretKey bs of
    CryptoPassed k -> return $! PrivKey k
    CryptoFailed e -> error (show e)


deriving instance Eq (PrivKey Ed25519_SHA512)

instance Ord (PrivKey Ed25519_SHA512) where
  compare = comparing privKeyToBS

deriving instance NFData (PrivKey Ed25519_SHA512)


deriving instance Eq  (PublicKey Ed25519_SHA512)

instance Ord (PublicKey Ed25519_SHA512) where
  compare = comparing pubKeyToBS

deriving instance NFData (PublicKey Ed25519_SHA512)
