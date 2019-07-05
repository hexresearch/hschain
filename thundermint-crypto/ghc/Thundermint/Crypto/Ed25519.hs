{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}
module Thundermint.Crypto.Ed25519 (
  Ed25519
  ) where

import Control.Monad.IO.Class
import Control.DeepSeq       (NFData(..))
import Crypto.Error          (CryptoFailable(..), throwCryptoError)
import Data.ByteArray        (convert)
import Data.Data             (Data)
import Data.Ord              (comparing)
import System.Entropy        (getEntropy)

import qualified Crypto.PubKey.Ed25519    as Ed

import Thundermint.Crypto

----------------------------------------------------------------
--
----------------------------------------------------------------

-- | Ed25519 public key signature system
data Ed25519 deriving (Data)

newtype instance PrivKey   Ed25519 = PrivKey   Ed.SecretKey
newtype instance PublicKey Ed25519 = PublicKey Ed.PublicKey


instance ByteReprSized (PublicKey Ed25519) where
  type ByteSize (PublicKey Ed25519) = 32
instance ByteReprSized (PrivKey Ed25519) where
  type ByteSize (PrivKey Ed25519) = 32

instance CryptoAsymmetric Ed25519 where
  publicKey   (PrivKey k)   = PublicKey $ Ed.toPublic k
  generatePrivKey = do
    bs <- liftIO $ getEntropy Ed.secretKeySize
    case Ed.secretKey bs of
      CryptoPassed k -> return $! PrivKey k
      CryptoFailed e -> error (show e)

instance ByteReprSized (Signature Ed25519) where
  type ByteSize (Signature Ed25519) = 64

instance CryptoSign Ed25519 where
  signBlob (PrivKey k)  = Signature . convert . Ed.sign k pubKey
   where pubKey = Ed.toPublic k

  verifyBlobSignature (PublicKey pubKey) blob (Signature s)
    = Ed.verify pubKey blob (throwCryptoError $ Ed.signature s)


----------------------------------------------------------------
-- Instances
----------------------------------------------------------------

deriving instance Eq (PrivKey   Ed25519)
deriving instance Eq (PublicKey Ed25519)

-- | WARNING: variable execution time!
instance Ord (PrivKey Ed25519) where
  compare = comparing encodeToBS
instance Ord (PublicKey Ed25519) where
  compare = comparing encodeToBS

deriving instance NFData (PrivKey   Ed25519)
deriving instance NFData (PublicKey Ed25519)

instance ByteRepr (PrivKey Ed25519) where
  decodeFromBS        bs = case Ed.secretKey bs of
    CryptoPassed k -> Just (PrivKey k)
    CryptoFailed _ -> Nothing
  encodeToBS (PrivKey k) = convert k

instance ByteRepr (PublicKey Ed25519) where
  decodeFromBS        bs = case Ed.publicKey bs of
    CryptoPassed k -> Just (PublicKey k)
    CryptoFailed _ -> Nothing
  encodeToBS (PublicKey k) = convert k
