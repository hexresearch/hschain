{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}
module HSChain.Crypto.Ed25519 (
  Ed25519
  ) where

import Control.Monad.IO.Class
import Control.DeepSeq       (NFData(..))
import Crypto.Error          (CryptoFailable(..), throwCryptoError)
import Data.ByteArray        (convert, ByteArrayAccess(..))
import Data.ByteString.Internal (memcmp)
import Data.Data             (Data)
import Data.Ord              (comparing)
import System.Entropy        (getEntropy)
import System.IO.Unsafe      (unsafePerformIO)

import qualified Crypto.PubKey.Ed25519    as Ed

import HSChain.Crypto

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
  asymmKeyAlgorithmName = "Ed25519"

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
  compare pk@(PublicKey k1) (PublicKey k2) =
    unsafePerformIO $
      withByteArray k1 $ \pk1 ->
        withByteArray k2 $ \pk2 -> do
          r <- memcmp pk1 pk2 (publicKeySize pk)
          return $ if r < 0 then LT -- NB: according to `man memcmp`, it returns negative, zero or positive (not necessary -1, 0, 1)
                   else if r > 0 then GT
                   else EQ

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
