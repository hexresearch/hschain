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
import Data.ByteString       (ByteString)
import Data.Data             (Data)
import Data.Ord              (comparing)

import Thundermint.Crypto

----------------------------------------------------------------
--
----------------------------------------------------------------

-- | Ed25519 public key signature system
data Ed25519 deriving (Data)

newtype instance PrivKey   Ed25519 = PrivKey   ()
newtype instance PublicKey Ed25519 = PublicKey ()


instance ByteReprSized (PublicKey Ed25519) where
  type ByteSize (PublicKey Ed25519) = 32
instance ByteReprSized (PrivKey Ed25519) where
  type ByteSize (PrivKey Ed25519) = 32

instance CryptoAsymmetric Ed25519 where
  publicKey   (PrivKey k)   = undefined
  generatePrivKey = undefined

instance ByteReprSized (Fingerprint Ed25519) where
  type ByteSize (Fingerprint Ed25519) = 32
instance ByteReprSized (Signature Ed25519) where
  type ByteSize (Signature Ed25519) = 64

instance CryptoSign Ed25519 where
  signBlob (PrivKey k)  = undefined
  verifyBlobSignature (PublicKey pubKey) blob (Signature s)
    = undefined
  fingerprint (PublicKey k) = undefined


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

instance (Ord (PrivKey Ed25519)) => ByteRepr (PrivKey Ed25519) where
  decodeFromBS        bs = undefined
  encodeToBS (PrivKey k) = undefined

instance (Ord (PublicKey Ed25519)) => ByteRepr (PublicKey Ed25519) where
  decodeFromBS        bs = undefined
  encodeToBS (PublicKey k) = undefined
