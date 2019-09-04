{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE ScopedTypeVariables        #-}

module HSChain.Crypto.Ed25519 (
    Ed25519_SHA512
  , generatePrivKey
  ) where

import Control.Monad.IO.Class
import Control.DeepSeq       (NFData(..))
import Data.Data             (Data)
import Crypto.Ed25519.Pure hiding (PublicKey)
import qualified Crypto.Ed25519.Pure as Ed

-- import HSChain.Crypto (CryptoSign, ByteRepr, CryptoHash, Hash(..), (:&))
import HSChain.Crypto
import Crypto.Random

import qualified Data.Digest.Pure.SHA as SHA
import qualified Data.ByteString.Lazy as BL
----------------------------------------------------------------
--
----------------------------------------------------------------

data Ed25519 deriving (Data)
data SHA512  deriving (Data)

type Ed25519_SHA512 = Ed25519 :& SHA512

newtype instance PrivKey   Ed25519 = PrivKey   Ed.PrivateKey
newtype instance PublicKey Ed25519 = PublicKey Ed.PublicKey

instance CryptoSignPrim Ed25519 where
  type FingerprintSize Ed25519 = 32
  type PublicKeySize   Ed25519 = 32
  type PrivKeySize     Ed25519 = 32
  type SignatureSize   Ed25519 = 64

genKeyIO :: IO (PrivKey Ed25519)
genKeyIO = do
  g :: SystemRandom <- newGenIO
  case generatePrivate g of
    Left _ -> error "Ed25519: internal error. Cannot generate key"
    Right (k,_) -> pure $ PrivKey k

instance CryptoSign Ed25519 where
  --
  signBlob (PrivKey k) bs = let (Sig s) = sign bs k (generatePublic k) in Signature s
  verifyBlobSignature (PublicKey pubKey) blob (Signature s)
    = valid blob pubKey (Sig s)
  --
  publicKey (PrivKey k) = PublicKey $ generatePublic k
  fingerprint (PublicKey k) = Fingerprint
                . BL.toStrict . SHA.bytestringDigest . SHA.sha256 . BL.fromStrict . exportPublic $ k
  generatePrivKey = liftIO genKeyIO

instance CryptoHash SHA512 where
  type HashSize SHA512 = 64
  hashBlob  = Hash . BL.toStrict . SHA.bytestringDigest . SHA.sha512 . BL.fromStrict
  hashEquality (Hash hbs) bs = hbs == bs

instance Eq (PrivKey Ed25519) where
  (PrivKey a) == (PrivKey b) = (exportPrivate a) == (exportPrivate b)

instance Eq (PublicKey Ed25519) where
  (PublicKey a) == (PublicKey b) = (exportPublic a) == (exportPublic b)

instance Ord (PrivKey Ed25519) where
  (PrivKey a) `compare` (PrivKey b) = (exportPrivate a) `compare` (exportPrivate b)

instance Ord (PublicKey Ed25519) where
  (PublicKey a) `compare` (PublicKey b) = (exportPublic a) `compare` (exportPublic b)

instance NFData (PrivKey Ed25519) where
  rnf k = k `seq` ()

instance NFData (PublicKey Ed25519) where
  rnf k = k `seq` ()

instance (Ord (PrivKey Ed25519)) => ByteRepr (PrivKey Ed25519) where
  decodeFromBS = fmap PrivKey . importPrivate
  encodeToBS (PrivKey k) = exportPrivate k

instance (Ord (PublicKey Ed25519)) => ByteRepr (PublicKey Ed25519) where
  decodeFromBS = fmap PublicKey . importPublic
  encodeToBS (PublicKey k) = exportPublic k
