{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}
module Thundermint.Crypto.Ed25519 (
    Ed25519
  ) where

import Control.Monad
import Control.DeepSeq
import Data.ByteString (ByteString)
import Data.Ord        (comparing)
import qualified Data.ByteString      as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.Digest.Pure.SHA as SHA

import GHCJS.Types
import JavaScript.TypedArray

import Thundermint.Crypto
import Thundermint.Crypto.NaCl

----------------------------------------------------------------
--
----------------------------------------------------------------

data Ed25519

data instance PrivKey Ed25519 = PrivKey
  { pkBS  :: !ByteString
  , privK :: !Uint8Array
  , pubK  :: !Uint8Array
  }

newtype instance PublicKey Ed25519 = PublicKey { unPublicKey :: Uint8Array }

instance ByteReprSized (PublicKey Ed25519) where
  type ByteSize (PublicKey Ed25519) = 32
instance ByteReprSized (PrivKey Ed25519) where
  type ByteSize (PrivKey Ed25519) = 32

instance CryptoAsymmetric Ed25519 where
  publicKey = PublicKey . pubK
  generatePrivKey = do
    arr <- randomBytes 32
    case decodeFromBS $ arrayToBs arr of
      Just k  -> return k
      Nothing -> error "Ed25519: internal error. Cannot generate key"

instance ByteReprSized (Fingerprint Ed25519) where
  type ByteSize (Fingerprint Ed25519) = 32
instance ByteReprSized (Signature Ed25519) where
  type ByteSize (Signature Ed25519) = 64

instance CryptoSign Ed25519 where
  signBlob k bs
    = Signature
    $ arrayToBs
    $ js_sign_detached (bsToArray bs) (privK k)
  verifyBlobSignature (PublicKey pubKey) blob (Signature s)
    = js_sign_detached_verify (bsToArray blob) (bsToArray s) pubKey
  --
  fingerprint   = Fingerprint
                . BL.toStrict . SHA.bytestringDigest . SHA.sha256 . BL.fromStrict
                . arrayToBs . js_sha512 . unPublicKey

instance Eq (PrivKey Ed25519) where
  PrivKey b1 _ _ == PrivKey b2 _ _ = b1 == b2

instance Eq (PublicKey Ed25519) where
  PublicKey a == PublicKey b = arrayToBs a == arrayToBs b

instance Ord (PrivKey Ed25519) where
  compare = comparing encodeToBS

instance Ord (PublicKey Ed25519) where
  compare = comparing encodeToBS


instance NFData (PrivKey Ed25519) where
  rnf k = k `seq` ()

instance NFData (PublicKey Ed25519) where
  rnf k = k `seq` ()

instance (Ord (PrivKey Ed25519)) => ByteRepr (PrivKey Ed25519) where
  decodeFromBS        bs = do
    keypair <- nonNullJs $ js_nacl_sign_fromSeed $ bsToArray bs
    return PrivKey { pkBS  = bs
                   , privK = js_getSecretKey keypair
                   , pubK  = js_getPublicKey keypair
                   }
  encodeToBS = pkBS

instance (Ord (PublicKey Ed25519)) => ByteRepr (PublicKey Ed25519) where
  decodeFromBS        bs = do
    guard $ BS.length bs == 32
    return $! PublicKey $ bsToArray bs

  encodeToBS (PublicKey k) = arrayToBs k

----------------------------------------------------------------
-- NaCl ed25519
----------------------------------------------------------------

foreign import javascript safe "nacl.sign.detached($1, $2)"
  js_sign_detached :: Uint8Array -> Uint8Array -> Uint8Array

foreign import javascript safe "nacl.sign.detached.verify($1, $2, $3)"
  js_sign_detached_verify :: Uint8Array -> Uint8Array -> Uint8Array -> Bool

foreign import javascript unsafe "try { $r = nacl.sign.keyPair.fromSeed($1); } catch (err) { console.log($1); console.log(err); $r = null; }"
  js_nacl_sign_fromSeed :: Uint8Array -> JSVal

foreign import javascript unsafe "$1.publicKey"
  js_getPublicKey :: JSVal -> Uint8Array

foreign import javascript unsafe "$1.secretKey"
  js_getSecretKey :: JSVal -> Uint8Array
