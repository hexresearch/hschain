{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}
module Thundermint.Crypto.Ed25519 (
    Ed25519_SHA512
  , generatePrivKey
  ) where

import Control.Monad
import Control.DeepSeq
import Data.ByteString (ByteString)
import Data.Ord        (comparing)
import qualified Data.ByteString      as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.Digest.Pure.SHA as SHA

import GHCJS.Buffer
import GHCJS.Types
import JavaScript.TypedArray

import Thundermint.Crypto

----------------------------------------------------------------
--
----------------------------------------------------------------

-- sha256 :: ByteString -> ByteString
-- sha256 = convert . id @(Digest SHA256) . Crypto.hash

data Ed25519
data SHA512

type Ed25519_SHA512 = Ed25519 :& SHA512

data instance PrivKey (Ed25519 :& hash) = PrivKey
  { pkBS  :: !ByteString
  , privK :: !Uint8Array
  , pubK  :: !Uint8Array
  }

newtype instance PublicKey (Ed25519 :& hash) = PublicKey { unPublicKey :: Uint8Array }

instance CryptoSignPrim (Ed25519 :& hash) where
  type FingerprintSize (Ed25519 :& hash) = 32
  type PublicKeySize   (Ed25519 :& hash) = 32
  type PrivKeySize     (Ed25519 :& hash) = 32
  type SignatureSize   (Ed25519 :& hash) = 64


instance CryptoSign (Ed25519 :& hash) where
  --
  signBlob k bs
    = Signature
    $ arrayToBs
    $ js_sign_detached (bsToArray bs) (privK k)
  verifyBlobSignature (PublicKey pubKey) blob (Signature s)
    = js_sign_detached_verify (bsToArray blob) (bsToArray s) pubKey
  --
  publicKey = PublicKey . pubK
  fingerprint   = Address
                . BL.toStrict . SHA.bytestringDigest . SHA.sha256 . BL.fromStrict
                . arrayToBs . js_sha512 . unPublicKey

instance CryptoHash (sign :& SHA512) where

  hashBlob  = Hash . arrayToBs . js_sha512 . bsToArray

  type HashSize     (sign :& SHA512) = 64

generatePrivKey :: IO (PrivKey (Ed25519 :& hash))
generatePrivKey = do
  arr <- js_randombytes 32
  case privKeyFromBS $ arrayToBs arr of
    Just k  -> return k
    Nothing -> error "Ed25519: internal error. Cannot generate key"


instance Eq (PrivKey (Ed25519 :& hash)) where
  PrivKey b1 _ _ == PrivKey b2 _ _ = b1 == b2

instance Eq (PublicKey (Ed25519 :& hash)) where
  PublicKey a == PublicKey b = arrayToBs a == arrayToBs b

instance Ord (PrivKey (Ed25519 :& hash)) where
  compare = comparing encodeToBS

instance Ord (PublicKey (Ed25519 :& hash)) where
  compare = comparing encodeToBS


instance NFData (PrivKey (Ed25519 :& hash)) where
  rnf k = k `seq` ()

instance NFData (PublicKey (Ed25519 :& hash)) where
  rnf k = k `seq` ()

instance (Ord (PrivKey (Ed25519 :& hash))) => ByteRepr (PrivKey (Ed25519 :& hash)) where
  decodeFromBS        bs = do
    keypair <- nonNullJs $ js_nacl_sign_fromSeed $ bsToArray bs
    return PrivKey { pkBS  = bs
                   , privK = js_getSecretKey keypair
                   , pubK  = js_getPublicKey keypair
                   }

  encodeToBS = pkBS

instance (Ord (PublicKey (Ed25519 :& hash))) => ByteRepr (PublicKey (Ed25519 :& hash)) where
  decodeFromBS        bs = do
    guard $ BS.length bs == 32
    return $ PublicKey $ bsToArray bs

  encodeToBS (PublicKey k) = arrayToBs k

----------------------------------------------------------------
-- NaCl ed25519
----------------------------------------------------------------

arrayToBs :: Uint8Array -> ByteString
arrayToBs arr
  -- Note that some intermediate buffer is have size which is multiple
  -- of 8 so we need to pass length explicitly
  = toByteString 0 (Just (JavaScript.TypedArray.length arr))
  $ createFromArrayBuffer
  $ buffer arr

bsToArray :: ByteString -> Uint8Array
bsToArray bs
  = subarray off len $ getUint8Array buf
  where
    (buf,off,len) = fromByteString (BS.copy bs)


nonNullJs :: JSVal -> Maybe JSVal
nonNullJs res
  | isNull res || isUndefined res = Nothing
  | otherwise                     = Just res


foreign import javascript safe "nacl.hash($1)"
  js_sha512 :: Uint8Array -> Uint8Array

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

foreign import javascript safe "nacl.randomBytes($1)"
  js_randombytes :: Int -> IO Uint8Array
