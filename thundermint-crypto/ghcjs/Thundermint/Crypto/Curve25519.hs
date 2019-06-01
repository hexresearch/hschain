{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}
-- |
module Thundermint.Crypto.Curve25519 (
  Curve25519
  ) where

import Control.Monad
import Control.DeepSeq
import Data.ByteString (ByteString)
import Data.Ord        (comparing)
import qualified Data.ByteString      as BS

import GHCJS.Types
import JavaScript.TypedArray

import Thundermint.Crypto
import Thundermint.Crypto.NaCl


----------------------------------------------------------------
--
----------------------------------------------------------------

data Curve25519

data instance PrivKey Curve25519 = PrivKey
  { pkBS  :: !ByteString
  , privK :: !Uint8Array
  , pubK  :: !Uint8Array
  }

newtype instance PublicKey Curve25519 = PublicKey Uint8Array

newtype instance DHSecret Curve25519 = DHSecret Uint8Array

instance ByteReprSized (PublicKey Curve25519) where
  type ByteSize (PublicKey Curve25519) = 32
instance ByteReprSized (PrivKey Curve25519) where
  type ByteSize (PrivKey Curve25519) = 32

instance CryptoAsymmetric Curve25519 where
  publicKey = PublicKey . pubK
  generatePrivKey = do
    arr <- randomBytes 32
    case decodeFromBS $ arrayToBs arr of
      Just k  -> return k
      Nothing -> error "Curve25519: internal error. Cannot generate key"

instance ByteReprSized (DHSecret Curve25519) where
  type ByteSize (DHSecret Curve25519) = 32

instance CryptoDH Curve25519 where
  diffieHelman (PublicKey pub) pk = DHSecret $ js_diffieHelman (privK pk) pub

instance Eq (PrivKey Curve25519) where
  PrivKey b1 _ _ == PrivKey b2 _ _ = b1 == b2

instance Eq (PublicKey Curve25519) where
  PublicKey a == PublicKey b = arrayToBs a == arrayToBs b

instance Eq (DHSecret Curve25519) where
  DHSecret a == DHSecret b = arrayToBs a == arrayToBs b


instance Ord (PrivKey Curve25519) where
  compare = comparing encodeToBS

instance Ord (PublicKey Curve25519) where
  compare = comparing encodeToBS

instance Ord (DHSecret Curve25519) where
  compare = comparing encodeToBS


instance NFData (PrivKey Curve25519) where
  rnf k = k `seq` ()

instance NFData (PublicKey Curve25519) where
  rnf k = k `seq` ()


instance (Ord (PrivKey Curve25519)) => ByteRepr (PrivKey Curve25519) where
  decodeFromBS bs = do
    keypair <- nonNullJs $ js_fromSecretKey $ bsToArray bs
    return PrivKey { pkBS  = bs
                   , privK = js_getSecretKey keypair
                   , pubK  = js_getPublicKey keypair
                   }
  encodeToBS = pkBS

instance (Ord (PublicKey Curve25519)) => ByteRepr (PublicKey Curve25519) where
  decodeFromBS        bs = do
    guard $ BS.length bs == 32
    return $! PublicKey $ bsToArray bs
  encodeToBS (PublicKey k) = arrayToBs k

instance (Ord (PublicKey Curve25519)) => ByteRepr (DHSecret Curve25519) where
  decodeFromBS        bs = do
    guard $ BS.length bs == 32
    return $! DHSecret $ bsToArray bs
  encodeToBS (DHSecret k) = arrayToBs k


----------------------------------------------------------------
-- NaCl Curve25519
----------------------------------------------------------------

foreign import javascript safe "try {$r = nacl.box.keyPair.fromSecretKey($1); } catch(err) { $r = null; }"
  js_fromSecretKey :: Uint8Array -> JSVal

foreign import javascript safe "{ $r = new Uint8Array(32); nacl.lowlevel.crypto_scalarmult($r,$1,$2); }"
  js_diffieHelman :: Uint8Array -> Uint8Array -> Uint8Array

foreign import javascript unsafe "$1.publicKey"
  js_getPublicKey :: JSVal -> Uint8Array

foreign import javascript unsafe "$1.secretKey"
  js_getSecretKey :: JSVal -> Uint8Array
