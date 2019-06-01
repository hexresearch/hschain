{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
-- |
module Thundermint.Crypto.Salsa20Poly1305 (
  Salsa20Poly1305
  ) where

import Control.DeepSeq (NFData(..))
import Data.Data       (Data)
import qualified Data.ByteString as BS

import GHCJS.Types
import JavaScript.TypedArray

import Thundermint.Crypto
import Thundermint.Crypto.NaCl

----------------------------------------------------------------
--
----------------------------------------------------------------

data Salsa20Poly1305
  deriving (Data)

newtype instance CypherKey Salsa20Poly1305 = Key Uint8Array

newtype instance CypherNonce Salsa20Poly1305 = Nonce Uint8Array

instance ByteReprSized (CypherKey Salsa20Poly1305) where
  type ByteSize (CypherKey Salsa20Poly1305) = 32 
instance ByteReprSized (CypherNonce Salsa20Poly1305) where
  type ByteSize (CypherNonce Salsa20Poly1305) = 24

instance StreamCypher Salsa20Poly1305 where
  encryptMessage (Key key) (Nonce nonce) msg
    = arrayToBs
    $ js_nacl_secretbox (bsToArray msg) nonce key
  --
  decryptMessage (Key key) (Nonce nonce) secretMsg = do
    let res = js_nacl_secretbox_open (bsToArray secretMsg) nonce key
    arr <- nonNullJs res
    return $! arrayToBs $! js_asUint8Arr arr
  --
  generateCypherKey   = Key   <$> randomBytes 32
  generateCypherNonce = Nonce <$> randomBytes 24
    

instance ByteRepr (CypherKey Salsa20Poly1305) where
  decodeFromBS bs
    | BS.length bs == 32 = Just $! Key $ bsToArray bs
    | otherwise          = Nothing
  encodeToBS (Key k) = arrayToBs k

instance ByteRepr (CypherNonce Salsa20Poly1305) where
  decodeFromBS bs
    | BS.length bs == 24 = Just $! Nonce $ bsToArray bs
    | otherwise          = Nothing
  encodeToBS (Nonce k) = arrayToBs k


instance NFData (CypherKey Salsa20Poly1305) where
  rnf (Key x) = x `seq` ()
instance NFData (CypherNonce Salsa20Poly1305) where
  rnf (Nonce x) = x `seq` ()

instance Eq (CypherKey Salsa20Poly1305) where
  Key x == Key y = arrayToBs x == arrayToBs y
instance Eq (CypherNonce Salsa20Poly1305) where
  Nonce x == Nonce y = arrayToBs x == arrayToBs y

----------------------------------------------------------------
-- NaCl secretbox
----------------------------------------------------------------

foreign import javascript safe "nacl.secretbox($1,$2,$3)"
  js_nacl_secretbox :: Uint8Array -> Uint8Array -> Uint8Array -> Uint8Array

foreign import javascript safe "nacl.secretbox.open($1,$2,$3)"
  js_nacl_secretbox_open :: Uint8Array -> Uint8Array -> Uint8Array -> JSVal

foreign import javascript unsafe "{$r = $1}"
  js_asUint8Arr :: JSVal -> Uint8Array
