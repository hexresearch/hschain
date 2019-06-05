{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}
-- |
module Thundermint.Crypto.KDFNaCl (
  KDFNaCl
  ) where

import qualified Data.ByteString      as BS
import JavaScript.TypedArray

import Thundermint.Crypto
import Thundermint.Crypto.NaCl

----------------------------------------------------------------
--
----------------------------------------------------------------

-- | KDF which is used in TweetNaCl public-key authentificated box.
--
--   It's a bit nasty function since it insists on 32-byte input. 
data KDFNaCl

instance ByteReprSized (KDFOutput KDFNaCl) where
  type ByteSize (KDFOutput KDFNaCl) = 32

instance CryptoKDF KDFNaCl where
  type KDFParams KDFNaCl = ()
  deriveKey () bs
    | BS.length bs < 32 = error "FIXME: don't know waht to do with short key"
    | otherwise         = KDFOutput $ arrayToBs $ js_nacl_hsalsa20 js_0 (bsToArray bs) js_sigma


----------------------------------------------------------------
-- FFI
----------------------------------------------------------------

-- It's not exported so we had to copy it from TweetNaCl sources
foreign import javascript unsafe "new Uint8Array([101, 120, 112, 97, 110, 100, 32, 51, 50, 45, 98, 121, 116, 101, 32, 107])"
  js_sigma :: Uint8Array

foreign import javascript unsafe "new Uint8Array(16)"
  js_0 :: Uint8Array

foreign import javascript safe "{$r = new Uint8Array(32); nacl.lowlevel.crypto_core_hsalsa20($r, $1, $2, $3);}"
  js_nacl_hsalsa20 :: Uint8Array -> Uint8Array -> Uint8Array -> Uint8Array
