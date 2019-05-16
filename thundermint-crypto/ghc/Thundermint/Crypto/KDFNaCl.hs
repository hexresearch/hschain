{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE TypeFamilies       #-}
-- |
module Thundermint.Crypto.KDFNaCl where

import Data.ByteArray        (convert,ScrubbedBytes)
import Unsafe.Coerce

import qualified Data.ByteArray           as Arr
import qualified Data.ByteString          as BS
import qualified Crypto.Cipher.XSalsa     as XSalsa

import Thundermint.Crypto


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
    | otherwise         = KDFOutput kdf
    where
      zeros  = BS.replicate 24 0
      xsalsa = XSalsa.initialize 20 bs zeros
      -- FIXME: Here we coerce newtype wrapper to its representation
      array  = unsafeCoerce xsalsa :: ScrubbedBytes
      chunk1 = Arr.view array 4  16
      chunk2 = Arr.view array 44 16
      kdf    = convert chunk1 <> convert chunk2
