{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeFamilies       #-}
-- |
module Thundermint.Crypto.SHA (
    SHA1
  , SHA256
  , SHA384
  , SHA512
  ) where

import Data.Data       (Data)
import qualified Data.ByteString.Lazy as BL
import qualified Data.Digest.Pure.SHA as SHA

import Thundermint.Crypto
import Thundermint.Crypto.NaCl


-- | SHA1 hash function
data SHA1  deriving (Data)

instance CryptoHash SHA1 where
  type HashSize SHA1 = 20
  hashBlob                   = Hash . BL.toStrict . SHA.bytestringDigest . SHA.sha1 . BL.fromStrict
  hashEquality (Hash hbs) bs = hbs == bs


-- | SHA256 hash function
data SHA256  deriving (Data)

instance CryptoHash SHA256 where
  type HashSize SHA256 = 32
  hashBlob                   = Hash . BL.toStrict . SHA.bytestringDigest . SHA.sha256 . BL.fromStrict
  hashEquality (Hash hbs) bs = hbs == bs


-- | SHA384 hash function
data SHA384  deriving (Data)

instance CryptoHash SHA384 where
  type HashSize SHA384 = 48
  hashBlob                   = Hash . BL.toStrict . SHA.bytestringDigest . SHA.sha384 . BL.fromStrict
  hashEquality (Hash hbs) bs = hbs == bs


-- | SHA512 hash function
data SHA512  deriving (Data)

instance CryptoHash SHA512 where
  type HashSize SHA512 = 64
  hashBlob  = Hash . arrayToBs . js_sha512 . bsToArray
  hashEquality (Hash hbs) bs = hbs == bs
