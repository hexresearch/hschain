{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances  #-}
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


instance ByteReprSized (Hash SHA1) where
  type ByteSize (Hash SHA1) = 20
instance CryptoHash SHA1 where
  hashBlob                   = Hash . BL.toStrict . SHA.bytestringDigest . SHA.sha1 . BL.fromStrict
  hashEquality (Hash hbs) bs = hbs == bs


-- | SHA256 hash function
data SHA256  deriving (Data)

instance ByteReprSized (Hash SHA256) where
  type ByteSize (Hash SHA256) = 32
instance CryptoHash SHA256 where
  hashBlob                   = Hash . BL.toStrict . SHA.bytestringDigest . SHA.sha256 . BL.fromStrict
  hashEquality (Hash hbs) bs = hbs == bs


-- | SHA384 hash function
data SHA384  deriving (Data)

instance ByteReprSized (Hash SHA384) where
  type ByteSize (Hash SHA384) = 48
instance CryptoHash SHA384 where
  hashBlob                   = Hash . BL.toStrict . SHA.bytestringDigest . SHA.sha384 . BL.fromStrict
  hashEquality (Hash hbs) bs = hbs == bs


-- | SHA512 hash function
data SHA512  deriving (Data)

instance ByteReprSized (Hash SHA512) where
  type ByteSize (Hash SHA512) = 64
instance CryptoHash SHA512 where
  hashBlob  = Hash . arrayToBs . js_sha512 . bsToArray
  hashEquality (Hash hbs) bs = hbs == bs
