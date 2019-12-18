{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TypeFamilies       #-}
-- |
module HSChain.Crypto.SHA (
    SHA1
  , SHA256
  , SHA384
  , SHA512
  ) where

import Data.Data       (Data)
import qualified Data.ByteString      as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.Digest.Pure.SHA as SHA

import HSChain.Crypto


-- | SHA1 hash function
data SHA1  deriving (Data)


instance ByteReprSized (Hash SHA1) where
  type ByteSize (Hash SHA1) = 20

instance CryptoHash SHA1 where
  hashBlob          = defaultHash     SHA.sha1
  hashLazyBlob      = defaultLazyHash SHA.sha1 
  hashAlgorithmName = "SHA1"

instance CryptoHMAC SHA1 where
  hmac = defaultHMAC SHA.hmacSha1


-- | SHA256 hash function
data SHA256  deriving (Data)

instance ByteReprSized (Hash SHA256) where
  type ByteSize (Hash SHA256) = 32

instance CryptoHash SHA256 where
  hashBlob          = defaultHash     SHA.sha256
  hashLazyBlob      = defaultLazyHash SHA.sha256
  hashAlgorithmName = "SHA256"

instance CryptoHMAC SHA256 where
  hmac = defaultHMAC SHA.hmacSha256


-- | SHA384 hash function
data SHA384  deriving (Data)

instance ByteReprSized (Hash SHA384) where
  type ByteSize (Hash SHA384) = 48

instance CryptoHash SHA384 where
  hashBlob          = defaultHash     SHA.sha384
  hashLazyBlob      = defaultLazyHash SHA.sha384
  hashAlgorithmName = "SHA384"

instance CryptoHMAC SHA384 where
  hmac = defaultHMAC SHA.hmacSha384


-- | SHA512 hash function
data SHA512  deriving (Data)

instance ByteReprSized (Hash SHA512) where
  type ByteSize (Hash SHA512) = 64

instance CryptoHash SHA512 where
  hashBlob          = defaultHash     SHA.sha512
  hashLazyBlob      = defaultLazyHash SHA.sha512
  hashAlgorithmName = "SHA512"

instance CryptoHMAC SHA512 where
  hmac = defaultHMAC SHA.hmacSha512


defaultHash :: (BL.ByteString -> SHA.Digest a) -> BS.ByteString -> Hash alg
defaultHash hashFun
  = Hash . BL.toStrict . SHA.bytestringDigest . hashFun . BL.fromStrict

defaultLazyHash :: (BL.ByteString -> SHA.Digest a) -> BL.ByteString -> Hash alg
defaultLazyHash hashFun
  = Hash . BL.toStrict . SHA.bytestringDigest . hashFun

defaultHMAC :: (BL.ByteString -> BL.ByteString -> SHA.Digest a)
            -> BS.ByteString -> BS.ByteString -> HMAC alg
defaultHMAC hmacFun key msg
  = HMAC $ Hash $ BL.toStrict $ SHA.bytestringDigest
  $ hmacFun (BL.fromStrict key) (BL.fromStrict msg)
