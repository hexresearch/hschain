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

import Data.STRef
import Data.Data       (Data)
import Data.Binary.Get (Decoder(..))
import qualified Data.ByteString      as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.Digest.Pure.SHA as SHA

import HSChain.Crypto


-- | SHA1 hash function
data SHA1  deriving (Data)


instance ByteReprSized (Hash SHA1) where
  type ByteSize (Hash SHA1) = 20

instance CryptoHash SHA1 where
  data HashAccum SHA1 s = AccSHA1 !(STRef s Int) !(STRef s (Decoder SHA.SHA1State))
  newHashAccum = do
    cnt <- newSTRef 0
    dec <- newSTRef SHA.sha1Incremental
    return $! AccSHA1 cnt dec
  updateHashAccum (AccSHA1 cnt dec) bs = do
    modifySTRef' cnt (+ BS.length bs)
    modifySTRef' dec $ \case
      Partial cont -> cont (Just bs)
      Fail{}       -> error "Fail constructor encountered"
      Done{}       -> error "Done constructor encountered"
  freezeHashAccum (AccSHA1 cnt dec) = do
    n <- readSTRef cnt
    d <- readSTRef dec
    return $! Hash . BL.toStrict . SHA.bytestringDigest
           $  SHA.completeSha1Incremental d n
  hashAlgorithmName = "hash:SHA1"

instance CryptoHMAC SHA1 where
  hmac = defaultHMAC SHA.hmacSha1


-- | SHA256 hash function
data SHA256  deriving (Data)

instance ByteReprSized (Hash SHA256) where
  type ByteSize (Hash SHA256) = 32

instance CryptoHash SHA256 where
  data HashAccum SHA256 s = AccSHA256 !(STRef s Int) !(STRef s (Decoder SHA.SHA256State))
  newHashAccum = do
    cnt <- newSTRef 0
    dec <- newSTRef SHA.sha256Incremental
    return $! AccSHA256 cnt dec
  updateHashAccum (AccSHA256 cnt dec) bs = do
    modifySTRef' cnt (+ BS.length bs)
    modifySTRef' dec $ \case
      Partial cont -> cont (Just bs)
      Fail{}       -> error "Fail constructor encountered"
      Done{}       -> error "Done constructor encountered"
  freezeHashAccum (AccSHA256 cnt dec) = do
    n <- readSTRef cnt
    d <- readSTRef dec
    return $! Hash . BL.toStrict . SHA.bytestringDigest
           $  SHA.completeSha256Incremental d n
  hashAlgorithmName = "hash:SHA256"


instance CryptoHMAC SHA256 where
  hmac = defaultHMAC SHA.hmacSha256


-- | SHA384 hash function
data SHA384  deriving (Data)

instance ByteReprSized (Hash SHA384) where
  type ByteSize (Hash SHA384) = 48

instance CryptoHash SHA384 where
  data HashAccum SHA384 s = AccSHA384 !(STRef s Int) !(STRef s (Decoder SHA.SHA512State))
  newHashAccum = do
    cnt <- newSTRef 0
    dec <- newSTRef SHA.sha384Incremental
    return $! AccSHA384 cnt dec
  updateHashAccum (AccSHA384 cnt dec) bs = do
    modifySTRef' cnt (+ BS.length bs)
    modifySTRef' dec $ \case
      Partial cont -> cont (Just bs)
      Fail{}       -> error "Fail constructor encountered"
      Done{}       -> error "Done constructor encountered"
  freezeHashAccum (AccSHA384 cnt dec) = do
    n <- readSTRef cnt
    d <- readSTRef dec
    return $! Hash . BL.toStrict . SHA.bytestringDigest
           $  SHA.completeSha384Incremental d n
  hashAlgorithmName = "hash:SHA384"


instance CryptoHMAC SHA384 where
  hmac = defaultHMAC SHA.hmacSha384


-- | SHA512 hash function
data SHA512  deriving (Data)

instance ByteReprSized (Hash SHA512) where
  type ByteSize (Hash SHA512) = 64

instance CryptoHash SHA512 where
  data HashAccum SHA512 s = AccSHA512 !(STRef s Int) !(STRef s (Decoder SHA.SHA512State))
  newHashAccum = do
    cnt <- newSTRef 0
    dec <- newSTRef SHA.sha512Incremental
    return $! AccSHA512 cnt dec
  updateHashAccum (AccSHA512 cnt dec) bs = do
    modifySTRef' cnt (+ BS.length bs)
    modifySTRef' dec $ \case
      Partial cont -> cont (Just bs)
      Fail{}       -> error "Fail constructor encountered"
      Done{}       -> error "Done constructor encountered"
  freezeHashAccum (AccSHA512 cnt dec) = do
    n <- readSTRef cnt
    d <- readSTRef dec
    return $! Hash . BL.toStrict . SHA.bytestringDigest
           $  SHA.completeSha512Incremental d n
  hashAlgorithmName = "hash:SHA512"


instance CryptoHMAC SHA512 where
  hmac = defaultHMAC SHA.hmacSha512


defaultHMAC :: (BL.ByteString -> BL.ByteString -> SHA.Digest a)
            -> BS.ByteString -> BS.ByteString -> HMAC alg
defaultHMAC hmacFun key msg
  = HMAC $ Hash $ BL.toStrict $ SHA.bytestringDigest
  $ hmacFun (BL.fromStrict key) (BL.fromStrict msg)
