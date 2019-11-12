{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}
-- |
module HSChain.Crypto.SHA (
    SHA1
  , SHA256
  , SHA384
  , SHA512
  ) where

import Control.Monad.Primitive (unsafeIOToPrim)
import Data.ByteArray          (convert)
import Data.Data               (Data)
import qualified Data.ByteString       as BS
import qualified Crypto.Hash           as Crypto
import Crypto.Hash.IO          (hashMutableInit,hashMutableUpdate,hashMutableFinalize,MutableContext)
import qualified Crypto.MAC.HMAC       as Crypto

import HSChain.Crypto


----------------------------------------------------------------
--
----------------------------------------------------------------

-- | SHA1 hash function
data SHA1  deriving (Data)

instance ByteReprSized (Hash SHA1) where
  type ByteSize (Hash SHA1) = 20

instance CryptoHash SHA1 where
  newtype HashAccum SHA1 s = AccSHA1 (MutableContext Crypto.SHA1)
  newHashAccum                   = unsafeIOToPrim $ fmap AccSHA1 $ hashMutableInit
  updateHashAccum (AccSHA1 s) bs = unsafeIOToPrim $ hashMutableUpdate s bs
  freezeHashAccum (AccSHA1 s)    = Hash . convert <$> unsafeIOToPrim (hashMutableFinalize s)
  hashAlgorithmName              = "hash:SHA1"
instance CryptoHMAC SHA1 where
  hmac = defaultHMAC @Crypto.SHA1



-- | SHA256 hash function
data SHA256  deriving (Data)

instance ByteReprSized (Hash SHA256) where
  type ByteSize (Hash SHA256) = 32

instance CryptoHash SHA256 where
  newtype HashAccum SHA256 s = AccSHA256 (MutableContext Crypto.SHA256)
  newHashAccum                     = unsafeIOToPrim $ fmap AccSHA256 $ hashMutableInit
  updateHashAccum (AccSHA256 s) bs = unsafeIOToPrim $ hashMutableUpdate s bs
  freezeHashAccum (AccSHA256 s)    = Hash . convert <$> unsafeIOToPrim (hashMutableFinalize s)
  hashAlgorithmName                = "hash:SHA256"

instance CryptoHMAC SHA256 where
  hmac = defaultHMAC @Crypto.SHA256



-- | SHA384 hash function
data SHA384  deriving (Data)

instance ByteReprSized (Hash SHA384) where
  type ByteSize (Hash SHA384) = 48

instance CryptoHash SHA384 where
  newtype HashAccum SHA384 s = AccSHA384 (MutableContext Crypto.SHA384)
  newHashAccum                     = unsafeIOToPrim $ fmap AccSHA384 $ hashMutableInit
  updateHashAccum (AccSHA384 s) bs = unsafeIOToPrim $ hashMutableUpdate s bs
  freezeHashAccum (AccSHA384 s)    = Hash . convert <$> unsafeIOToPrim (hashMutableFinalize s)
  hashAlgorithmName                = "hash:SHA384"

instance CryptoHMAC SHA384 where
  hmac = defaultHMAC @Crypto.SHA384



-- | SHA512 hash function
data SHA512  deriving (Data)

instance ByteReprSized (Hash SHA512) where
  type ByteSize (Hash SHA512) = 64

instance CryptoHash SHA512 where
  newtype HashAccum SHA512 s = AccSHA512 (MutableContext Crypto.SHA512)
  newHashAccum                     = unsafeIOToPrim $ fmap AccSHA512 $ hashMutableInit
  updateHashAccum (AccSHA512 s) bs = unsafeIOToPrim $ hashMutableUpdate s bs
  freezeHashAccum (AccSHA512 s)    = Hash . convert <$> unsafeIOToPrim (hashMutableFinalize s)
  hashAlgorithmName                = "hash:SHA512"

instance CryptoHMAC SHA512 where
  hmac = defaultHMAC @Crypto.SHA512



defaultHMAC
  :: forall crypto alg. Crypto.HashAlgorithm crypto
  => BS.ByteString -> BS.ByteString -> HMAC alg
defaultHMAC key
  = HMAC . Hash . convert . id @(Crypto.HMAC crypto) . Crypto.hmac key
