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

import Data.ByteArray          (convert)
import Data.Data               (Data)
import qualified Data.ByteString       as BS
import qualified Data.ByteString.Lazy  as BL
import qualified Crypto.Hash           as Crypto
import Crypto.Hash          (Digest)
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
  hashBlob          = defaultHash @Crypto.SHA1
  hashLazyBlob      = defaultLazyHash @Crypto.SHA1
  hashAlgorithmName = "hash:SHA1"

instance CryptoHMAC SHA1 where
  hmac = defaultHMAC @Crypto.SHA1



-- | SHA256 hash function
data SHA256  deriving (Data)

instance ByteReprSized (Hash SHA256) where
  type ByteSize (Hash SHA256) = 32

instance CryptoHash SHA256 where
  hashBlob          = defaultHash @Crypto.SHA256
  hashLazyBlob      = defaultLazyHash @Crypto.SHA256
  hashAlgorithmName = "hash:SHA256"

instance CryptoHMAC SHA256 where
  hmac = defaultHMAC @Crypto.SHA256



-- | SHA384 hash function
data SHA384  deriving (Data)

instance ByteReprSized (Hash SHA384) where
  type ByteSize (Hash SHA384) = 48

instance CryptoHash SHA384 where
  hashBlob          = defaultHash @Crypto.SHA384
  hashLazyBlob      = defaultLazyHash @Crypto.SHA384
  hashAlgorithmName = "hash:SHA384"

instance CryptoHMAC SHA384 where
  hmac = defaultHMAC @Crypto.SHA384



-- | SHA512 hash function
data SHA512  deriving (Data)

instance ByteReprSized (Hash SHA512) where
  type ByteSize (Hash SHA512) = 64

instance CryptoHash SHA512 where
  hashBlob          = defaultHash @Crypto.SHA512
  hashLazyBlob      = defaultLazyHash @Crypto.SHA512
  hashAlgorithmName = "hash:SHA512"

instance CryptoHMAC SHA512 where
  hmac = defaultHMAC @Crypto.SHA512

 
defaultHash
  :: forall crypto alg. Crypto.HashAlgorithm crypto
  => BS.ByteString -> Hash alg
defaultHash = Hash . convert . id @(Digest crypto) . Crypto.hash

defaultLazyHash
  :: forall crypto alg. Crypto.HashAlgorithm crypto
  => BL.ByteString -> Hash alg
defaultLazyHash = Hash . convert . id @(Digest crypto) . Crypto.hashlazy


defaultHMAC
  :: forall crypto alg. Crypto.HashAlgorithm crypto
  => BS.ByteString -> BS.ByteString -> HMAC alg
defaultHMAC key
  = HMAC . Hash . convert . id @(Crypto.HMAC crypto) . Crypto.hmac key
