{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}
-- |
module Thundermint.Crypto.SHA (
    SHA1
  , SHA256
  , SHA384
  , SHA512
  ) where


import Crypto.Hash           (Digest)
import Data.ByteArray        (convert)
import Data.Data             (Data)
import qualified Data.ByteString       as BS
import qualified Crypto.Hash           as Crypto
import qualified Crypto.MAC.HMAC       as Crypto

import Thundermint.Crypto


----------------------------------------------------------------
--
----------------------------------------------------------------

-- | SHA1 hash function
data SHA1  deriving (Data)

instance ByteReprSized (Hash SHA1) where
  type ByteSize (Hash SHA1) = 20

instance CryptoHash SHA1 where
  hashBlob                   = defaultHash @Crypto.SHA1
  hashEquality (Hash hbs) bs = hbs == bs

instance CryptoHMAC SHA1 where
  hmac = defaultHMAC @Crypto.SHA1



-- | SHA256 hash function
data SHA256  deriving (Data)

instance ByteReprSized (Hash SHA256) where
  type ByteSize (Hash SHA256) = 32

instance CryptoHash SHA256 where
  hashBlob                   = defaultHash @Crypto.SHA256
  hashEquality (Hash hbs) bs = hbs == bs

instance CryptoHMAC SHA256 where
  hmac = defaultHMAC @Crypto.SHA256



-- | SHA384 hash function
data SHA384  deriving (Data)

instance ByteReprSized (Hash SHA384) where
  type ByteSize (Hash SHA384) = 48

instance CryptoHash SHA384 where
  hashBlob                   = defaultHash @Crypto.SHA384
  hashEquality (Hash hbs) bs = hbs == bs

instance CryptoHMAC SHA384 where
  hmac = defaultHMAC @Crypto.SHA384



-- | SHA512 hash function
data SHA512  deriving (Data)

instance ByteReprSized (Hash SHA512) where
  type ByteSize (Hash SHA512) = 64

instance CryptoHash SHA512 where
  hashBlob                   = defaultHash @Crypto.SHA512
  hashEquality (Hash hbs) bs = hbs == bs

instance CryptoHMAC SHA512 where
  hmac = defaultHMAC @Crypto.SHA512



defaultHash
  :: forall crypto alg. Crypto.HashAlgorithm crypto
  => BS.ByteString -> Hash alg
defaultHash = Hash . convert . id @(Digest crypto) . Crypto.hash

defaultHMAC
  :: forall crypto alg. Crypto.HashAlgorithm crypto
  => BS.ByteString -> BS.ByteString -> HMAC alg
defaultHMAC key
  = HMAC . Hash . convert . id @(Crypto.HMAC crypto) . Crypto.hmac key
