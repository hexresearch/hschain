{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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

import qualified Crypto.Hash           as Crypto

import Thundermint.Crypto


----------------------------------------------------------------
--
----------------------------------------------------------------

-- | SHA1 hash function
data SHA1  deriving (Data)

instance ByteReprSized (Hash SHA1) where
  type ByteSize (Hash SHA1) = 20
instance CryptoHash SHA1 where
  hashBlob                   = Hash . convert . id @(Digest Crypto.SHA1) . Crypto.hash
  hashEquality (Hash hbs) bs = hbs == bs


-- | SHA256 hash function
data SHA256  deriving (Data)

instance ByteReprSized (Hash SHA256) where
  type ByteSize (Hash SHA256) = 32
instance CryptoHash SHA256 where
  hashBlob                   = Hash . convert . id @(Digest Crypto.SHA256) . Crypto.hash
  hashEquality (Hash hbs) bs = hbs == bs


-- | SHA384 hash function
data SHA384  deriving (Data)

instance ByteReprSized (Hash SHA384) where
  type ByteSize (Hash SHA384) = 48
instance CryptoHash SHA384 where
  hashBlob                   = Hash . convert . id @(Digest Crypto.SHA384) . Crypto.hash
  hashEquality (Hash hbs) bs = hbs == bs



-- | SHA512 hash function
data SHA512  deriving (Data)

instance ByteReprSized (Hash SHA512) where
  type ByteSize (Hash SHA512) = 64
instance CryptoHash SHA512 where
  hashBlob                   = Hash . convert . id @(Digest Crypto.SHA512) . Crypto.hash
  hashEquality (Hash hbs) bs = hbs == bs
