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

instance CryptoHash SHA1 where
  type HashSize SHA1 = 20
  hashBlob                   = Hash . convert . id @(Digest Crypto.SHA1) . Crypto.hash
  hashEquality (Hash hbs) bs = hbs == bs


-- | SHA256 hash function
data SHA256  deriving (Data)

instance CryptoHash SHA256 where
  type HashSize SHA256 = 32
  hashBlob                   = Hash . convert . id @(Digest Crypto.SHA256) . Crypto.hash
  hashEquality (Hash hbs) bs = hbs == bs


-- | SHA384 hash function
data SHA384  deriving (Data)

instance CryptoHash SHA384 where
  type HashSize SHA384 = 48
  hashBlob                   = Hash . convert . id @(Digest Crypto.SHA384) . Crypto.hash
  hashEquality (Hash hbs) bs = hbs == bs



-- | SHA512 hash function
data SHA512  deriving (Data)

instance CryptoHash SHA512 where
  type HashSize SHA512 = 64
  hashBlob                   = Hash . convert . id @(Digest Crypto.SHA512) . Crypto.hash
  hashEquality (Hash hbs) bs = hbs == bs
