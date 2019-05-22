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
module Thundermint.Crypto.Curve25519 (
  Curve25519
  ) where

import Control.Monad.IO.Class
import Control.DeepSeq       (NFData(..))
import Data.Data             (Data)
import Data.Ord              (comparing)

import Thundermint.Crypto


----------------------------------------------------------------
--
----------------------------------------------------------------

data Curve25519 deriving (Data)

newtype instance PrivKey   Curve25519 = PrivKey   ()
newtype instance PublicKey Curve25519 = PublicKey ()
newtype instance DHSecret  Curve25519 = DHSecret  ()

instance ByteReprSized (PublicKey Curve25519) where
  type ByteSize (PublicKey Curve25519) = 32
instance ByteReprSized (PrivKey Curve25519) where
  type ByteSize (PrivKey Curve25519) = 32

instance CryptoAsymmetric Curve25519 where
  publicKey (PrivKey k) = undefined
  generatePrivKey = undefined

instance ByteReprSized (DHSecret Curve25519) where
  type ByteSize (DHSecret Curve25519) = 32

instance CryptoDH Curve25519 where
  diffieHelman (PublicKey pub) (PrivKey priv) = undefined


----------------------------------------------------------------
-- Instances
----------------------------------------------------------------

deriving instance Eq (PrivKey   Curve25519)
deriving instance Eq (PublicKey Curve25519)
deriving instance Eq (DHSecret  Curve25519)

-- | WARNING: variable execution time!
instance Ord (PrivKey Curve25519) where
  compare = comparing encodeToBS
instance Ord (PublicKey Curve25519) where
  compare = comparing encodeToBS
-- | WARNING: variable execution time!
instance Ord (DHSecret Curve25519) where
  compare = comparing encodeToBS

deriving instance NFData (PrivKey   Curve25519)
deriving instance NFData (PublicKey Curve25519)
deriving instance NFData (DHSecret  Curve25519)

instance (Ord (PrivKey Curve25519)) => ByteRepr (PrivKey Curve25519) where
  decodeFromBS        bs = undefined
  encodeToBS (PrivKey k) = undefined

instance (Ord (PublicKey Curve25519)) => ByteRepr (PublicKey Curve25519) where
  decodeFromBS        bs = undefined
  encodeToBS (PublicKey k) = undefined

instance ByteRepr (DHSecret Curve25519) where
  decodeFromBS bs = undefined
  encodeToBS (DHSecret k) = undefined
