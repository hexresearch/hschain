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
import Crypto.Error          (CryptoFailable(..))
import Data.ByteArray        (convert)
import Data.Data             (Data)
import Data.Ord              (comparing)
import System.Entropy        (getEntropy)

import qualified Crypto.PubKey.Curve25519 as C25519

import Thundermint.Crypto


----------------------------------------------------------------
--
----------------------------------------------------------------

data Curve25519 deriving (Data)

newtype instance PrivKey   Curve25519 = PrivKey   C25519.SecretKey
newtype instance PublicKey Curve25519 = PublicKey C25519.PublicKey
newtype instance DHSecret  Curve25519 = DHSecret  C25519.DhSecret

instance ByteReprSized (PublicKey Curve25519) where
  type ByteSize (PublicKey Curve25519) = 32
instance ByteReprSized (PrivKey Curve25519) where
  type ByteSize (PrivKey Curve25519) = 32

instance CryptoAsymmetric Curve25519 where
  publicKey (PrivKey k) = PublicKey $ C25519.toPublic k
  generatePrivKey = do
    bs <- liftIO $ getEntropy 32
    case C25519.secretKey bs of
      CryptoPassed k -> return $! PrivKey k
      CryptoFailed e -> error (show e)

instance ByteReprSized (DHSecret Curve25519) where
  type ByteSize (DHSecret Curve25519) = 32

instance CryptoDH Curve25519 where
  diffieHelman (PublicKey pub) (PrivKey priv) = DHSecret $ C25519.dh pub priv


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

instance ByteRepr (PrivKey Curve25519) where
  decodeFromBS        bs = case C25519.secretKey bs of
    CryptoPassed k -> Just (PrivKey k)
    CryptoFailed _ -> Nothing
  encodeToBS (PrivKey k) = convert k

instance ByteRepr (PublicKey Curve25519) where
  decodeFromBS        bs = case C25519.publicKey bs of
    CryptoPassed k -> Just (PublicKey k)
    CryptoFailed _ -> Nothing
  encodeToBS (PublicKey k) = convert k

instance ByteRepr (DHSecret Curve25519) where
  decodeFromBS bs = case C25519.dhSecret bs of
    CryptoPassed k -> Just (DHSecret k)
    CryptoFailed _ -> Nothing
  encodeToBS (DHSecret k) = convert k
