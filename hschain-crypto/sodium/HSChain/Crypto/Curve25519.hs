{-# LANGUAGE CApiFFI                    #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}
-- |
module HSChain.Crypto.Curve25519 (
  Curve25519
  ) where

import Control.Monad.IO.Class
import Control.DeepSeq       (NFData(..))
import Data.Data             (Data)
import Data.Ord              (comparing)
import Data.Word
import qualified Data.ByteArray  as Arr
import qualified Data.ByteString as BS
import Foreign.C.Types
import Foreign.Ptr
import System.IO.Unsafe

import HSChain.Crypto
import HSChain.Crypto.Sodium


----------------------------------------------------------------
--
----------------------------------------------------------------

data Curve25519 deriving (Data)

data instance PrivKey   Curve25519 = PrivKey
  !Arr.ScrubbedBytes
  !(PublicKey Curve25519)
newtype instance PublicKey Curve25519 = PublicKey BS.ByteString
newtype instance DHSecret  Curve25519 = DHSecret  Arr.ScrubbedBytes

instance ByteReprSized (PublicKey Curve25519) where
  type ByteSize (PublicKey Curve25519) = 32
instance ByteReprSized (PrivKey Curve25519) where
  type ByteSize (PrivKey Curve25519) = 32

instance CryptoAsymmetric Curve25519 where
  publicKey (PrivKey _ k) = k
  generatePrivKey = liftIO $ do
    (pub,priv) <-
      Arr.allocRet (fromIntegral crypto_box_SECRETKEYBYTES) $ \pPriv ->
        Arr.alloc (fromIntegral crypto_box_PUBLICKEYBYTES) $ \pPub  ->
          check =<< crypto_box_keypair pPub pPriv
    return $! PrivKey priv (PublicKey pub)
  asymmKeyAlgorithmName = "Curve25519"



instance ByteReprSized (DHSecret Curve25519) where
  type ByteSize (DHSecret Curve25519) = 32

instance CryptoDH Curve25519 where
  diffieHelman (PublicKey pub) (PrivKey priv _) = unsafePerformIO $ do
    sec <- Arr.withByteArray priv $ \pPriv ->
      Arr.withByteArray pub $ \pPub ->
        Arr.alloc 32 $ \pSec ->
          check =<< crypto_scalarmult_curve25519 pSec pPriv pPub
    return $! DHSecret sec

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

instance NFData (PrivKey Curve25519) where
  rnf k = k `seq` ()
deriving instance NFData (PublicKey Curve25519)
deriving instance NFData (DHSecret  Curve25519)

instance (Ord (PrivKey Curve25519)) => ByteRepr (PrivKey Curve25519) where
  decodeFromBS bs
    | BS.length bs == 32 = Just $! unsafePerformIO $ do
        let priv = Arr.convert bs
        pub <- Arr.alloc (fromIntegral crypto_box_PUBLICKEYBYTES) $ \pPub ->
          Arr.withByteArray priv $ \pPriv ->
            check =<< crypto_scalarmult_curve25519_base pPub pPriv
        return $! PrivKey priv (PublicKey pub)
    | otherwise = Nothing
  --
  encodeToBS (PrivKey bs _) = Arr.convert bs

instance (Ord (PublicKey Curve25519)) => ByteRepr (PublicKey Curve25519) where
  decodeFromBS bs
    | BS.length bs == 32 = Just $ PublicKey bs
    | otherwise          = Nothing
  encodeToBS (PublicKey k) = k

instance ByteRepr (DHSecret Curve25519) where
  decodeFromBS bs
    | BS.length bs == 32 = Just $! DHSecret $ Arr.convert bs
    | otherwise          = Nothing
  encodeToBS (DHSecret k) = Arr.convert k


----------------------------------------------------------------
--
----------------------------------------------------------------
foreign import capi "sodium.h crypto_box_keypair"
     crypto_box_keypair
  :: Ptr Word8 -> Ptr Word8 -> IO CInt

foreign import capi "sodium.h crypto_scalarmult_curve25519_base"
     crypto_scalarmult_curve25519_base
  :: Ptr Word8 -> Ptr Word8 -> IO CInt

foreign import capi "sodium.h crypto_scalarmult_curve25519"
     crypto_scalarmult_curve25519
  :: Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO CInt

foreign import capi "sodium.h value crypto_box_SECRETKEYBYTES" crypto_box_SECRETKEYBYTES :: CInt
foreign import capi "sodium.h value crypto_box_PUBLICKEYBYTES" crypto_box_PUBLICKEYBYTES :: CInt
