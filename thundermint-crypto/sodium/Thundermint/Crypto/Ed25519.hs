{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}
module Thundermint.Crypto.Ed25519 (
  Ed25519
  ) where

import Control.Monad.IO.Class
import Control.DeepSeq       (NFData(..))
import Data.Data             (Data)
import Data.Ord              (comparing)
import Data.Word
import qualified Data.ByteArray  as Arr
import qualified Data.ByteString as BS
import Foreign.C.Types
import Foreign.Marshal.Alloc
import Foreign.Ptr
import Foreign.Storable
import System.IO.Unsafe

import Thundermint.Crypto
import Thundermint.Crypto.Sodium

----------------------------------------------------------------
--
----------------------------------------------------------------

-- | Ed25519 public key signature system
data Ed25519 deriving (Data)

data instance PrivKey Ed25519 = PrivKey
  !Arr.ScrubbedBytes
  !(PublicKey Ed25519)
newtype instance PublicKey Ed25519 = PublicKey BS.ByteString


instance ByteReprSized (PublicKey Ed25519) where
  type ByteSize (PublicKey Ed25519) = 32
instance ByteReprSized (PrivKey Ed25519) where
  type ByteSize (PrivKey Ed25519) = 32

instance CryptoAsymmetric Ed25519 where
  publicKey (PrivKey _ k) = k
  generatePrivKey = liftIO $ do
    (pub,priv) <-
      Arr.allocRet (fromIntegral crypto_sign_SECRETKEYBYTES) $ \pPriv ->
        Arr.alloc (fromIntegral crypto_sign_PUBLICKEYBYTES) $ \pPub  ->
          check =<< sodium_crypto_sign_keypair pPub pPriv
    return $! PrivKey priv (PublicKey pub)

instance ByteReprSized (Signature Ed25519) where
  type ByteSize (Signature Ed25519) = 64

instance CryptoSign Ed25519 where
  signBlob (PrivKey pk _) bs = Signature $ unsafePerformIO $ do
    Arr.alloc 64 $ \pSig -> 
      Arr.withByteArray pk $ \pPK ->
        Arr.withByteArray bs $ \pMsg ->
          alloca $ \pLen -> do
            poke pLen 64
            check =<< crypto_sign_detached
              pSig pLen
              pMsg (fromIntegral $ Arr.length bs)
              pPK
  --
  verifyBlobSignature (PublicKey pubKey) blob (Signature s) = unsafePerformIO $ do
    Arr.withByteArray pubKey   $ \pPK  ->
      Arr.withByteArray s      $ \pSig ->
        Arr.withByteArray blob $ \pMsg -> do
          r <- crypto_sign_verify_detached
            pSig
            pMsg (fromIntegral $ Arr.length blob)
            pPK
          return $! r == 0


----------------------------------------------------------------
-- Instances
----------------------------------------------------------------

deriving instance Eq (PrivKey   Ed25519)
deriving instance Eq (PublicKey Ed25519)

-- | WARNING: variable execution time!
instance Ord (PrivKey Ed25519) where
  compare = comparing encodeToBS
instance Ord (PublicKey Ed25519) where
  compare = comparing encodeToBS

instance NFData (PrivKey Ed25519) where
  rnf x = x `seq` ()
deriving instance NFData (PublicKey Ed25519)

instance (Ord (PrivKey Ed25519)) => ByteRepr (PrivKey Ed25519) where
  decodeFromBS bs
    | BS.length bs == 32 = Just $! unsafePerformIO $ do
        (pub,priv) <-
          Arr.allocRet (fromIntegral crypto_sign_SECRETKEYBYTES) $ \pPriv ->
            Arr.alloc (fromIntegral crypto_sign_PUBLICKEYBYTES) $ \pPub  ->
              Arr.withByteArray bs $ \pSeed ->
                check =<< sodium_crypto_sign_seed_keypair pPub pPriv pSeed
        return $! PrivKey priv (PublicKey pub)
    | otherwise = Nothing
  encodeToBS (PrivKey bs _)
    = BS.take 32 $ Arr.convert bs

instance (Ord (PublicKey Ed25519)) => ByteRepr (PublicKey Ed25519) where
  decodeFromBS bs
    | BS.length bs == 32 = Just $ PublicKey bs
    | otherwise          = Nothing
  encodeToBS (PublicKey k) = k


----------------------------------------------------------------
-- C API bindings
----------------------------------------------------------------

foreign import capi "sodium.h crypto_sign_keypair" sodium_crypto_sign_keypair
  :: Ptr Word8 -> Ptr Word8 -> IO CInt

foreign import capi "sodium.h crypto_sign_seed_keypair" sodium_crypto_sign_seed_keypair
  :: Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO CInt

foreign import capi "sodium.h crypto_sign_detached" crypto_sign_detached
  :: Ptr Word8 -> Ptr CULLong   -- Signature
  -> Ptr Word8 -> CULLong       -- message length
  -> Ptr Word8                  -- Secret key
  -> IO CInt

foreign import capi "sodium.h crypto_sign_verify_detached" crypto_sign_verify_detached
  :: Ptr Word8                  -- Signature
  -> Ptr Word8 -> CULLong       -- Message
  -> Ptr Word8                  -- Public key
  -> IO CInt

foreign import capi "sodium.h value crypto_sign_PUBLICKEYBYTES" crypto_sign_PUBLICKEYBYTES :: CInt
foreign import capi "sodium.h value crypto_sign_SECRETKEYBYTES" crypto_sign_SECRETKEYBYTES :: CInt
