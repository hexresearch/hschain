{-# LANGUAGE CApiFFI                    #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeFamilies               #-}
-- |
module HSChain.Crypto.Salsa20Poly1305 (Salsa20Poly1305) where

import Control.Monad.IO.Class
import Control.DeepSeq (NFData(..))
import Data.Data       (Data)
import Data.Word
import qualified Data.ByteString as BS
import qualified Data.ByteArray  as Arr
import Foreign.C.Types
import Foreign.Ptr
import System.IO.Unsafe

import HSChain.Crypto
import HSChain.Crypto.Sodium


----------------------------------------------------------------
--
----------------------------------------------------------------

data Salsa20Poly1305
  deriving (Data)

newtype instance CypherKey Salsa20Poly1305 = Key Arr.ScrubbedBytes
  deriving newtype (Eq, Ord, NFData)

newtype instance CypherNonce Salsa20Poly1305 = Nonce Arr.ScrubbedBytes
  deriving newtype (Eq, Ord, NFData)

instance ByteReprSized (CypherKey Salsa20Poly1305) where
  type ByteSize (CypherKey Salsa20Poly1305) = 32
instance ByteReprSized (CypherNonce Salsa20Poly1305) where
  type ByteSize (CypherNonce Salsa20Poly1305) = 24


instance StreamCypher Salsa20Poly1305 where
  encryptMessage (Key key) (Nonce nonce) msg = unsafePerformIO $ do
    buf <-
      Arr.withByteArray key   $ \pKey    ->
      Arr.withByteArray nonce $ \pNonce  ->
      Arr.withByteArray msg   $ \pMsg    ->
      Arr.alloc         lenC  $ \pCypher ->
        check =<< crypto_secretbox_easy pCypher pMsg lenM pNonce pKey
    return buf
    where
      lenM,lenC :: Integral i => i
      lenM = fromIntegral $ Arr.length msg
      lenC = lenM + fromIntegral crypto_secretbox_MACBYTES
  --
  decryptMessage (Key key) (Nonce nonce) secretMsg = unsafePerformIO $ do
    -- let n = fromIntegral $ Arr.length secretMsg
    --     m = fromIntegral (Arr.length secretMsg)
    --       - fromIntegral crypto_secretbox_MACBYTES
    (r,buf) <-
      Arr.withByteArray key       $ \pKey    ->
      Arr.withByteArray nonce     $ \pNonce  ->
      Arr.withByteArray secretMsg $ \pCypher ->
      Arr.allocRet      lenM      $ \pMsg    ->
        crypto_secretbox_open_easy pMsg pCypher lenC pNonce pKey
    case r of
      0 -> return $! Just $! buf
      _ -> return Nothing
    where
      lenC,lenM :: Integral i => i
      lenC = fromIntegral $ Arr.length secretMsg
      lenM = lenC - fromIntegral crypto_secretbox_MACBYTES
  --
  generateCypherKey   = liftIO $ do
    buf <- randomBytes $ fromIntegral crypto_secretbox_KEYBYTES
    return $! Key buf
  generateCypherNonce = liftIO $ do
    buf <- randomBytes $ fromIntegral crypto_secretbox_NONCEBYTES
    return $! Nonce buf


instance ByteRepr (CypherKey Salsa20Poly1305) where
  decodeFromBS bs
    | BS.length bs == 32 = Just $! Key $ Arr.convert bs
    | otherwise          = Nothing
  encodeToBS (Key k) = Arr.convert k

instance ByteRepr (CypherNonce Salsa20Poly1305) where
  decodeFromBS bs
    | BS.length bs == 24 = Just $! Nonce $ Arr.convert bs
    | otherwise          = Nothing
  encodeToBS (Nonce k) = Arr.convert k


----------------------------------------------------------------
--
----------------------------------------------------------------

foreign import capi "sodium.h crypto_secretbox_easy" crypto_secretbox_easy
  :: Ptr Word8 -> Ptr Word8 -> CULLong -> Ptr Word8 -> Ptr Word8 -> IO CInt

foreign import capi "sodium.h crypto_secretbox_open_easy" crypto_secretbox_open_easy
  :: Ptr Word8 -> Ptr Word8 -> CULLong -> Ptr Word8 -> Ptr Word8 -> IO CInt

foreign import capi "sodium.h value crypto_secretbox_KEYBYTES"   crypto_secretbox_KEYBYTES   :: CInt
foreign import capi "sodium.h value crypto_secretbox_NONCEBYTES" crypto_secretbox_NONCEBYTES :: CInt
foreign import capi "sodium.h value crypto_secretbox_MACBYTES"   crypto_secretbox_MACBYTES   :: CInt
