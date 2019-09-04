{-# LANGUAGE CApiFFI            #-}
{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE TypeFamilies       #-}
-- |
module HSChain.Crypto.KDFNaCl (KDFNaCl) where

import Data.Data             (Data)
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

data KDFNaCl
  deriving Data

instance ByteReprSized (KDFOutput KDFNaCl) where
  type ByteSize (KDFOutput KDFNaCl) = 32

instance CryptoKDF KDFNaCl where
  type KDFParams KDFNaCl = ()
  deriveKey () bs
    | BS.length bs < 32 = error "FIXME: don't know waht to do with short key"
    | otherwise         = KDFOutput $ unsafePerformIO $ do
        buf <-
          Arr.alloc 32            $ \pOut  ->
          Arr.withByteArray zeros $ \pZero ->
          Arr.withByteArray bs    $ \pIn   ->
          check =<< crypto_core_hsalsa20 pOut pZero pIn nullPtr
        return $! buf

zeros :: BS.ByteString
zeros = BS.replicate 16 0


----------------------------------------------------------------
-- FFI
----------------------------------------------------------------

foreign import capi "sodium.h crypto_core_hsalsa20" crypto_core_hsalsa20
  :: Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO CInt
