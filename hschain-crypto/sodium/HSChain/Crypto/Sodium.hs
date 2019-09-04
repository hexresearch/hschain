{-# LANGUAGE CApiFFI #-}
-- |
module HSChain.Crypto.Sodium (
    check
  , randomBytes
  ) where

import Data.Word
import qualified Data.ByteArray  as Arr
import Foreign.C.Types
import Foreign.Ptr



randomBytes :: Arr.ByteArray arr => Int -> IO arr
randomBytes n = do
  Arr.alloc n $ \p -> randombytes_buf p (fromIntegral n)  

check :: CInt -> IO ()
check 0 = return ()
check _ = error "sodium error"

foreign import capi "sodium.h randombytes_buf" randombytes_buf
  :: Ptr Word8 -> CSize -> IO ()
