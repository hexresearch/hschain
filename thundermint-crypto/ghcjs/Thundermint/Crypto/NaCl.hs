-- |
-- Common utils for wrapping of
module Thundermint.Crypto.NaCl (
    arrayToBs
  , bsToArray
  , nonNullJs
  , randomBytes
    -- * JS foreign calls
  , js_sha512
  ) where

import Control.Monad.IO.Class
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS

import GHCJS.Types
import GHCJS.Buffer
import JavaScript.TypedArray


foreign import javascript safe "nacl.hash($1)"
  js_sha512 :: Uint8Array -> Uint8Array

arrayToBs :: Uint8Array -> ByteString
arrayToBs arr
  -- Note that some intermediate buffer is have size which is multiple
  -- of 8 so we need to pass length explicitly
  = toByteString (byteOffset arr) (Just (JavaScript.TypedArray.length arr))
  $ createFromArrayBuffer
  $ buffer arr

bsToArray :: ByteString -> Uint8Array
bsToArray bs
  = subarray off len $ getUint8Array buf
  where
    (buf,off,len) = fromByteString (BS.copy bs)

nonNullJs :: JSVal -> Maybe JSVal
nonNullJs res
  | isNull res || isUndefined res = Nothing
  | otherwise                     = Just res

randomBytes :: MonadIO m => Int -> m Uint8Array
randomBytes = liftIO . js_randomBytes

foreign import javascript safe "nacl.randomBytes($1)"
  js_randomBytes :: Int -> IO Uint8Array
