-- |
-- 
module Thundermint.Crypto.NaCl where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS

import GHCJS.Buffer
import JavaScript.TypedArray

foreign import javascript safe "nacl.hash($1)"
  js_sha512 :: Uint8Array -> Uint8Array


arrayToBs :: Uint8Array -> ByteString
arrayToBs arr
  -- Note that some intermediate buffer is have size which is multiple
  -- of 8 so we need to pass length explicitly
  = toByteString 0 (Just (JavaScript.TypedArray.length arr))
  $ createFromArrayBuffer
  $ buffer arr

bsToArray :: ByteString -> Uint8Array
bsToArray bs
  = subarray off len $ getUint8Array buf
  where
    (buf,off,len) = fromByteString (BS.copy bs)
