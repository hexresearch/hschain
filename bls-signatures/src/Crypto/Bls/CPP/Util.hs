{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
module Crypto.Bls.CPP.Util
    ( hash256
    , hashSize
    , hash256serialize
    ) where


import Data.ByteString (ByteString)
import qualified Data.ByteString.Internal as BS
import qualified Language.C.Inline as C
import qualified Language.C.Inline.Cpp as C
import System.IO.Unsafe

import Crypto.Bls.CPP.Internal
import Crypto.Bls.CPP.Types


C.context blsCtx
C.include "chiabls/bls.hpp"
C.include "<iostream>"
C.using "namespace bls"
C.using "namespace std"


hashSize :: Int
hashSize = 32 -- TODO get from sources!


hash256 :: ByteString -> Hash256
hash256 message = Hash256 $ unsafePerformIO $ BS.create hashSize $ \hashbuffer ->
    [C.exp| void { Util::Hash256($(uint8_t * hashbuffer), (uint8_t const*)$bs-ptr:message, $bs-len:message) }|]


hash256serialize :: Hash256 -> ByteString
hash256serialize (Hash256 bs) = bs
