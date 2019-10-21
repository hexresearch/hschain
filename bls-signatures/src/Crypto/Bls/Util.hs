{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
module Crypto.Bls.Util
    ( hash256
    , hashSize
    ) where


import Data.ByteString (ByteString)
import qualified Data.ByteString.Internal as BS
import qualified Language.C.Inline as C
import qualified Language.C.Inline.Cpp as C

import Crypto.Bls.Internal
import Crypto.Bls.Types


C.context blsCtx
C.include "chiabls/bls.hpp"
C.include "<iostream>"
C.using "namespace bls"
C.using "namespace std"


hashSize :: Int
hashSize = 32 -- TODO get from sources!


hash256 :: ByteString -> IO Hash256
hash256 message = fmap Hash256 $ BS.create hashSize $ \hashbuffer ->
    [C.exp| void { Util::Hash256($(uint8_t * hashbuffer), (uint8_t const*)$bs-ptr:message, $bs-len:message) }|]
