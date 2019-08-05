{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
module Crypto.Bls.Util
    ( hash256
    ) where


import Data.Maybe
import Data.ByteString (ByteString)
import Data.Vector (Vector)
import qualified Data.ByteString as BS
import qualified Data.Vector as V
import qualified Data.ByteString.Internal as BS
import qualified Language.C.Inline as C
import qualified Language.C.Inline.Cpp as C

import Crypto.Bls.Internal
import Crypto.Bls.Types
import Crypto.Bls.Arrays


C.context blsCtx
C.include "bls.hpp"
C.include "<iostream>"
C.using "namespace bls"
C.using "namespace std"


-- TODO change 32 to hash size.
hash256 :: ByteString -> IO Hash256
hash256 message = fmap Hash256 $ BS.create 32 $ \hashbuffer ->
    [C.exp| void { Util::Hash256($(uint8_t * hashbuffer), (uint8_t const*)$bs-ptr:message, $bs-len:message) }|]
