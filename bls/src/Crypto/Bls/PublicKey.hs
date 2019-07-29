{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
module Crypto.Bls.PublicKey
    ( PublicKey
    -- , fromSeed
    ) where


import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Language.C.Inline as C
import qualified Language.C.Inline.Cpp as C

import Crypto.Bls.Internal
import Crypto.Bls.Types


C.context blsCtx
C.include "bls.hpp"
C.using "namespace bls"



--fromSeed :: ByteString -> IO PrivateKey
--fromSeed seed = fromPtr [C.exp|
--    PrivateKey * {
--        new PrivateKey(PrivateKey::FromSeed((uint8_t const*)$bs-ptr:seed, $bs-len:seed))
--    }|]


