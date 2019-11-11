{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
module Crypto.Bls.AggregationInfo
    ( AggregationInfo
    , fromMsg
    ) where


import Data.ByteString (ByteString)
import qualified Language.C.Inline as C
import qualified Language.C.Inline.Cpp as C

import Crypto.Bls.Internal
import Crypto.Bls.Types


C.context blsCtx
C.include "chiabls/bls.hpp"
C.using "namespace bls"


fromMsg :: PublicKey -> ByteString -> IO AggregationInfo
fromMsg pubk msg = withPtr pubk $ \pubkptr -> fromPtr [C.exp|
    AggregationInfo* {
        new AggregationInfo(AggregationInfo::FromMsg(*$(PublicKey* pubkptr), (uint8_t const*)$bs-ptr:msg, $bs-len:msg))
    }|]
