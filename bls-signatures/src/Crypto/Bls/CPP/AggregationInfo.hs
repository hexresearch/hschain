{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
module Crypto.Bls.CPP.AggregationInfo
    ( AggregationInfo
    , aggregationInfoFromMsg
    ) where


import Data.ByteString (ByteString)
import qualified Language.C.Inline as C
import qualified Language.C.Inline.Cpp as C

import Crypto.Bls.CPP.Internal
import Crypto.Bls.CPP.Types


C.context blsCtx
C.include "chiabls/bls.hpp"
C.using "namespace bls"


aggregationInfoFromMsg :: PublicKey -> ByteString -> IO AggregationInfo
aggregationInfoFromMsg pubk msg = withPtr pubk $ \pubkptr -> fromPtr [C.exp|
    AggregationInfo* {
        new AggregationInfo(AggregationInfo::FromMsg(*$(PublicKey* pubkptr), (uint8_t const*)$bs-ptr:msg, $bs-len:msg))
    }|]
