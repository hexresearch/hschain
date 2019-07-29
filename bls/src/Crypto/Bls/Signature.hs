{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
module Crypto.Bls.Signature
    ( Signature
    , copy
    , unsafeSetAggregationInfo
    , setAggregationInfo
    , verify
    ) where


import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Language.C.Inline as C
import qualified Language.C.Inline.Cpp as C
import qualified Language.C.Inline.Unsafe as CU
import Foreign.Marshal.Utils (toBool)

import Crypto.Bls.Internal
import Crypto.Bls.Types



C.context blsCtx
C.include "bls.hpp"
C.using "namespace bls"



-- | Create a deep copy of object
--
copy :: Signature -> IO Signature
copy sig = withPtr sig $ \sigptr -> fromPtr [C.exp|
    Signature * {
        new Signature(*$(Signature* sigptr))
    }|]


unsafeSetAggregationInfo :: Signature -> AggregationInfo -> IO ()
unsafeSetAggregationInfo sig ai =
    withPtr sig $ \sigptr ->
    withPtr ai $ \aiptr -> [C.block|
        void {
            $(Signature* sigptr)->SetAggregationInfo(*$(AggregationInfo* aiptr));
        }|]


setAggregationInfo :: Signature -> AggregationInfo -> IO Signature
setAggregationInfo sig ai = do
    sig' <- copy sig
    unsafeSetAggregationInfo sig' ai
    return sig'


verify :: Signature -> IO Bool
verify sig = fmap toBool $ withPtr sig $ \sigptr ->
    [CU.exp| bool { $(Signature* sigptr)->Verify() } |]



