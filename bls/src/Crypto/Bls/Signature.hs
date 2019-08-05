{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
module Crypto.Bls.Signature
    ( Signature
    , copy
    , unsafeSetAggregationInfo
    , setAggregationInfo
    , verify
    , verifyInsecure
    ) where


import Data.Maybe
import Data.Coerce
import Data.Vector (Vector)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BS
import qualified Data.Vector as V
import qualified Language.C.Inline as C
import qualified Language.C.Inline.Cpp as C
import Foreign.Marshal.Utils (toBool)
import Foreign.C.String
import qualified Language.C.Inline as C
import qualified Language.C.Inline.Cpp as C

import Crypto.Bls.Internal
import Crypto.Bls.Types
import Crypto.Bls.Arrays

import qualified Data.Vector.Storable as VM

C.context blsCtx
C.include "bls.hpp"
C.include "<vector>"
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
    [C.exp| bool { $(Signature* sigptr)->Verify() } |]


-- * Insecure signatures

    {-


                    ( std::vector<uint8_t*>
                        ( $vec-ptr:chashes
                        , $vec-ptr:chashes + $vec-len:chashes) 

                    , std::vector<PublicKey>
                        ( $(PublicKey * pubKeysPtr)
                        , $(PublicKey * pubKeysPtr) + $(size_t pubKeysLen))

                        -}


-- TODO optimize it!
withByteStringsVector :: Vector ByteString -> (VM.Vector CString -> IO a) -> IO a
withByteStringsVector strs act = withByteStringsVector' (V.toList strs) []
  where
    -- TODO using folds? or 'construct'?
    -- withByteStringsVector' :: [ByteString] -> [CString] -> IO a
    withByteStringsVector' [] acc = act (VM.fromList (reverse acc))
    withByteStringsVector' (bs:bss) acc =
        BS.unsafeUseAsCString bs (\cs -> withByteStringsVector' bss (cs:acc))



verifyInsecure :: InsecureSignature -> Vector Hash256 -> Vector PublicKey -> IO Bool
verifyInsecure insecureSig hashes pubKeys =
    fmap (fromMaybe False) $
    withPtr insecureSig $ \insecureSigPtr ->
        withByteStringsVector (coerce hashes) $ \hashesVec ->
            withArrayPtrLen pubKeys $ \pubKeysPtr pubKeysLen -> fmap toBool $
                [C.exp| bool {
                    $(InsecureSignature * insecureSigPtr)->Verify(
                        std::vector<const uint8_t*>
                            ( (const uint8_t**)($vec-ptr:(char * * hashesVec))
                            , (const uint8_t**)($vec-ptr:(char * * hashesVec) + $vec-len:hashesVec)),
                        std::vector<PublicKey>
                            ( $(PublicKey * pubKeysPtr)
                            , $(PublicKey * pubKeysPtr) + $(size_t pubKeysLen))
                        )
                }|]
