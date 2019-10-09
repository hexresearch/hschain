{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
module Crypto.Bls.Signature
    ( Signature
    , InsecureSignature
    , copy
    , unsafeSetAggregationInfo
    , setAggregationInfo
    , verify
    , verifyInsecure
    , verifyInsecure1
    , serializeInsecureSignature
    , aggregateInsecureSignatures
    , deserializeInsecureSignature
    , signatureSize
    ) where


import Data.Maybe
import Data.Coerce
import Data.Vector (Vector)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Unsafe as BS
import qualified Data.Vector as V
import qualified Language.C.Inline as C
import qualified Language.C.Inline.Cpp as C
import Foreign.Marshal.Utils (toBool)
import Foreign.C.String
import qualified Data.ByteString.Internal as BS

import Crypto.Bls.Internal
import Crypto.Bls.Types
import Crypto.Bls.Arrays

import qualified Data.Vector.Storable as VM

C.context blsCtx
C.include "bls.hpp"
C.include "<vector>"
C.using "namespace bls"


signatureSize :: Int
signatureSize = fromIntegral [C.pure| size_t { InsecureSignature::SIGNATURE_SIZE }|]


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


verifyInsecure1 :: InsecureSignature -> Hash256 -> PublicKey -> IO Bool
verifyInsecure1 insecureSig Hash256{..} pubKey =
    withPtr insecureSig $ \insecureSigPtr ->
        withPtr pubKey $ \pubKeyPtr -> fmap toBool $
            [C.block| bool {
                auto msgHash = (const uint8_t**)&($bs-ptr:unHash256);
                return $(InsecureSignature * insecureSigPtr)->Verify(
                    std::vector<const uint8_t*>(msgHash, msgHash + 1),
                    std::vector<PublicKey>($(PublicKey * pubKeyPtr), $(PublicKey * pubKeyPtr) + 1));
            }|]

insecureSignatureSize :: Int
insecureSignatureSize = fromIntegral [C.pure| size_t { InsecureSignature::SIGNATURE_SIZE }|]


serializeInsecureSignature :: InsecureSignature -> IO ByteString
serializeInsecureSignature sig = withPtr sig $ \sigptr ->
    BS.create insecureSignatureSize $ \sigbuffer ->
        [C.exp| void { $(InsecureSignature * sigptr)->Serialize($(uint8_t * sigbuffer)) }|]


deserializeInsecureSignature :: ByteString -> IO InsecureSignature -- TODO add check size
deserializeInsecureSignature bs = fromPtr [C.exp|
    InsecureSignature * {
        new InsecureSignature(InsecureSignature::FromBytes((uint8_t const*)$bs-ptr:bs))
    }|]


aggregateInsecureSignatures :: Vector InsecureSignature -> IO InsecureSignature
aggregateInsecureSignatures sigs =
    fmap (fromMaybe (error "empty sigs array")) $
    withArrayPtrLen sigs $ \sigsPtr sigsLen ->
        fromPtr [C.exp| InsecureSignature * {
            new InsecureSignature(InsecureSignature::Aggregate(
                std::vector<InsecureSignature>( $(InsecureSignature * sigsPtr)
                                              , $(InsecureSignature * sigsPtr) + $(size_t sigsLen))))
            }|]
