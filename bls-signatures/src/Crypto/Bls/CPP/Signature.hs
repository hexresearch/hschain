{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
module Crypto.Bls.CPP.Signature
    ( Signature
    , InsecureSignature
    , copy
    , unsafeSetAggregationInfo
    , setAggregationInfo
    , verify
    , insecureSignatureVerifyVec
    , insecureSignatureVerify
    , insecureSignatureSerialize
    , insecureSignatureAggregate
    , insecureSignatureAggregateVec
    , insecureSignatureDeserialize
    , signatureSize
    , signatureSerialize
    , signatureDeserialize
    , insecureSignatureEq
    , signatureEq
    ) where


import Data.ByteString (ByteString)
import Data.Coerce
import Data.Maybe
import Data.Vector (Vector)
import Foreign.C.String
import Foreign.Marshal.Utils (toBool)
import System.IO.Unsafe
import qualified Data.ByteString.Internal as BS
import qualified Data.ByteString.Unsafe   as BS
import qualified Data.Vector              as V
import qualified Data.Vector.Storable     as VM
import qualified Language.C.Inline        as C
import qualified Language.C.Inline.Cpp    as C

import Crypto.Bls.CPP.Arrays
import Crypto.Bls.CPP.Internal
import Crypto.Bls.CPP.Types


C.context blsCtx
C.include "chiabls/bls.hpp"
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

-- TODO optimize it!
withByteStringsVector :: Vector ByteString -> (VM.Vector CString -> IO a) -> IO a
withByteStringsVector strs act = withByteStringsVector' (V.toList strs) []
  where
    -- TODO using folds? or 'construct'?
    -- withByteStringsVector' :: [ByteString] -> [CString] -> IO a
    withByteStringsVector' [] acc = act (VM.fromList (reverse acc))
    withByteStringsVector' (bs:bss) acc =
        BS.unsafeUseAsCString bs (\cs -> withByteStringsVector' bss (cs:acc))


insecureSignatureVerifyVec :: InsecureSignature -> Vector Hash256 -> Vector PublicKey -> Bool
insecureSignatureVerifyVec insecureSig hashes pubKeys =
    fromMaybe False $
    unsafePerformIO $
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


insecureSignatureVerify :: InsecureSignature -> [Hash256] -> [PublicKey] -> Bool
insecureSignatureVerify isig hashes pks = insecureSignatureVerifyVec isig (V.fromList hashes) (V.fromList pks)


--verifyInsecure1 :: InsecureSignature -> Hash256 -> PublicKey -> IO Bool
--verifyInsecure1 insecureSig Hash256{..} pubKey =
--    withPtr insecureSig $ \insecureSigPtr ->
--        withPtr pubKey $ \pubKeyPtr -> fmap toBool $
--            [C.block| bool {
--                auto msgHash = (const uint8_t**)&($bs-ptr:unHash256);
--                return $(InsecureSignature * insecureSigPtr)->Verify(
--                    std::vector<const uint8_t*>(msgHash, msgHash + 1),
--                    std::vector<PublicKey>($(PublicKey * pubKeyPtr), $(PublicKey * pubKeyPtr) + 1));
--            }|]


insecureSignatureSize :: Int
insecureSignatureSize = fromIntegral [C.pure| size_t { InsecureSignature::SIGNATURE_SIZE }|]


insecureSignatureSerialize :: InsecureSignature -> ByteString
insecureSignatureSerialize sig = unsafePerformIO $ withPtr sig $ \sigptr ->
    BS.create insecureSignatureSize $ \sigbuffer ->
        [C.exp| void { $(InsecureSignature * sigptr)->Serialize($(uint8_t * sigbuffer)) }|]


insecureSignatureDeserialize :: ByteString -> InsecureSignature -- TODO add check size
insecureSignatureDeserialize bs = unsafePerformIO $ fromPtr [C.exp|
    InsecureSignature * {
        new InsecureSignature(InsecureSignature::FromBytes((uint8_t const*)$bs-ptr:bs))
    }|]


signatureSerialize :: Signature -> ByteString
signatureSerialize sig = unsafePerformIO $ withPtr sig $ \sigptr ->
    BS.create signatureSize $ \sigbuffer ->
        [C.exp| void { $(Signature * sigptr)->Serialize($(uint8_t * sigbuffer)) }|]


signatureDeserialize :: ByteString -> Signature -- TODO add check size
signatureDeserialize bs = unsafePerformIO $ fromPtr [C.exp|
    Signature * {
        new Signature(Signature::FromBytes((uint8_t const*)$bs-ptr:bs))
    }|]


insecureSignatureAggregateVec :: Vector InsecureSignature -> InsecureSignature
insecureSignatureAggregateVec sigs =
    fromMaybe (error "empty sigs array") $
    unsafePerformIO $
    withArrayPtrLen sigs $ \sigsPtr sigsLen ->
        fromPtr [C.exp| InsecureSignature * {
            new InsecureSignature(InsecureSignature::Aggregate(
                std::vector<InsecureSignature>( $(InsecureSignature * sigsPtr)
                                              , $(InsecureSignature * sigsPtr) + $(size_t sigsLen))))
            }|]


insecureSignatureAggregate :: [InsecureSignature] -> InsecureSignature
insecureSignatureAggregate = insecureSignatureAggregateVec . V.fromList


insecureSignatureEq :: InsecureSignature -> InsecureSignature -> Bool
insecureSignatureEq isig1 isig2 = toBool $ unsafePerformIO $
    withPtr isig1 $ \isig1ptr ->
        withPtr isig2 $ \isig2ptr ->
            [C.exp| bool { *$(InsecureSignature* isig1ptr) == *$(InsecureSignature* isig2ptr) } |]


signatureEq :: Signature -> Signature -> Bool
signatureEq sig1 sig2 = toBool $ unsafePerformIO $
    withPtr sig1 $ \sig1ptr ->
        withPtr sig2 $ \sig2ptr ->
            [C.exp| bool { *$(Signature* sig1ptr) == *$(Signature* sig2ptr) } |]

