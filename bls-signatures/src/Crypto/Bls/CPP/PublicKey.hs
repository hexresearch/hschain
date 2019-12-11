{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
module Crypto.Bls.CPP.PublicKey
    ( PublicKey
    , publicKeySize
    , publicKeySerialize
    , publicKeyDeserialize
    , publicKeyGetFingerprint
    , publicKeyInsecureAggregateVec
    , publicKeyInsecureAggregate
    , publicKeyEq
    ) where


import Data.ByteString (ByteString)
import Data.Word
import Data.Maybe
import Data.Vector (Vector)
import Foreign.Marshal.Utils (toBool)
import System.IO.Unsafe
import qualified Data.ByteString.Internal as BS
import qualified Data.Vector as V
import qualified Language.C.Inline as C
import qualified Language.C.Inline.Cpp as C

import Crypto.Bls.CPP.Arrays
import Crypto.Bls.CPP.Internal
import Crypto.Bls.CPP.Types


C.context blsCtx
C.include "chiabls/bls.hpp"
C.using "namespace bls"


publicKeySize :: Int
publicKeySize = fromIntegral [C.pure| size_t { PublicKey::PUBLIC_KEY_SIZE }|]


publicKeySerialize :: PublicKey -> ByteString
publicKeySerialize pk = unsafePerformIO $ withPtr pk $ \pkptr ->
    BS.create publicKeySize $ \pkbuffer ->
        [C.exp| void { $(PublicKey * pkptr)->Serialize($(uint8_t * pkbuffer)) }|]


publicKeyDeserialize :: ByteString -> PublicKey -- TODO check size
publicKeyDeserialize bs = unsafePerformIO $ fromPtr [C.exp|
    PublicKey * {
        new PublicKey(PublicKey::FromBytes((uint8_t const*)$bs-ptr:bs))
    }|]


publicKeyGetFingerprint :: PublicKey -> Word32
publicKeyGetFingerprint pk = fromIntegral $ unsafePerformIO $ withPtr pk $ \pkPtr ->
    [C.exp| uint32_t { $(PublicKey* pkPtr)->GetFingerprint() }|]


-- | Insecurely aggregate multiple private keys into one
publicKeyInsecureAggregateVec :: Vector PublicKey -> PublicKey
publicKeyInsecureAggregateVec publicKeys =
    fromMaybe (error "aggregateInsecure with empty vector!") $
    unsafePerformIO $
    withArrayPtrLen publicKeys $ \ptrPublicKeys lenPublicKeys ->
        fromPtr [C.exp| PublicKey * {
            new PublicKey(PublicKey::AggregateInsecure(
                std::vector<PublicKey>( $(PublicKey * ptrPublicKeys)
                                       , $(PublicKey * ptrPublicKeys) + $(size_t lenPublicKeys))))
        }|]


-- | Insecurely aggregate multiple private keys into one
publicKeyInsecureAggregate :: [PublicKey] -> PublicKey
publicKeyInsecureAggregate = publicKeyInsecureAggregateVec . V.fromList


publicKeyEq :: PublicKey -> PublicKey -> Bool
publicKeyEq pubKey1 pubKey2 = toBool $ unsafePerformIO $
    withPtr pubKey1 $ \pubKey1Ptr ->
        withPtr pubKey2 $ \pubKey2Ptr ->
            [C.exp| bool { *$(PublicKey* pubKey1Ptr) == *$(PublicKey* pubKey2Ptr) } |]

