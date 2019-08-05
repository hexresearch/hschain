{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
module Crypto.Bls.PrivateKey
    ( PrivateKey
    , aggregateInsecurePrivateKey
    , fromSeed
    , getPublicKey
    , privateKeySize
    , serializePrivateKey
    , sign
    , signInsecure
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



fromSeed :: ByteString -> IO PrivateKey
fromSeed seed = fromPtr [C.exp|
    PrivateKey * {
        new PrivateKey(PrivateKey::FromSeed((uint8_t const*)$bs-ptr:seed, $bs-len:seed))
    }|]


privateKeySize :: Int
privateKeySize = fromIntegral [C.pure| size_t { PrivateKey::PRIVATE_KEY_SIZE }|]


serializePrivateKey :: PrivateKey -> IO ByteString
serializePrivateKey pk = withPtr pk $ \pkptr ->
    BS.create privateKeySize $ \pkbuffer ->
        [C.exp| void { $(PrivateKey * pkptr)->Serialize($(uint8_t * pkbuffer)) }|]


getPublicKey :: PrivateKey -> IO PublicKey
getPublicKey pk = withPtr pk $ \pkptr -> fromPtr [C.exp|
    PublicKey * {
        new PublicKey($(PrivateKey* pkptr)->GetPublicKey())
    }|]


sign :: PrivateKey -> ByteString -> IO Signature
sign pk msg = withPtr pk $ \pkptr -> fromPtr [C.exp|
    Signature * {
        new Signature($(PrivateKey* pkptr)->Sign((uint8_t const*)$bs-ptr:msg, $bs-len:msg))
    }|]


signInsecure :: PrivateKey -> ByteString -> IO InsecureSignature
signInsecure pk msg = withPtr pk $ \pkptr -> fromPtr [C.exp|
    InsecureSignature * {
        new InsecureSignature($(PrivateKey* pkptr)->SignInsecure((uint8_t const*)$bs-ptr:msg, $bs-len:msg))
    }|]


-- | Insecurely aggregate multiple private keys into one
aggregateInsecurePrivateKey :: Vector PrivateKey -> IO PrivateKey
aggregateInsecurePrivateKey privateKeys =
    fmap (fromMaybe (error "aggregateInsecure with empty vector!")) $
        withArrayPtrLen privateKeys $ \ptrPrivateKeys lenPrivateKeys ->
            fromPtr [C.exp| PrivateKey * {
                new PrivateKey(PrivateKey::AggregateInsecure(
                    std::vector<PrivateKey>( $(PrivateKey * ptrPrivateKeys)
                                           , $(PrivateKey * ptrPrivateKeys) + $(size_t lenPrivateKeys))))
            }|]

