{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
module Crypto.Bls.PrivateKey
    ( PrivateKey
    , aggregateInsecurePrivateKey
    , aggregateInsecurePrivateKeyList
    , fromSeed
    , getPublicKey
    , privateKeySize
    , serializePrivateKey
    , sign
    , signInsecure
    , signInsecurePrehashed
    , deserializePrivateKey
    , equalPrivateKey
    ) where


import Data.ByteString (ByteString)
import Data.Maybe
import Data.Vector (Vector)
import Foreign.Marshal.Utils (toBool)
import System.IO.Unsafe
import qualified Data.ByteString.Internal as BS
import qualified Data.Vector              as V
import qualified Language.C.Inline        as C
import qualified Language.C.Inline.Cpp    as C

import Crypto.Bls.Arrays
import Crypto.Bls.Internal
import Crypto.Bls.Types


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


deserializePrivateKey :: ByteString -> IO PrivateKey -- TODO check size
deserializePrivateKey bs = fromPtr [C.exp|
    PrivateKey * {
        new PrivateKey(PrivateKey::FromBytes((uint8_t const*)$bs-ptr:bs))
    }|]


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

signInsecurePrehashed :: PrivateKey -> Hash256 -> IO InsecureSignature
signInsecurePrehashed pk Hash256{..} = withPtr pk $ \pkptr -> fromPtr [C.exp|
    InsecureSignature * {
        new InsecureSignature($(PrivateKey* pkptr)->SignInsecurePrehashed((uint8_t const*)$bs-ptr:unHash256))
    }|]


-- | Insecurely aggregate multiple private keys into one
--
aggregateInsecurePrivateKey :: Vector PrivateKey -> IO PrivateKey
aggregateInsecurePrivateKey privateKeys =
    fmap (fromMaybe (error "aggregateInsecure with empty vector!")) $
        withArrayPtrLen privateKeys $ \ptrPrivateKeys lenPrivateKeys ->
            fromPtr [C.exp| PrivateKey * {
                new PrivateKey(PrivateKey::AggregateInsecure(
                    std::vector<PrivateKey>( $(PrivateKey * ptrPrivateKeys)
                                           , $(PrivateKey * ptrPrivateKeys) + $(size_t lenPrivateKeys))))
            }|]


aggregateInsecurePrivateKeyList :: [PrivateKey] -> IO PrivateKey
aggregateInsecurePrivateKeyList = aggregateInsecurePrivateKey . V.fromList


equalPrivateKey :: PrivateKey -> PrivateKey -> Bool
equalPrivateKey pk1 pk2 = toBool $ unsafePerformIO $
    withPtr pk1 $ \pk1ptr ->
        withPtr pk2 $ \pk2ptr ->
            [C.exp| bool { *$(PrivateKey* pk1ptr) == *$(PrivateKey* pk2ptr) } |]

