{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
module Crypto.Bls.CPP.PrivateKey
    ( PrivateKey
    , aggregateInsecurePrivateKey
    , aggregateInsecurePrivateKeyList
    , privateKeyFromSeed
    , privateKeyGetPublicKey
    , privateKeySize
    , privateKeySerialize
    , sign
    , signInsecure
    , signInsecurePrehashed
    , privateKeyDeserialize
    , privateKeyEq
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

import Crypto.Bls.CPP.Arrays
import Crypto.Bls.CPP.Internal
import Crypto.Bls.CPP.Types

C.context blsCtx
C.include "chiabls/bls.hpp"
C.include "<iostream>"
C.using "namespace bls"
C.using "namespace std"



privateKeyFromSeed :: ByteString -> PrivateKey
privateKeyFromSeed seed = unsafePerformIO $ fromPtr [C.exp|
    PrivateKey * {
        new PrivateKey(PrivateKey::FromSeed((uint8_t const*)$bs-ptr:seed, $bs-len:seed))
    }|]


privateKeySize :: Int
privateKeySize = fromIntegral [C.pure| size_t { PrivateKey::PRIVATE_KEY_SIZE }|]


privateKeySerialize :: PrivateKey -> ByteString
privateKeySerialize pk = unsafePerformIO $ withPtr pk $ \pkptr ->
    BS.create privateKeySize $ \pkbuffer ->
        [C.exp| void { $(PrivateKey * pkptr)->Serialize($(uint8_t * pkbuffer)) }|]


privateKeyDeserialize :: ByteString -> PrivateKey -- TODO check size
privateKeyDeserialize bs = unsafePerformIO $ fromPtr [C.exp|
    PrivateKey * {
        new PrivateKey(PrivateKey::FromBytes((uint8_t const*)$bs-ptr:bs))
    }|]


privateKeyGetPublicKey :: PrivateKey -> PublicKey
privateKeyGetPublicKey pk = unsafePerformIO $ withPtr pk $ \pkptr -> fromPtr [C.exp|
    PublicKey * {
        new PublicKey($(PrivateKey* pkptr)->GetPublicKey())
    }|]


sign :: PrivateKey -> ByteString -> IO Signature
sign pk msg = withPtr pk $ \pkptr -> fromPtr [C.exp|
    Signature * {
        new Signature($(PrivateKey* pkptr)->Sign((uint8_t const*)$bs-ptr:msg, $bs-len:msg))
    }|]


signInsecure :: PrivateKey -> ByteString -> InsecureSignature
signInsecure pk msg = unsafePerformIO $ withPtr pk $ \pkptr -> fromPtr [C.exp|
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

privateKeyEq :: PrivateKey -> PrivateKey -> Bool
privateKeyEq pk1 pk2 = not $ toBool $ unsafePerformIO $
    withPtr pk1 $ \pk1ptr ->
        withPtr pk2 $ \pk2ptr ->
            -- TODO странное поведение при возврате 'false' (см. первый тест Equivalence);
            --      который почему-то преобразовывается в '-256'; однако, использование отрицания помогает.
            [C.exp| bool { *$(PrivateKey* pk1ptr) != *$(PrivateKey* pk2ptr) } |]
            {-
            [C.block| bool {
                PrivateKey* p1 = $(PrivateKey* pk1ptr);
                PrivateKey* p2 = $(PrivateKey* pk2ptr);
                bool b = (*p1 == *p2);
                int x = b;
                std::cout << "P1 - 1: " << b      << std::endl;
                std::cout << "P1 - 2: " << (int)b << std::endl;
                std::cout << "P1 - 3: " << x      << std::endl;
                return (x + 1);
            } |]
            -}
