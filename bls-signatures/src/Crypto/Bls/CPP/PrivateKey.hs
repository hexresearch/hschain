{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
module Crypto.Bls.CPP.PrivateKey
    ( PrivateKey
    , privateKeyDeserialize
    , privateKeyEq
    , privateKeyFromSeed
    , privateKeyGetPublicKey
    , privateKeyInsecureAggregate
    , privateKeySerialize
    , privateKeySizeGet
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


privateKeyDeserialize :: ByteString -> Maybe PrivateKey -- TODO check size
privateKeyDeserialize bs = Just $ unsafePerformIO $ fromPtr [C.exp|
    PrivateKey * {
        new PrivateKey(PrivateKey::FromBytes((uint8_t const*)$bs-ptr:bs))
    }|]


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


privateKeyFromSeed :: ByteString -> PrivateKey
privateKeyFromSeed seed = unsafePerformIO $ fromPtr [C.exp|
    PrivateKey * {
        new PrivateKey(PrivateKey::FromSeed((uint8_t const*)$bs-ptr:seed, $bs-len:seed))
    }|]


privateKeyGetPublicKey :: PrivateKey -> PublicKey
privateKeyGetPublicKey pk = unsafePerformIO $ withPtr pk $ \pkptr -> fromPtr [C.exp|
    PublicKey * {
        new PublicKey($(PrivateKey* pkptr)->GetPublicKey())
    }|]


-- | Insecurely aggregate multiple private keys into one
--
privateKeyInsecureAggregateVector :: Vector PrivateKey -> PrivateKey
privateKeyInsecureAggregateVector privateKeys = unsafePerformIO $
    fmap (fromMaybe (error "aggregateInsecure with empty vector!")) $
        withArrayPtrLen privateKeys $ \ptrPrivateKeys lenPrivateKeys ->
            fromPtr [C.exp| PrivateKey * {
                new PrivateKey(PrivateKey::AggregateInsecure(
                    std::vector<PrivateKey>( $(PrivateKey * ptrPrivateKeys)
                                           , $(PrivateKey * ptrPrivateKeys) + $(size_t lenPrivateKeys))))
            }|]


privateKeyInsecureAggregate :: [PrivateKey] -> PrivateKey
privateKeyInsecureAggregate = privateKeyInsecureAggregateVector . V.fromList


privateKeySerialize :: PrivateKey -> ByteString
privateKeySerialize pk = unsafePerformIO $ withPtr pk $ \pkptr ->
    BS.create privateKeySizeGet $ \pkbuffer ->
        [C.exp| void { $(PrivateKey * pkptr)->Serialize($(uint8_t * pkbuffer)) }|]


privateKeySizeGet :: Int
privateKeySizeGet = fromIntegral [C.pure| size_t { PrivateKey::PRIVATE_KEY_SIZE }|]

