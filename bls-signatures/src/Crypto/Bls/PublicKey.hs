{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
module Crypto.Bls.PublicKey
    ( PublicKey
    , publicKeySize
    , serializePublicKey
    , deserializePublicKey
    , aggregateInsecurePublicKey
    , aggregateInsecurePublicKeyList
    , equalPublicKey
    ) where


import Data.ByteString (ByteString)
import Data.Maybe
import Data.Vector (Vector)
import Foreign.Marshal.Utils (toBool)
import System.IO.Unsafe
import qualified Data.ByteString.Internal as BS
import qualified Language.C.Inline as C
import qualified Language.C.Inline.Cpp as C
import qualified Data.Vector as V

import Crypto.Bls.Arrays
import Crypto.Bls.Internal
import Crypto.Bls.Types


C.context blsCtx
C.include "bls.hpp"
C.using "namespace bls"


publicKeySize :: Int
publicKeySize = fromIntegral [C.pure| size_t { PublicKey::PUBLIC_KEY_SIZE }|]


serializePublicKey :: PublicKey -> IO ByteString
serializePublicKey pk = withPtr pk $ \pkptr ->
    BS.create publicKeySize $ \pkbuffer ->
        [C.exp| void { $(PublicKey * pkptr)->Serialize($(uint8_t * pkbuffer)) }|]


deserializePublicKey :: ByteString -> IO PublicKey -- TODO check size
deserializePublicKey bs = fromPtr [C.exp|
    PublicKey * {
        new PublicKey(PublicKey::FromBytes((uint8_t const*)$bs-ptr:bs))
    }|]


-- | Insecurely aggregate multiple private keys into one
aggregateInsecurePublicKey :: Vector PublicKey -> IO PublicKey
aggregateInsecurePublicKey publicKeys =
    fmap (fromMaybe (error "aggregateInsecure with empty vector!")) $
        withArrayPtrLen publicKeys $ \ptrPublicKeys lenPublicKeys ->
            fromPtr [C.exp| PublicKey * {
                new PublicKey(PublicKey::AggregateInsecure(
                    std::vector<PublicKey>( $(PublicKey * ptrPublicKeys)
                                           , $(PublicKey * ptrPublicKeys) + $(size_t lenPublicKeys))))
            }|]

-- | Insecurely aggregate multiple private keys into one
aggregateInsecurePublicKeyList :: [PublicKey] -> IO PublicKey
aggregateInsecurePublicKeyList = aggregateInsecurePublicKey . V.fromList


equalPublicKey :: PublicKey -> PublicKey -> Bool
equalPublicKey pubKey1 pubKey2 = toBool $ unsafePerformIO $
    withPtr pubKey1 $ \pubKey1Ptr ->
        withPtr pubKey2 $ \pubKey2Ptr ->
            [C.exp| bool { *$(PublicKey* pubKey1Ptr) == *$(PublicKey* pubKey2Ptr) } |]

