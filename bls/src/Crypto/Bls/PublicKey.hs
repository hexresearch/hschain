{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
module Crypto.Bls.PublicKey
    ( PublicKey
    , serializePublicKey
    , aggregateInsecurePublicKey
    ) where


import Data.Maybe
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Language.C.Inline as C
import qualified Language.C.Inline.Cpp as C
import qualified Data.ByteString.Internal as BS
import Data.Vector (Vector)
import qualified Data.Vector as V

import Crypto.Bls.Internal
import Crypto.Bls.Types
import Crypto.Bls.Arrays


C.context blsCtx
C.include "bls.hpp"
C.using "namespace bls"


publicKeySize :: Int
publicKeySize = fromIntegral [C.pure| size_t { PublicKey::PUBLIC_KEY_SIZE }|]


serializePublicKey :: PublicKey -> IO ByteString
serializePublicKey pk = withPtr pk $ \pkptr ->
    BS.create publicKeySize $ \pkbuffer ->
        [C.exp| void { $(PublicKey * pkptr)->Serialize($(uint8_t * pkbuffer)) }|]



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
