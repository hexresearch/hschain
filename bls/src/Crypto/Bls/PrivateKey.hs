{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
module Crypto.Bls.PrivateKey
    ( PrivateKey
    , fromSeed
    , getPublicKey
    , sign
    ) where


import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Language.C.Inline as C
import qualified Language.C.Inline.Cpp as C

import Crypto.Bls.Internal
import Crypto.Bls.Types


C.context blsCtx
C.include "bls.hpp"
C.using "namespace bls"



fromSeed :: ByteString -> IO PrivateKey
fromSeed seed = fromPtr [C.exp|
    PrivateKey * {
        new PrivateKey(PrivateKey::FromSeed((uint8_t const*)$bs-ptr:seed, $bs-len:seed))
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

