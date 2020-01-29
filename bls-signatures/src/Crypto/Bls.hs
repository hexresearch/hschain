{-# LANGUAGE CPP #-}
module Crypto.Bls
    ( Hash256(..)
    , InsecureSignature
    , PrivateKey
    , PublicKey
    , Signature
    , initBls
    , hash256
    , hash256serialize
    , insecureSignatureAggregate
    , insecureSignatureDeserialize
    , insecureSignatureEq
    , insecureSignatureSerialize
    , insecureSignatureVerify
    , privateKeyDeserialize
    , privateKeyEq
    , privateKeyFromSeed
    , privateKeyGetPublicKey
    , privateKeyInsecureAggregate
    , privateKeySerialize
    , privateKeySizeGet
    , publicKeyDeserialize
    , publicKeyEq
    , publicKeyGetFingerprint
    , publicKeyInsecureAggregate
    , publicKeySerialize
    , publicKeySizeGet
    , signInsecure
    , signInsecurePrehashed
    , signatureDeserialize
    , signatureEq
    , signatureSerialize
    , signatureSizeGet
    )
    where

#ifdef __GHCJS__

import Crypto.Bls.JavaScript.Common
import Crypto.Bls.JavaScript.PrivateKey
import Crypto.Bls.JavaScript.PublicKey
import Crypto.Bls.JavaScript.Signature

#else

import Crypto.Bls.CPP.AggregationInfo
import Crypto.Bls.CPP.Init
import Crypto.Bls.CPP.PrivateKey
import Crypto.Bls.CPP.PublicKey
import Crypto.Bls.CPP.Signature
import Crypto.Bls.CPP.Threshold
import Crypto.Bls.CPP.Types
import Crypto.Bls.CPP.Util

#endif

