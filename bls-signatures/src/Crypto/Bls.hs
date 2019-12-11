{-# LANGUAGE CPP #-}
module Crypto.Bls
    ( PrivateKey
    , PublicKey
    , Signature
    , InsecureSignature
    , Hash256
    , initBls
    , hash256
    , hash256serialize
    , privateKeyFromSeed
    , privateKeyGetPublicKey
    , privateKeySerialize
    , privateKeyDeserialize
    , privateKeyEq
    , publicKeyGetFingerprint
    , publicKeyInsecureAggregate
    , publicKeySerialize
    , publicKeyDeserialize
    , publicKeyEq
    , insecureSignatureAggregate
    , insecureSignatureEq
    , insecureSignatureDeserialize
    , insecureSignatureSerialize
    , insecureSignatureVerify
    , signInsecure
    , signatureEq
    , signatureDeserialize
    , signatureSerialize
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

