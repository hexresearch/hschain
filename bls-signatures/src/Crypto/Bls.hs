{-# LANGUAGE CPP #-}
module Crypto.Bls
    (
#ifdef __GHCJS__
      PrivateKey
    , PublicKey
    , Signature
    , InsecureSignature
    , initBls
    , hash256
    , privateKeyFromSeed
    , privateKeyGetPublicKey
    , privateKeySerialize
    , privateKeyFromBytes
    , privateKeyEq
    , publicKeyGetFingerprint
    , publicKeyInsecureAggregate
    , publicKeySerialize
    , publicKeyFromBytes
    , publicKeyEq
    , insecureSignatureAggregate
    , insecureSignatureEq
    , insecureSignatureFromBytes
    , insecureSignatureSerialize
    , insecureSignatureVerify
    , signInsecure
    , signatureEq
    , signatureFromBytes
    , signatureSerialize
#else
      module Crypto.Bls.CPP.AggregationInfo
    , module Crypto.Bls.CPP.PrivateKey
    , module Crypto.Bls.CPP.PublicKey
    , module Crypto.Bls.CPP.Signature
    , module Crypto.Bls.CPP.Threshold
    , module Crypto.Bls.CPP.Types
    , module Crypto.Bls.CPP.Util
#endif
    )
    where


#ifdef __GHCJS__

--import Crypto.Bls.AggregationInfo
import Crypto.Bls.JavaScript.Common
import Crypto.Bls.JavaScript.PrivateKey
import Crypto.Bls.JavaScript.PublicKey
import Crypto.Bls.JavaScript.Signature
--import Crypto.Bls.PublicKey
--import Crypto.Bls.Signature
--import Crypto.Bls.Threshold
--import Crypto.Bls.Types
--import Crypto.Bls.Util

#else

import Crypto.Bls.CPP.AggregationInfo
import Crypto.Bls.CPP.PrivateKey
import Crypto.Bls.CPP.PublicKey
import Crypto.Bls.CPP.Signature
import Crypto.Bls.CPP.Threshold
import Crypto.Bls.CPP.Types
import Crypto.Bls.CPP.Util

#endif

