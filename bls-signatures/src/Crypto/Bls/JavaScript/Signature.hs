module Crypto.Bls.JavaScript.Signature
    ( Signature(..)
    , InsecureSignature(..)
    , insecureSignatureAggregate
    , insecureSignatureDeserialize
    , insecureSignatureEq
    , insecureSignatureSerialize
    , insecureSignatureVerify
    , signInsecure
    , signInsecurePrehashed
    , signatureDeserialize
    , signatureEq
    , signatureSerialize
    , signatureSizeGet
    ) where


import Crypto.Bls.JavaScript.Common
import Crypto.Bls.JavaScript.PrivateKey
import Crypto.Bls.JavaScript.PublicKey
import Data.ByteString (ByteString)
import GHCJS.Types
import JavaScript.Array
import JavaScript.TypedArray as A

-- * --------------------------------------------------------------------------

foreign import javascript "($1).InsecureSignature.aggregate($2)"
    js_insecureAggregate :: JSVal -> JSArray -> JSVal

foreign import javascript "($1).verify($2, $3)"
    js_insecureVerify :: JSVal -> JSArray -> JSArray -> Bool

foreign import javascript "($1).signInsecure($2)"
    js_signInsecure :: JSVal -> Uint8Array -> JSVal

foreign import javascript "($1).signPrehashed($2)"
    js_signPrehashed :: JSVal -> Uint8Array -> JSVal

foreign import javascript "($1).InsecureSignature.fromBytes($2)"
    js_insecureSignatureFromBytes :: JSVal -> Uint8Array -> JSVal

foreign import javascript "($1).Signature.fromBytes($2)"
    js_signatureFromBytes :: JSVal -> Uint8Array -> JSVal

foreign import javascript "($1).Signature.SIGNATURE_SIZE"
    js_signatureSize :: JSVal -> Int

-- * --------------------------------------------------------------------------

newtype Signature = Signature JSVal


newtype InsecureSignature = InsecureSignature JSVal


instance IsJSVal' Signature where jsval' (Signature j) = j


instance IsJSVal' InsecureSignature where jsval' (InsecureSignature j) = j

-- * --------------------------------------------------------------------------

signatureSerialize :: Signature -> ByteString
signatureSerialize (Signature jssig) = arr2bs $ js_serialize jssig


insecureSignatureSerialize :: InsecureSignature -> ByteString
insecureSignatureSerialize (InsecureSignature jssig) = arr2bs $ js_serialize jssig


insecureSignatureAggregate :: [InsecureSignature] -> InsecureSignature
insecureSignatureAggregate sigs = InsecureSignature $ js_insecureAggregate (getJsVal blsModule) (fromList $ map getJsVal sigs)


signInsecure :: PrivateKey -> ByteString -> InsecureSignature
signInsecure (PrivateKey jspk) msg = InsecureSignature $ js_signInsecure jspk (bs2arr msg)


signInsecurePrehashed :: PrivateKey -> Hash256 -> InsecureSignature
signInsecurePrehashed (PrivateKey jspk) (Hash256 hash) = InsecureSignature $ js_signPrehashed jspk (bs2arr hash)


insecureSignatureVerify :: InsecureSignature -> [Hash256] -> [PublicKey] -> Bool
insecureSignatureVerify insig hashes publicKeys =
    js_insecureVerify (getJsVal insig)
                      (fromList $ map (jsval . bs2arr . unHash256) hashes)
                      (fromList $ map getJsVal publicKeys)


-- TODO export `operator==()` to JavaScript and use it
signatureEq :: Signature -> Signature -> Bool
signatureEq sig1 sig2 = signatureSerialize sig1 == signatureSerialize sig2


-- TODO export `operator==()` to JavaScript and use it
insecureSignatureEq :: InsecureSignature -> InsecureSignature -> Bool
insecureSignatureEq isig1 isig2 = insecureSignatureSerialize isig1 == insecureSignatureSerialize isig2


insecureSignatureDeserialize :: ByteString -> Maybe InsecureSignature
insecureSignatureDeserialize bytes = Just $ InsecureSignature $ js_insecureSignatureFromBytes (getJsVal blsModule) (bs2arr bytes)  -- TODO Catch errors!


signatureDeserialize :: ByteString -> Signature
signatureDeserialize bytes = Signature $ js_signatureFromBytes (getJsVal blsModule) (bs2arr bytes)


signatureSizeGet :: Int
signatureSizeGet = js_signatureSize (getJsVal blsModule)

