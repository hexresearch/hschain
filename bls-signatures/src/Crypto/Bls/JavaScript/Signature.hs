module Crypto.Bls.JavaScript.Signature
    ( Signature(..)
    , InsecureSignature(..)
    , serializeSignature
    , serializeInsecureSignature
    , aggregateInsecureSignatures
    , insecureVerify
    , signInsecure
    , insecureSignatureEq
    , signatureEq
    , insecureSignatureFromBytes
    ) where


import Crypto.Bls.JavaScript.Common
import Crypto.Bls.JavaScript.PrivateKey
import Crypto.Bls.JavaScript.PublicKey
import Data.ByteString (ByteString)
import Data.Coerce
import Data.Word
import GHCJS.Buffer as BUF
import GHCJS.Foreign
import GHCJS.Marshal
import GHCJS.Marshal.Pure
import GHCJS.Types
import JavaScript.Array
import JavaScript.Object
import JavaScript.Object.Internal as O
import JavaScript.TypedArray as A
import qualified Data.ByteString as BS
import qualified Data.JSString
import qualified GHCJS.Foreign  as F
import qualified GHCJS.Foreign.Callback  as F
import qualified GHCJS.Types    as T


foreign import javascript "($1).InsecureSignature.aggregate($2)"
    js_insecureAggregate :: JSVal -> JSArray -> JSVal

foreign import javascript "($1).verify($2, $3)"
    js_insecureVerify :: JSVal -> JSArray -> JSArray -> Bool


foreign import javascript "($1).signInsecure($2)"
    js_signInsecure :: JSVal -> Uint8Array -> JSVal

foreign import javascript "($1).InsecureSignature.fromBytes($2)"
    js_insecureSignatureFromBytes :: JSVal -> Uint8Array -> JSVal

newtype Signature = Signature JSVal


newtype InsecureSignature = InsecureSignature JSVal


instance IsJSVal' Signature where jsval' (Signature j) = j


instance IsJSVal' InsecureSignature where jsval' (InsecureSignature j) = j


serializeSignature :: Signature -> ByteString
serializeSignature (Signature jssig) = arr2bs $ js_serialize jssig


serializeInsecureSignature :: InsecureSignature -> ByteString
serializeInsecureSignature (InsecureSignature jssig) = arr2bs $ js_serialize jssig


aggregateInsecureSignatures :: [InsecureSignature] -> InsecureSignature
aggregateInsecureSignatures sigs = InsecureSignature $ js_insecureAggregate (getJsVal blsModule) (fromList $ map getJsVal sigs)


signInsecure :: PrivateKey -> ByteString -> InsecureSignature
signInsecure (PrivateKey jspk) msg = InsecureSignature $ js_signInsecure jspk (bs2arr msg)


insecureVerify :: InsecureSignature -> [ByteString] -> [PublicKey] -> Bool
insecureVerify insig hashes publicKeys = js_insecureVerify (getJsVal insig) (fromList $ map (jsval . bs2arr) hashes) (fromList $ map getJsVal publicKeys)


-- TODO export `operator==()` to JavaScript and use it
signatureEq :: Signature -> Signature -> Bool
signatureEq sig1 sig2 = serializeSignature sig1 == serializeSignature sig2


-- TODO export `operator==()` to JavaScript and use it
insecureSignatureEq :: InsecureSignature -> InsecureSignature -> Bool
insecureSignatureEq isig1 isig2 = serializeInsecureSignature isig1 == serializeInsecureSignature isig2


insecureSignatureFromBytes :: ByteString -> InsecureSignature
insecureSignatureFromBytes bytes = InsecureSignature $ js_insecureSignatureFromBytes (getJsVal blsModule) (bs2arr bytes)
