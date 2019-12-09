module Crypto.Bls.JavaScript.Signature
    ( Signature(..)
    , InsecureSignature(..)
    , serializeSignature
    , serializeInsecureSignature
    , aggregateInsecureSignatures
    , insecureVerify
    , signInsecure
    ) where


import GHCJS.Types
import Data.Word

import qualified Data.JSString
import Data.Coerce
-- import Data.JSString.Internal.Type

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS

import GHCJS.Foreign
import GHCJS.Marshal
import GHCJS.Marshal.Pure
import JavaScript.Object.Internal as O

import JavaScript.Object

import qualified GHCJS.Types    as T
import qualified GHCJS.Foreign  as F
import qualified GHCJS.Foreign.Callback  as F

import JavaScript.TypedArray as A
import GHCJS.Buffer as BUF


import Crypto.Bls.JavaScript.Common
import Crypto.Bls.JavaScript.PublicKey
import Crypto.Bls.JavaScript.PrivateKey
import JavaScript.Array


foreign import javascript "($1).InsecureSignature.aggregate($2)"
    js_insecureAggregate :: JSVal -> JSArray -> JSVal

foreign import javascript "($1).verify($2, $3)"
    js_insecureVerify :: JSVal -> JSArray -> JSArray -> Bool


foreign import javascript "($1).signInsecure($2)"
    js_signInsecure :: JSVal -> Uint8Array -> JSVal



newtype Signature = Signature JSVal

instance IsJSVal' Signature where jsval' (Signature j) = j

newtype InsecureSignature = InsecureSignature JSVal

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
