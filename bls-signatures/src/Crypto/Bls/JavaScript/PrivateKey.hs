module Crypto.Bls.JavaScript.PrivateKey
    ( PrivateKey(..)
    , fromSeed
    , getPublicKey
    , serializePrivateKey
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


-- TODO: add finalizer to PrivateKey: it must be `.delete()` after it was used.
newtype PrivateKey = PrivateKey JSVal


instance IsJSVal' PrivateKey where jsval' (PrivateKey j) = j

foreign import javascript "($1).PrivateKey.fromSeed($2)" js_privateKeyFromSeed :: JSVal -> Uint8Array -> JSVal
-- foreign import javascript unsafe "seed = $2; console.log('-- SEED ----'); console.log(seed.toString()); ($1).PrivateKey.fromSeed($2)" js_privateKeyFromSeed :: JSVal -> Uint8Array -> IO JSVal


-- foreign import javascript "($1).serialize()" js_serialize :: JSVal -> Uint8Array

foreign import javascript "($1).getPublicKey()" js_getPublicKey :: JSVal -> JSVal


getPublicKey :: PrivateKey -> PublicKey
getPublicKey (PrivateKey jspk) = PublicKey $ js_getPublicKey jspk


serializePrivateKey :: PrivateKey -> ByteString
serializePrivateKey (PrivateKey jspk) = arr2bs $ js_serialize jspk


fromSeed :: ByteString -> PrivateKey
fromSeed seed = PrivateKey $ js_privateKeyFromSeed (getJsVal blsModule) (bs2arr seed)

