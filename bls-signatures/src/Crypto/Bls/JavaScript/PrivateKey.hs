module Crypto.Bls.JavaScript.PrivateKey
    ( PrivateKey(..)
    , fromSeed
    , getPublicKey
    , serializePrivateKey
    , privateKeyFromBytes
    , privateKeyEq
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


foreign import javascript "($1).PrivateKey.fromSeed($2)"
    js_privateKeyFromSeed :: JSVal -> Uint8Array -> JSVal


foreign import javascript "($1).PrivateKey.fromBytes($2, $3)"
    js_privateKeyFromBytes :: JSVal -> Uint8Array -> Int -> JSVal

foreign import javascript "($1).getPublicKey()"
    js_getPublicKey :: JSVal -> JSVal


-- * --------------------------------------------------------------------------

newtype PrivateKey = PrivateKey JSVal


instance IsJSVal' PrivateKey where jsval' (PrivateKey j) = j


-- * --------------------------------------------------------------------------


getPublicKey :: PrivateKey -> PublicKey
getPublicKey (PrivateKey jspk) = PublicKey $ js_getPublicKey jspk


serializePrivateKey :: PrivateKey -> ByteString
serializePrivateKey (PrivateKey jspk) = arr2bs $ js_serialize jspk


fromSeed :: ByteString -> PrivateKey
fromSeed seed = PrivateKey $ js_privateKeyFromSeed (getJsVal blsModule) (bs2arr seed)


privateKeyFromBytes :: ByteString -> PrivateKey
privateKeyFromBytes bytes = PrivateKey $ js_privateKeyFromBytes (getJsVal blsModule) (bs2arr bytes) (BS.length bytes)


-- TODO export `operator==()` to JavaScript and use it
privateKeyEq :: PrivateKey -> PrivateKey -> Bool
privateKeyEq pk1 pk2 = serializePrivateKey pk1 == serializePrivateKey pk2

