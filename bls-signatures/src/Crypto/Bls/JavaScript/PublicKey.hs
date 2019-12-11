module Crypto.Bls.JavaScript.PublicKey
    ( PublicKey(..)
    , getFingerprint
    , aggregateInsecurePublicKeys
    , serializePublicKey
    , publicKeyFromBytes
    , publicKeyEq
    ) where


--import Control.Concurrent.MVar
import GHCJS.Types
import Data.Word

--import qualified Data.JSString
--import Data.Coerce
---- import Data.JSString.Internal.Type

import Data.ByteString (ByteString)
--import qualified Data.ByteString as BS

--import GHCJS.Foreign
--import GHCJS.Marshal
--import GHCJS.Marshal.Pure
--import JavaScript.Object.Internal as O

--import JavaScript.Object

--import qualified GHCJS.Types    as T
--import qualified GHCJS.Foreign  as F
--import qualified GHCJS.Foreign.Callback  as F

--import JavaScript.TypedArray as A
import JavaScript.Array
--import GHCJS.Buffer as BUF

import Crypto.Bls.JavaScript.Common
import JavaScript.TypedArray as A
import qualified Data.ByteString as BS



newtype PublicKey = PublicKey JSVal


instance IsJSVal' PublicKey where jsval' (PublicKey j) = j


foreign import javascript "($1).getFingerprint()"
    js_getFingerprint :: JSVal -> Word32


foreign import javascript "($1).PublicKey.aggregateInsecure($2)"
    js_aggregateInsecure :: JSVal -> JSArray -> JSVal


foreign import javascript "($1).PublicKey.fromBytes($2)"
    js_publicKeyFromBytes :: JSVal -> Uint8Array -> JSVal


serializePublicKey :: PublicKey -> ByteString
serializePublicKey (PublicKey jspubkey) = arr2bs $ js_serialize jspubkey


getFingerprint :: PublicKey -> Word32
getFingerprint (PublicKey jspk) = js_getFingerprint jspk


aggregateInsecurePublicKeys :: [PublicKey] -> PublicKey
aggregateInsecurePublicKeys pks = PublicKey $ js_aggregateInsecure (getJsVal blsModule) (fromList $ map getJsVal pks)

publicKeyFromBytes :: ByteString -> PublicKey
publicKeyFromBytes bytes = PublicKey $ js_publicKeyFromBytes (getJsVal blsModule) (bs2arr bytes)


-- TODO export `operator==()` to JavaScript and use it
publicKeyEq :: PublicKey -> PublicKey -> Bool
publicKeyEq pk1 pk2 = serializePublicKey pk1 == serializePublicKey pk2

