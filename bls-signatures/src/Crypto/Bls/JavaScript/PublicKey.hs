module Crypto.Bls.JavaScript.PublicKey
    ( PublicKey(..)
    , publicKeyGetFingerprint
    , publicKeyInsecureAggregate
    , publicKeySerialize
    , publicKeyFromBytes
    , publicKeyEq
    ) where


import GHCJS.Types
import Data.Word
import Data.ByteString (ByteString)
import JavaScript.Array

import Crypto.Bls.JavaScript.Common
import JavaScript.TypedArray as A
import qualified Data.ByteString as BS

-- * --------------------------------------------------------------------------

foreign import javascript "($1).getFingerprint()"
    js_getFingerprint :: JSVal -> Word32


foreign import javascript "($1).PublicKey.aggregateInsecure($2)"
    js_aggregateInsecure :: JSVal -> JSArray -> JSVal


foreign import javascript "($1).PublicKey.fromBytes($2)"
    js_publicKeyFromBytes :: JSVal -> Uint8Array -> JSVal

-- * --------------------------------------------------------------------------

newtype PublicKey = PublicKey JSVal


instance IsJSVal' PublicKey where jsval' (PublicKey j) = j

-- * --------------------------------------------------------------------------

publicKeySerialize :: PublicKey -> ByteString
publicKeySerialize (PublicKey jspubkey) = arr2bs $ js_serialize jspubkey


publicKeyGetFingerprint :: PublicKey -> Word32
publicKeyGetFingerprint (PublicKey jspk) = js_getFingerprint jspk


publicKeyInsecureAggregate :: [PublicKey] -> PublicKey
publicKeyInsecureAggregate pks = PublicKey $ js_aggregateInsecure (getJsVal blsModule) (fromList $ map getJsVal pks)

publicKeyFromBytes :: ByteString -> PublicKey
publicKeyFromBytes bytes = PublicKey $ js_publicKeyFromBytes (getJsVal blsModule) (bs2arr bytes)


-- TODO export `operator==()` to JavaScript and use it
publicKeyEq :: PublicKey -> PublicKey -> Bool
publicKeyEq pk1 pk2 = publicKeySerialize pk1 == publicKeySerialize pk2

