module Crypto.Bls.JavaScript.PrivateKey
    ( PrivateKey(..)
    , privateKeyDeserialize
    , privateKeyEq
    , privateKeyFromSeed
    , privateKeyGetPublicKey
    , privateKeyInsecureAggregate
    , privateKeySerialize
    , privateKeySizeGet
    ) where



import Data.ByteString (ByteString)
import GHCJS.Types
import JavaScript.Array
import JavaScript.TypedArray as A
import qualified Data.ByteString as BS

import Crypto.Bls.JavaScript.Common
import Crypto.Bls.JavaScript.PublicKey

-- * --------------------------------------------------------------------------

foreign import javascript "($1).PrivateKey.fromSeed($2)"
    js_fromSeed :: JSVal -> Uint8Array -> JSVal

foreign import javascript "($1).PrivateKey.fromBytes($2, $3)"
    js_fromBytes :: JSVal -> Uint8Array -> Int -> JSVal

foreign import javascript "($1).getPublicKey()"
    js_getPublicKey :: JSVal -> JSVal

foreign import javascript "($1).PrivateKey.aggregateInsecure($2)"
    js_aggregateInsecure :: JSVal -> JSArray -> JSVal

foreign import javascript "($1).PrivateKey.PRIVATE_KEY_SIZE"
    js_privateKeySize :: JSVal -> Int

-- * --------------------------------------------------------------------------

newtype PrivateKey = PrivateKey JSVal


instance IsJSVal' PrivateKey where jsval' (PrivateKey j) = j

-- * --------------------------------------------------------------------------

privateKeyGetPublicKey :: PrivateKey -> PublicKey
privateKeyGetPublicKey (PrivateKey jspk) = PublicKey $ js_getPublicKey jspk


privateKeyInsecureAggregate :: [PrivateKey] -> PrivateKey
privateKeyInsecureAggregate pks = PrivateKey $ js_aggregateInsecure (getJsVal blsModule) (fromList $ map getJsVal pks)

privateKeySerialize :: PrivateKey -> ByteString
privateKeySerialize (PrivateKey jspk) = arr2bs $ js_serialize jspk


privateKeyFromSeed :: ByteString -> PrivateKey
privateKeyFromSeed seed = PrivateKey $ js_fromSeed (getJsVal blsModule) (bs2arr seed)


privateKeyDeserialize :: ByteString -> Maybe PrivateKey
privateKeyDeserialize bytes = Just $ PrivateKey $ js_fromBytes (getJsVal blsModule) (bs2arr bytes) (BS.length bytes) -- TODO Catch errors! 


-- TODO export `operator==()` to JavaScript and use it
privateKeyEq :: PrivateKey -> PrivateKey -> Bool
privateKeyEq pk1 pk2 = privateKeySerialize pk1 == privateKeySerialize pk2

privateKeySizeGet :: Int
privateKeySizeGet = js_privateKeySize (getJsVal blsModule)
