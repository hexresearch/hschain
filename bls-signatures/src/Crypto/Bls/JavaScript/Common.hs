module Crypto.Bls.JavaScript.Common where


import Control.Concurrent.MVar
import Data.ByteString (ByteString)
import Data.IORef
import GHCJS.Buffer as BUF
import GHCJS.Types
import JavaScript.TypedArray as A
import System.IO.Unsafe
import qualified Data.ByteString as BS
import qualified GHCJS.Foreign.Callback as F


-- foreign import javascript unsafe "console.log($1)"
--     js_consolelog :: JSVal -> IO ()

foreign import javascript "($1).serialize()"
    js_serialize :: JSVal -> Uint8Array

foreign import javascript unsafe "require('blsjs')().then($1)"
    js_loadBlsCallback :: (F.Callback (JSVal -> IO ())) -> IO ()

foreign import javascript "crypto_.createHash('sha256').update($1).digest()"
    js_hash256 :: Uint8Array -> Uint8Array


-- TODO вынести в отдельный модуль
newtype Hash256 = Hash256 { unHash256 :: ByteString }


hash256 :: ByteString -> Hash256
hash256 = Hash256 . arr2bs . js_hash256 . bs2arr


hash256serialize :: Hash256 -> ByteString
hash256serialize (Hash256 bs) = bs


-- TODO взять реализацию из NaCL, bsToArray / arrayToBs
bs2arr :: ByteString -> Uint8Array
bs2arr bs = arr
  where
    (buf, off, len) = fromByteString (BS.copy bs)
    arr         = subarray off len $ getUint8Array buf -- TODO разобраться с превышением длины


arr2bs :: Uint8Array -> ByteString
arr2bs arr = bs
  where
    buf = createFromArrayBuffer $ buffer arr
    bs  = BUF.toByteString 0 Nothing buf


newtype BlsModule = BlsModule JSVal


-- IsJSVal hidden, so we introduce own
class IsJSVal' a where
    jsval' :: a -> JSVal

getJsVal :: (IsJSVal' a) => a -> JSVal
getJsVal = jsval'


instance IsJSVal' BlsModule where jsval' (BlsModule j) = j


blsModuleRef :: IORef BlsModule
blsModuleRef = unsafePerformIO $ newIORef $ BlsModule $ nullRef
{-# NOINLINE blsModule #-}


blsModule :: BlsModule
blsModule = unsafePerformIO $ readIORef $ blsModuleRef


-- | This function load and initialize BLS library.
-- TODO update comments
--
--   Minimal definition is just:
--
--   ```
--   withBls :: (BlsModule -> IO()) -> IO ()
--   withBls act = F.syncCallback1 F.ContinueAsync (act . BlsModule) >>= loadBlsCallback
--   ```
--
--   But in this case `act` will run in different JS thread
--   and output to console with internal Haskell functions
--   such as `putStrLn`, `Debug.Trace.trace` will not be
--   available.
--
--   So we pass `bls` to current thread and execute `act` in
--   this current thread.
--
initBls :: IO ()
initBls = do
    vbls <- newEmptyMVar
    F.syncCallback1 F.ContinueAsync (putMVar vbls) >>= js_loadBlsCallback
    bls <- takeMVar vbls
    let blsm = BlsModule bls
    writeIORef blsModuleRef blsm
    -- TODO set up BLS value in JavaScript, not in haskell.


