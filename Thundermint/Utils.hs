-- | Some small but useful functions
--
{-# LANGUAGE CPP #-}
module Thundermint.Utils
   ( -- * PeerId encode/decode
     PeerId
   , peerIdLength
   , encodePeerId
   , decodePeerId
     -- * Timings
   , waitSec
   ) where


#include "MachDeps.h"


import Control.Concurrent (threadDelay)
import Control.Monad.IO.Class
import Data.Word
import Foreign.ForeignPtr
import Foreign.Ptr
import Foreign.Storable
import System.IO.Unsafe
import qualified Data.ByteString.Internal as B
import qualified Data.ByteString.Lazy as BL


type PeerId = Word64


peerIdLength :: Int
peerIdLength = 8


#ifdef WORDS_BIGENDIAN

    Sorry! decodePeerId & encodePeerId not implemented on your arch yet!

#else


encodePeerId :: PeerId -> BL.ByteString
encodePeerId m = BL.fromStrict $ B.unsafeCreate peerIdLength (\p -> poke (castPtr p) m)


decodePeerId :: BL.ByteString -> Either String PeerId
decodePeerId bs =
    let (fp', ofs, len) = B.toForeignPtr (BL.toStrict bs)
        fp = plusForeignPtr fp' ofs
    in
    if ofs == 0 then
        if len == peerIdLength
        then unsafePerformIO $ withForeignPtr fp (fmap Right . peek . castPtr)
        else Left ("Wrong PeerId length (expected = " ++ show peerIdLength ++ ", actual = " ++ show len ++ ")")
    else
        Left ("Wrong ofs = " ++ show ofs)

#endif



-- | Wait a few seconds
--
waitSec :: (MonadIO m) => Double -> m ()
waitSec sec = liftIO $ threadDelay $ round $ 1e6 * sec
