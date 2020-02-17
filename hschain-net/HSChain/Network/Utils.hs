{-# LANGUAGE RecordWildCards #-}

module HSChain.Network.Utils where

import Control.Monad.IO.Class ( MonadIO, liftIO )
import Data.Bits              ( unsafeShiftL )
import Data.Word              ( Word32 )

import qualified Data.ByteString.Lazy as LBS
import qualified Network.Socket       as Net

type HeaderSize = Int

headerSize :: HeaderSize
headerSize = 4

decodeWord16BE :: LBS.ByteString -> Maybe Word32
decodeWord16BE bs | LBS.length bs < fromIntegral headerSize = Nothing
                  | otherwise =
                      let w8s = LBS.unpack $ LBS.take (fromIntegral headerSize) bs
                          shiftBy = (*) 8
                          word32 = foldr (\b (acc, i) ->
                                         (fromIntegral b `unsafeShiftL` shiftBy i + acc, i + 1))
                                   (0,0)
                                   w8s
                      in (Just $ fst word32)

newSocket :: MonadIO m => Net.AddrInfo -> m Net.Socket
newSocket ai = liftIO $ do
    sock <- Net.socket (Net.addrFamily     ai)
                       (Net.addrSocketType ai)
                       (Net.addrProtocol   ai)
    Net.setSocketOption sock Net.NoDelay 1
    Net.setSocketOption sock Net.ReuseAddr 1
    return sock

-- | Check whether socket is IP6
isIPv6addr :: Net.AddrInfo -> Bool
isIPv6addr = (==) Net.AF_INET6 . Net.addrFamily
