{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
module HSChain.Network.TCP
  ( newNetworkTcp
  ) where

import Control.Monad          (when)
import Control.Monad.Catch    (bracketOnError, throwM)
import Control.Monad.IO.Class (liftIO, MonadIO)
import Data.List              (find)
import Data.Monoid            ((<>))
import Data.Word
import System.Timeout         (timeout)

import qualified Data.ByteString.Builder        as BB
import qualified Data.ByteString.Lazy           as LBS
import qualified Network.Socket                 as Net
import qualified Network.Socket.ByteString.Lazy as NetLBS

import HSChain.Network.Utils
import HSChain.Network.Internal
import HSChain.Network.Types


-- | API implementation for real tcp network
newNetworkTcp :: Word16 -> NetworkAPI
newNetworkTcp selfPeerInfo = NetworkAPI
  { listenOn = do
      addrs <- liftIO $ Net.getAddrInfo (Just tcpListenHints) Nothing (Just serviceName)
      addr  <- if
        | Just a <- find isIPv6addr addrs -> return a
        | a:_    <- addrs                 -> return a
        | otherwise                       -> throwM NoAddressAvailable
      --
      bracketOnError (liftIO $ newSocket addr) (liftIO . Net.close) $ \sock -> liftIO $ do
        when (isIPv6addr addr) $
          Net.setSocketOption sock Net.IPv6Only 0
        Net.bind sock (Net.addrAddress addr)
        Net.listen sock 5
        return (liftIO $ Net.close sock, accept sock)
  --
  , connect  = \addr -> do
      (addrInfo,sockAddr,_) <- netAddrToAddrInfo addr
      bracketOnError (newSocket addrInfo) (liftIO . Net.close) $ \sock -> do
        let tenSec = 10000000
        -- Waits for connection for 10 sec and throws `ConnectionTimedOut` exception
        liftIO $ maybe (throwM ConnectionTimedOut) return
             =<< timeout tenSec (Net.connect sock sockAddr)
        return $ applyConn sock
  , listenPort = fromIntegral selfPeerInfo
  }
 where
  serviceName = show selfPeerInfo

accept :: (MonadIO m)
       => Net.Socket -> m (P2PConnection, NetAddr)
accept sock = do
  (conn, addr) <- liftIO $ Net.accept sock
  return ( applyConn conn
         , sockAddrToNetAddr addr
         )

applyConn :: Net.Socket -> P2PConnection
applyConn conn = P2PConnection (liftIO . sendBS conn) (liftIO $ recvBS conn) (liftIO $ Net.close conn)

sendBS :: Net.Socket -> LBS.ByteString -> IO ()
sendBS sock =  \s -> NetLBS.sendAll sock (BB.toLazyByteString $ toFrame s)
               where
                 toFrame msg = let len =  fromIntegral (LBS.length msg) :: Word32
                                   hexLen = BB.word32BE len
                               in (hexLen <> BB.lazyByteString msg)

recvBS :: Net.Socket -> IO LBS.ByteString
recvBS sock = do
  header <- recvAll sock headerSize
  if LBS.null header
  then throwM ConnectionClosed
  else let len = decodeWord16BE header
       in case len of
            Just n  -> recvAll sock (fromIntegral n)
            Nothing -> throwM ConnectionClosed


-- | helper function read given length of bytes
recvAll :: Net.Socket -> Int -> IO LBS.ByteString
recvAll sock n = LBS.concat `fmap` loop (fromIntegral n)
  where
    loop 0    = return []
    loop left = do
      r <- NetLBS.recv sock left
      if LBS.null r
      then return []
      else fmap (r:) (loop (left - LBS.length r))

