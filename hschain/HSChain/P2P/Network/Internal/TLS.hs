{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NumDecimals         #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- |
-- Abstract API for network which support
module HSChain.P2P.Network.Internal.TLS (
    -- * Real tls network
    newNetworkTls
  , getCredential
  , getCredentialFromBuffer
  ) where

import Control.Arrow            (first)
import Control.Monad            (when, forM, join)
import Control.Monad.Catch      (bracketOnError, throwM, MonadMask)
import Control.Monad.Fix        (fix)
import Control.Monad.IO.Class   (MonadIO, liftIO)
import Data.ByteString.Internal (ByteString(..))
import Data.Default.Class       (def)
import Data.List                (find)
import Data.Monoid              ((<>))
import Data.Word
import Foreign.C.Error          (Errno(Errno), ePIPE)
import System.IO.Error          (isEOFError)
import System.Timeout           (timeout)
import System.X509              (getSystemCertificateStore)

import qualified Control.Exception       as E

import qualified Data.ByteString         as BS
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Lazy    as LBS
import qualified Data.IORef              as I
import qualified GHC.IO.Exception        as Eg
import qualified Network.Socket          as Net
import qualified Network.TLS             as TLS

import HSChain.Control
import HSChain.P2P.Network.Parameters
import HSChain.P2P.Network.RealNetworkStub
import HSChain.P2P.Network.Internal.Utils
import HSChain.P2P.Network.IpAddresses     (getNetAddrPort)
import HSChain.P2P.Types


----------------------------------------------------------------
--
----------------------------------------------------------------

newNetworkTls :: TLS.Credential -> PeerInfo -> NetworkAPI
newNetworkTls creds ourPeerInfo = (realNetworkStub ourPeerInfo)
  { listenOn = do
      addrs <- liftIO $ Net.getAddrInfo (Just tcpListenHints) Nothing (Just serviceName)
      addr  <- case () of
        _ | Just a <- find isIPv6addr addrs -> return a
          | a:_    <- addrs                 -> return a
          | otherwise                       -> throwM NoAddressAvailable
      liftIO $ listenerTls creds addr
  --
  , connect  = \addr -> do
      let port = getNetAddrPort addr
      (addrInfo,sockAddr,mhostName) <- netAddrToAddrInfo addr
      hostName <- throwNothing CantReverseLookipHostname mhostName
      bracketOnError (newSocket addrInfo) (liftIO . Net.close) $ \ sock -> do
        -- Waits for connection for 10 sec and throws `ConnectionTimedOut` exception
        liftIO $ throwNothingM ConnectionTimedOut
               $ timeout 10e6
               $ Net.connect sock sockAddr
        connectTls creds hostName port sock
  }
  where
    serviceName = show $ piPeerPort ourPeerInfo


listenerTls
  :: (MonadIO m, MonadMask m)
  => TLS.Credential
  -> Net.AddrInfo
  -> IO (m (), m (P2PConnection, NetAddr))
listenerTls creds addr =
  bracketOnError (newSocket addr) Net.close $ \sock -> do
    when (isIPv6addr addr) $
      Net.setSocketOption sock Net.IPv6Only 0
    Net.bind sock (Net.addrAddress addr)
    Net.listen sock 5
    return ( liftIO $ Net.close sock
           , acceptTls creds sock
           )

connectTls :: MonadIO m
           => TLS.Credential
           -> Net.HostName
           -> Word16
           -> Net.Socket
           -> m P2PConnection
connectTls creds host port sock = liftIO $ do
  store <- getSystemCertificateStore
  ctx   <- TLS.contextNew sock
         $ mkClientParams host (show port) creds store
  TLS.handshake ctx
  TLS.contextHookSetLogging ctx getLogging
  applyConn ctx


acceptTls :: (MonadMask m, MonadIO m)
          => TLS.Credential -> Net.Socket -> m (P2PConnection, NetAddr)
acceptTls creds sock =
    bracketOnError
        (liftIO $ Net.accept sock)
        (\(s,_) -> liftIO $ Net.close s)
        (\(s, addr) -> do
           store <- liftIO getSystemCertificateStore
           ctx <- TLS.contextNew s (mkServerParams creds  (Just store))
           liftIO $ TLS.contextHookSetLogging ctx getLogging
           TLS.handshake ctx
           let netAddr = sockAddrToNetAddr addr
           cnn <- liftIO $ applyConn ctx
           return (cnn, netAddr)
       )


-- | Like 'TLS.bye' from the "Network.TLS" module, except it ignores 'ePIPE'
-- errors which might happen if the remote peer closes the connection first.
-- from Network.Simple.TCP.TLS module
silentBye :: TLS.Context -> IO ()
silentBye ctx =
    E.catch (TLS.bye ctx) $ \e -> case e of
        Eg.IOError{ Eg.ioe_type  = Eg.ResourceVanished
                  , Eg.ioe_errno = Just ioe
                  } | Errno ioe == ePIPE
          -> return ()
        _ -> E.throwIO e

applyConn :: TLS.Context
          -> IO P2PConnection
applyConn context = do
  ref <- liftIO $ I.newIORef ""
  return $
     P2PConnection (tlsSend context)
                   (tlsRecv context ref)
                   (liftIO $ tlsClose context)
                   (PeerInfo (PeerId 0) 0 0)
  where 
    tlsClose :: TLS.Context -> IO ()
    tlsClose ctx = silentBye ctx `E.catch` \(_ :: E.IOException) -> pure ()

    tlsRecv :: MonadIO m
            => TLS.Context -> I.IORef ByteString -> m LBS.ByteString
    tlsRecv ctx ref = liftIO $ do
      -- FIXME: This function is not threadsafe. It is not a problem, because 
      -- P2PConnection is not used concurrently now. To fix that this function
      -- call should be exclusive. 
      b0 <- I.readIORef ref
      (mHeader, headerLeftover) <- recvAll ctx b0 headerSize
      res <- do
        forM mHeader $ \ header ->
          forM (decodeWord16BE header) $ \ n -> do
            (result,leftover) <- recvAll ctx headerLeftover $ fromIntegral n
            I.writeIORef ref leftover
            return result
      case join $ join res of
        Nothing -> throwM ConnectionClosed
        Just a  -> return a

    tlsSend :: MonadIO m
            => TLS.Context -> LBS.ByteString -> m ()
    tlsSend ctx = TLS.sendData ctx . BB.toLazyByteString . toFrame
      where
        toFrame msg = let len = fromIntegral $ LBS.length msg
                      in (BB.word32BE len <> BB.lazyByteString msg)

-------------------------------------------------------------------------------
-- framing for tls
-------------------------------------------------------------------------------

-- | Reads from TLS context specified bytes number taking in account leftovers
recvAll :: TLS.Context
        -> BS.ByteString
        -> Int
        -> IO (Maybe LBS.ByteString, BS.ByteString)
recvAll ctx bs0 size0
  | size0 <= len0 = return
                  $ first (Just . LBS.fromStrict)
                  $ BS.splitAt size0 bs0 
  | otherwise     = fix go (BB.byteString bs0) (size0 - len0)
  where
    len0 = BS.length bs0
    go loop b sizeLeft = do
      mBs <- recvT ctx
      case mBs of
        Nothing -> return (Nothing,"")
        Just bs -> do
          let receivedLen = BS.length bs
              b'          = b <> BB.byteString bs
          if receivedLen < sizeLeft
             then
               loop b' (sizeLeft - receivedLen)
             else do
               let (r, leftover) = LBS.splitAt (fromIntegral size0) $ BB.toLazyByteString b'
               return (Just r, LBS.toStrict leftover)

-- | recvData wrapper which converts EOF to Maybe ByteString
recvT :: TLS.Context -> IO (Maybe ByteString)
recvT ctx =  E.handle onEOF $ do
    x <- TLS.recvData ctx
    return $ if BS.null x
                then Nothing -- never should happend
                else Just x
  where
    onEOF e
      | Just TLS.Error_EOF <- E.fromException e = return Nothing
      | Just ioe <- E.fromException e
      , isEOFError ioe                          = return Nothing
      | otherwise                               = E.throwIO e

-------------------------------------------------------------------------------
-- debuger hooks
-------------------------------------------------------------------------------

ioDebug :: Bool
debug = False

debug :: Bool
ioDebug = False

getLogging :: TLS.Logging
getLogging = ioLogging $ packetLogging def

packetLogging :: TLS.Logging -> TLS.Logging
packetLogging logging
            | debug = logging { TLS.loggingPacketSent = \packet -> putStrLn ("C: PacketSent " ++ show packet)
                              , TLS.loggingPacketRecv = \packet -> putStrLn ("C: PacketRecv " ++ show packet)
                              }
            | otherwise = logging


ioLogging :: TLS.Logging -> TLS.Logging
ioLogging logging
            | ioDebug = logging { TLS.loggingIOSent = \io -> putStrLn ("C: IOSent " ++ show io)
                                , TLS.loggingIORecv = \header io -> putStrLn ("C: IORecv header:" ++ show header ++ " io:" ++ show io)
                                }
            | otherwise = logging
