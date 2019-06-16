{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards     #-}
-- |
-- Abstract API for network which support
module Thundermint.P2P.Network.Internal.TLS (
    -- * Real tls network
    newNetworkTls
  , getCredential
  , getCredentialFromBuffer
  ) where

import Codec.Serialise
import Control.Arrow            (first)
import Control.Monad            (when, forM, join)
import Control.Monad.Catch      (bracketOnError, throwM, MonadMask)
import Control.Monad.Fix        (fix)
import Control.Monad.IO.Class   (MonadIO, liftIO)
import Data.ByteString.Internal (ByteString(..))
import Data.Default.Class       (def)
import Data.List                (find)
import Data.Maybe               (fromJust)
import Data.Monoid              ((<>))
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

import Thundermint.Control
import Thundermint.P2P.Network.Parameters
import Thundermint.P2P.Network.RealNetworkStub
import Thundermint.P2P.Network.Internal.Utils
import Thundermint.P2P.Types


----------------------------------------------------------------
--
----------------------------------------------------------------

newNetworkTls :: TLS.Credential -> PeerInfo -> NetworkAPI
newNetworkTls creds ourPeerInfo = (realNetworkStub ourPeerInfo)
  { listenOn = do
      let hints = Net.defaultHints
            { Net.addrFlags      = [Net.AI_PASSIVE]
            , Net.addrSocketType = Net.Stream
            }
      addrs <- liftIO $ Net.getAddrInfo (Just hints) Nothing (Just serviceName)
      addr  <- case () of
        _ | Just a <- find isIPv6addr addrs -> return a
          | a:_    <- addrs                 -> return a
          | otherwise                       -> throwM NoAddressAvailable
      liftIO $ listenerTls ourPeerInfo creds addr
  --
  , connect  = \addr -> do
      let hints = Just Net.defaultHints
            { Net.addrSocketType = Net.Stream
            }
      (hostName, serviceName') <- liftIO $ Net.getNameInfo
                                            [Net.NI_NUMERICHOST, Net.NI_NUMERICSERV]
                                            True
                                            True
                                            $ netAddrToSockAddr addr
      addrInfo:_ <- liftIO $ Net.getAddrInfo hints hostName serviceName'
      bracketOnError (newSocket addrInfo) (liftIO . Net.close) $ \ sock -> do
        let tenSec = 10000000
        -- Waits for connection for 10 sec and throws `ConnectionTimedOut` exception
        liftIO $ throwNothingM ConnectionTimedOut
               $ timeout tenSec
               $ Net.connect sock $ netAddrToSockAddr addr
        connectTls ourPeerInfo creds hostName serviceName' sock
  }
  where
    serviceName = show $ piPeerPort ourPeerInfo


listenerTls
  :: (MonadIO m, MonadMask m)
  => PeerInfo
  -> TLS.Credential
  -> Net.AddrInfo
  -> IO (m (), m (P2PConnection, NetAddr))
listenerTls selfPI creds addr =
  bracketOnError (newSocket addr) Net.close $ \sock -> do
    when (isIPv6addr addr) $
      Net.setSocketOption sock Net.IPv6Only 0
    Net.bind sock (Net.addrAddress addr)
    Net.listen sock 5
    return ( liftIO $ Net.close sock
           , acceptTls selfPI creds sock
           )

connectTls :: MonadIO m
           => PeerInfo
           -> TLS.Credential
           -> Maybe Net.HostName
           -> Maybe Net.ServiceName
           -> Net.Socket
           -> m P2PConnection
connectTls selfPI creds host port sock = do
        store <- liftIO getSystemCertificateStore
        ctx <- liftIO $ TLS.contextNew sock (mkClientParams (fromJust  host) ( fromJust port) creds store)
        TLS.handshake ctx
        liftIO $ TLS.contextHookSetLogging ctx getLogging
        applyConn ctx selfPI


acceptTls :: (MonadMask m, MonadIO m)
          => PeerInfo -> TLS.Credential -> Net.Socket -> m (P2PConnection, NetAddr)
acceptTls selfPI creds sock =
    bracketOnError
        (liftIO $ Net.accept sock)
        (\(s,_) -> liftIO $ Net.close s)
        (\(s, addr) -> do
           store <- liftIO getSystemCertificateStore
           ctx <- TLS.contextNew s (mkServerParams creds  (Just store))
           liftIO $ TLS.contextHookSetLogging ctx getLogging
           TLS.handshake ctx
           cnn <- applyConn ctx selfPI
           return (cnn, sockAddrToNetAddr addr)

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

setProperPeerInfo :: MonadIO m => PeerInfo -> P2PConnection -> m P2PConnection
setProperPeerInfo selfPI conn@P2PConnection{..} = do
    send $ serialise selfPI
    encodedPeerInfo <- recv
    case encodedPeerInfo of
      Nothing -> fail "connection dropped before receiving peer info"
      Just bs -> case deserialiseOrFail bs of
        Left err -> fail ("unable to deserealize peer info: " ++ show err)
        Right peerInfo -> return $ conn { connectedPeer = peerInfo }

applyConn :: MonadIO m => TLS.Context -> PeerInfo -> m P2PConnection
applyConn context selfPI = do
    ref <- liftIO $ I.newIORef ""
    setProperPeerInfo selfPI $ P2PConnection (tlsSend context)
                                             (tlsRecv context ref)
                                             (liftIO $ tlsClose context)
                                             (PeerInfo (PeerId 0) 0 0)
  where 
    tlsClose :: TLS.Context -> IO ()
    tlsClose ctx = silentBye ctx `E.catch` \(_ :: E.IOException) -> pure ()

    tlsRecv :: MonadIO m
            => TLS.Context -> I.IORef ByteString -> m (Maybe LBS.ByteString)
    tlsRecv ctx ref = liftIO $ do
      -- FIXME: This function is not threadsafe. It is not a problem, because 
      -- P2PConnection is not used concurrently now. To fix that this function
      -- call should be exclusive. 
      b0 <- I.readIORef ref
      (mHeader, headerLeftover) <- recvAll ctx b0 headerSize
      join.join <$> do
        forM mHeader $ \ header ->
          forM (decodeWord16BE header) $ \ n -> do
            (result,leftover) <- recvAll ctx headerLeftover $ fromIntegral n
            I.writeIORef ref leftover
            return result
            
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
