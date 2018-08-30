{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- |
-- Abstract API for network which support
module Thundermint.P2P.NetworkTls (
    -- * Real tls network
   realNetworkTls
  ) where

import Control.Monad          (when)
import Control.Monad.Catch    (bracketOnError, throwM)
import Control.Monad.Catch    (MonadMask)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.List              (find)
import Data.Maybe             (fromJust, fromMaybe)
import System.Timeout         (timeout)

import           Foreign.C.Error  (Errno(Errno), ePIPE)
import qualified GHC.IO.Exception as Eg

import qualified Control.Exception  as E
import qualified Data.ByteString    as BS
import           Data.Default.Class (def)
import qualified Network.TLS        as TLS

import qualified Data.ByteString.Lazy as LBS
import qualified Network.Socket       as Net

import System.X509           (getSystemCertificateStore)
import Thundermint.Control
import Thundermint.P2P.Tls
import Thundermint.P2P.Types

----------------------------------------------------------------
--
----------------------------------------------------------------
realNetworkTls :: TLS.Credential -> Net.ServiceName -> NetworkAPI Net.SockAddr
realNetworkTls creds listenPort = NetworkAPI
  { listenOn = do
      let hints = Net.defaultHints
            { Net.addrFlags      = [Net.AI_PASSIVE]
            , Net.addrSocketType = Net.Stream
            }
      addrs <- liftIO $ Net.getAddrInfo (Just hints) Nothing (Just listenPort)

      when (null addrs) $
        throwM NoAddressAvailable
      let addr = fromMaybe (head addrs) $ find isIPv6addr addrs

      bracketOnError (liftIO $ listenerTls addr) (liftIO .  Net.close . fst)
                     (\(sock, _) -> liftIO $
                                    return (liftIO $ Net.close sock, acceptTls creds sock))

  , connect  = \addr -> do
      let hints = Just Net.defaultHints
            { Net.addrSocketType = Net.Stream
            }
      (hostName, serviceName) <- liftIO $ Net.getNameInfo
                                            [Net.NI_NUMERICHOST, Net.NI_NUMERICSERV]
                                            True
                                            True
                                            addr


      addrInfo:_ <- liftIO $ Net.getAddrInfo hints hostName serviceName
      bracketOnError (newSocket addrInfo) (liftIO . Net.close) $ \ sock -> do
        let tenSec = 10000000
        -- Waits for connection for 10 sec and throws `ConnectionTimedOut` exception
        liftIO $ throwNothingM ConnectionTimedOut
               $ timeout tenSec
               $ Net.connect sock addr

        connectTls creds hostName serviceName sock
  }

newSocket :: MonadIO m => Net.AddrInfo -> m Net.Socket
newSocket ai = liftIO $ Net.socket (Net.addrFamily     ai)
                                   (Net.addrSocketType ai)
                                   (Net.addrProtocol   ai)


isIPv6addr :: Net.AddrInfo -> Bool
isIPv6addr = (==) Net.AF_INET6 . Net.addrFamily

listenerTls :: Net.AddrInfo -> IO (Net.Socket, Net.AddrInfo)
listenerTls addr = do
      sock <- newSocket addr
      when (isIPv6addr addr) $
        Net.setSocketOption sock Net.IPv6Only 0
      Net.bind sock (Net.addrAddress addr)
      Net.listen sock 5
      return (sock, addr)

connectTls :: MonadIO m =>
              TLS.Credential
           -> Maybe Net.HostName
           -> Maybe Net.ServiceName
           -> Net.Socket
           -> m Connection
connectTls creds host port sock = do
        store <- liftIO $ getSystemCertificateStore
        ctx <- liftIO $ TLS.contextNew sock (mkClientParams (fromJust  host) ( fromJust port) creds store)
        TLS.handshake ctx
        liftIO $ TLS.contextHookSetLogging ctx getLogging
        return $ applyConn ctx


acceptTls :: (MonadMask m, MonadIO m) => TLS.Credential -> Net.Socket -> m (Connection, Net.SockAddr)
acceptTls creds sock = do
    bracketOnError
        (liftIO $ Net.accept sock)
        (\(s,_) -> liftIO $ Net.close s)
        (\(s, addr) -> do
           ctx <- TLS.contextNew s (mkServerParams creds )
           liftIO $ TLS.contextHookSetLogging ctx getLogging
           TLS.handshake ctx
           return $ (applyConn ctx, addr)

        )


-- | Like 'TLS.bye' from the "Network.TLS" module, except it ignores 'ePIPE'
-- errors which might happen if the remote peer closes the connection first.
-- from Network.Simple.TCP.TLS module
silentBye :: TLS.Context -> IO ()
silentBye ctx = do
    E.catch (TLS.bye ctx) $ \e -> case e of
        Eg.IOError{ Eg.ioe_type  = Eg.ResourceVanished
                  , Eg.ioe_errno = Just ioe
                  } | Errno ioe == ePIPE
          -> return ()
        _ -> E.throwIO e

applyConn :: TLS.Context -> Connection
applyConn context =
    Connection (send context) (recv context) (liftIO $ tlsClose context)

        where

          tlsClose ctx = (silentBye ctx `E.catch` \(_ :: E.IOException) -> pure ())
          -- | https://hackage.haskell.org/package/network-simple-tls-0.3/docs/src/Network.Simple.TCP.TLS.html#useTls
          -- Up to @16384@ decrypted bytes will be received at once.
          recv :: MonadIO m => TLS.Context -> m (Maybe LBS.ByteString)
          recv ctx = liftIO $ do
                       E.handle (\TLS.Error_EOF -> return Nothing)
                            (do bs <- TLS.recvData ctx
                                if BS.null bs
                                then return Nothing -- I think this never happens
                                else return (Just $ LBS.fromStrict bs))

          send :: MonadIO m => TLS.Context -> LBS.ByteString -> m ()
          send ctx = \bs -> TLS.sendData ctx  bs


-------------------------------------------------------------------------------
-- debuger hooks
-------------------------------------------------------------------------------

ioDebug :: Bool
debug = False

debug :: Bool
ioDebug = False

getLogging :: TLS.Logging
getLogging = ioLogging $ packetLogging $ def

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
