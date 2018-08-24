{-# LANGUAGE BangPatterns        #-}
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

import Control.Concurrent.STM

import Control.Concurrent     (forkIO, killThread)
import Control.Monad          (forM_, forever, void, when)
import Control.Monad.Catch    (bracket, bracketOnError, onException, throwM)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Bits              (unsafeShiftL)
import Data.Functor           (($>))
import Data.List              (find)
import Data.Maybe             (fromJust, fromMaybe)
import Data.Monoid            ((<>))
import Data.Word              (Word32)
import System.Timeout         (timeout)

import qualified Control.Exception          as E
import qualified Data.ByteString            as BS
import           Data.X509
import           Data.X509.CertificateStore
import           Network.Simple.TCP.TLS
import qualified Network.TLS                as TLS
import qualified Network.TLS.Extra          as TLSExtra
-- import qualified Network.TLS.Hooks          as TLSHooks
import qualified Crypto.Random.AESCtr
import           Data.Default.Class   (def)


import qualified Data.ByteString.Builder        as BB
import qualified Data.ByteString.Lazy           as LBS
import qualified Data.Map                       as Map
import qualified Network.Socket                 as Net
import qualified Network.Socket.ByteString      as NetBS
import qualified Network.Socket.ByteString.Lazy as NetLBS

import Thundermint.Control
import Thundermint.P2P.Tls
import Thundermint.P2P.Types

----------------------------------------------------------------
--
----------------------------------------------------------------
headerSize :: HeaderSize
headerSize = 4





----------------------------------------------------------------
--
----------------------------------------------------------------
newSocket :: MonadIO m => Net.AddrInfo -> m Net.Socket
newSocket ai = liftIO $ Net.socket (Net.addrFamily     ai)
                                   (Net.addrSocketType ai)
                                   (Net.addrProtocol   ai)

{-sockParams :: T.Credentials -> T.ServerParams
sockParams creds = def { T.serverWantClientCert = False
                       , T.serverShared         = shared creds
                       , T.serverSupported      = supported
                       }
-}

{-instance HasBackend NetworkAPI{..} where
    initializeBackend _ = return ()
    getBackend sock = Backend (return ()) (Network.close sock) (Network.sendAll sock) recvAll
      where recvAll n = B.concat <$> loop n
              where loop 0    = return []
                    loop left = do
                        r <- safeRecv sock left
                        if B.null r
                            then return []
                            else liftM (r:) (loop (left - B.length r))
-}
-- | API implementation for real tcp network
-- realNetworkTls :: Net.ServiceName -> NetworkAPI Net.SockAddr
-- | Makes an TLS context `T.Backend` from a `Socket`.
socketBackend :: Socket -> TLS.Backend
socketBackend sock = do
    TLS.Backend (return ()) (Net.close sock) (NetBS.sendAll sock) recvAll
  where
    recvAll = step BS.empty
       where step !acc 0 = return acc
             step !acc n = do
                bs <- NetBS.recv sock n
                step (acc `BS.append` bs) (n - BS.length bs)


{-- socketBackend' :: NetworkAPI addr -> TLS.Backend
socketBackend' NetworkAPI{..} = do
    (c, a) <- listenOn
    (conn, vv) <- a
    return $ TLS.Backend (return ())
           (c)
           (Thundermint.P2P.Types.send conn) (Thundermint.P2P.Types.recv conn)
-}


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

{-      bracketOnError ( liftIO $ do
                       sock <- newSocket addr
                       when (isIPv6addr addr) $
                            Net.setSocketOption sock Net.IPv6Only 0
                       Net.bind sock (Net.addrAddress addr)
                       Net.listen sock 5
                       ctx <- TLS.contextNew (socketBackend sock) (mkServerSettings creds)
                       TLS.handshake ctx
                       pure ctx)
                       (liftIO .(\ctx -> TLS.bye ctx `E.catch` \(_ :: E.IOException) -> pure ()))
        (\ctx -> return (TLS.bye ctx , accept ctx))
-}
      bracketOnError (liftIO $ newSocket addr) (liftIO . Net.close) $ \sock -> liftIO $ do
        when (isIPv6addr addr) $
          Net.setSocketOption sock Net.IPv6Only 0
        Net.bind sock (Net.addrAddress addr)
        Net.listen sock 5
        --bracket (do
        ctx <- TLS.contextNew (socketBackend sock) (mkServerSettings creds)
        TLS.handshake ctx
        return (liftIO $ TLS.bye ctx `E.catch` \(_ :: E.IOException) -> pure (), handleAccept sock ctx)

  , connect  = \addr -> do
      let hints = Just Net.defaultHints
            { Net.addrSocketType = Net.Stream
            }
      (hostName, serviceName) <- liftIO $ Net.getNameInfo [] True True addr
      addrInfo:_ <- liftIO $ Net.getAddrInfo hints hostName serviceName
      bracketOnError (newSocket addrInfo) (liftIO . Net.close) $ \ sock -> do
        let tenSec = 10000000
        -- Waits for connection for 10 sec and throws `ConnectionTimedOut` exception
        liftIO $ throwNothingM ConnectionTimedOut
               $ timeout tenSec
               $ Net.connect sock addr
        ctx <- TLS.contextNew sock (mkClientSettings (fromJust hostName) (fromJust serviceName))
        TLS.handshake ctx
        return $ applyConn ctx
  }
 where
  isIPv6addr = (==) Net.AF_INET6 . Net.addrFamily

handleAccept :: MonadIO m => Net.Socket -> TLS.Context  -> m (Connection, Net.SockAddr)
handleAccept sock ctx = do -- applyConn' ctx undefined -- do
    (_, addr) <- liftIO $ Net.accept sock
    return $ (applyConn ctx, addr)


applyConn :: TLS.Context -> Connection
applyConn ctx =
    Connection ( TLS.sendData ctx)
                   ( do
                     bs <- TLS.recvData ctx
                     return . Just $ LBS.fromStrict bs)
                   (TLS.bye ctx)
