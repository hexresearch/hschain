{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
-- |
-- Abstract API for network which support
module Thundermint.P2P.Network.TLS (
    -- * Real tls network
    realNetworkTls
  , newSocket
  , getCredential
  , getCredentialFromBuffer
  , headerSize
  ) where

import Codec.Serialise
import Control.Monad            (when)
import Control.Monad.Catch      (bracketOnError, throwM, MonadMask)
import Control.Monad.IO.Class   (MonadIO, liftIO)
import Data.Bits                (unsafeShiftL)
import Data.ByteString.Internal (ByteString(..))
import Data.Default.Class       (def)
import Data.List                (find)
import Data.Maybe               (fromJust, fromMaybe)
import Data.Monoid              ((<>))
import Data.Word                (Word32)
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
import Thundermint.P2P.Types
----------------------------------------------------------------
--
----------------------------------------------------------------

headerSize :: HeaderSize
headerSize = 4



realNetworkTls :: TLS.Credential -> PeerInfo -> NetworkAPI
realNetworkTls creds ourPeerInfo = (realNetworkStub serviceName)
  { listenOn = do
      let hints = Net.defaultHints
            { Net.addrFlags      = [Net.AI_PASSIVE]
            , Net.addrSocketType = Net.Stream
            }
      addrs <- liftIO $ Net.getAddrInfo (Just hints) Nothing (Just serviceName)

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
        connectTls creds hostName serviceName' sock
  }
  where
    serviceName = show $ piPeerPort ourPeerInfo

newSocket :: MonadIO m => Net.AddrInfo -> m Net.Socket
newSocket ai = liftIO $ do
    sock <- Net.socket (Net.addrFamily     ai)
                       (Net.addrSocketType ai)
                       (Net.addrProtocol   ai)
    Net.setSocketOption sock Net.NoDelay 1
    Net.setSocketOption sock Net.ReuseAddr 1
    return sock


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
           -> m P2PConnection
connectTls creds host port sock = do
        store <- liftIO $ getSystemCertificateStore
        ctx <- liftIO $ TLS.contextNew sock (mkClientParams (fromJust  host) ( fromJust port) creds store)
        TLS.handshake ctx
        liftIO $ TLS.contextHookSetLogging ctx getLogging
        conn <- applyConn ctx
        return $ conn


acceptTls :: (MonadMask m, MonadIO m) => TLS.Credential -> Net.Socket -> m (P2PConnection, NetAddr)
acceptTls creds sock = do
    bracketOnError
        (liftIO $ Net.accept sock)
        (\(s,_) -> liftIO $ Net.close s)
        (\(s, addr) -> do
           store <- liftIO $ getSystemCertificateStore
           ctx <- TLS.contextNew s (mkServerParams creds  (Just store))
           liftIO $ TLS.contextHookSetLogging ctx getLogging
           TLS.handshake ctx
           cnn <- applyConn ctx
           return $ (cnn, sockAddrToNetAddr addr)

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

setProperPeerInfo :: MonadIO m => P2PConnection -> m P2PConnection
setProperPeerInfo conn@P2PConnection{..} = do
    encodedPeerInfo <- recv
    case encodedPeerInfo of
      Nothing -> fail "connection dropped before receiving peer info"
      Just bs -> case deserialiseOrFail bs of
        Left err -> fail ("unable to deserealize peer info: " ++ show err)
        Right peerInfo -> return $ conn { connectedPeer = peerInfo }

applyConn :: MonadIO m => TLS.Context -> m P2PConnection
applyConn context = do
    ref <- liftIO $ I.newIORef ""
    setProperPeerInfo $ P2PConnection (tlsSend context) (tlsRecv context ref) (liftIO $ tlsClose context)
      (PeerInfo (PeerId 0) 0 0)

        where
          tlsClose ctx = (silentBye ctx `E.catch` \(_ :: E.IOException) -> pure ())

          tlsRecv ctx ref = liftIO $ do
            header <- recvBufT' ctx ref headerSize
            if LBS.null header
            then return Nothing
            else let len = decodeWord16BE header
                 in case len of
                      Just n  -> Just <$> (recvBufT' ctx ref (fromIntegral n))
                      Nothing -> return Nothing

          tlsSend ctx =  \s -> TLS.sendData ctx (BB.toLazyByteString $ toFrame s)
                 where
                   toFrame msg = let len =  fromIntegral (LBS.length msg) :: Word32
                                     hexLen = BB.word32BE len
                                 in (hexLen <> BB.lazyByteString msg)

-------------------------------------------------------------------------------
-- framing for tls
-------------------------------------------------------------------------------
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

recvT :: I.IORef ByteString -> TLS.Context -> IO ByteString
recvT cref ctx = do
            cached <- I.readIORef cref
            if cached /= "" then do
                I.writeIORef cref ""
                return cached
              else
                recvT' ctx

-- TLS version of recv (decrypting) without a cache.
recvT' :: TLS.Context -> IO ByteString
recvT' ctx =  E.handle onEOF go
    where
      onEOF e
              | Just TLS.Error_EOF <- E.fromException e       = return BS.empty
              | Just ioe <- E.fromException e, isEOFError ioe = return BS.empty
              | otherwise                                   = E.throwIO e
      go = do
                x <- TLS.recvData ctx
                if BS.null x then
                    go
                  else
                    return x

-- TLS version of recvBuf with a cache for leftover input data.
recvBufT' :: TLS.Context -> I.IORef ByteString -> Int -> IO LBS.ByteString
recvBufT' ctx cref siz = do
            cached <- I.readIORef cref
            when(cached /= "") $ I.writeIORef cref ""
            (ret, leftover) <- fill cached siz (recvT cref ctx)
            I.writeIORef cref leftover
            return ret



fill :: BS.ByteString -> Int -> IO BS.ByteString -> IO (LBS.ByteString,BS.ByteString)
fill bs0 siz0 tlsRecv
  | siz0 <= len0 = do
      let (bs, leftover) = BS.splitAt siz0 bs0
      return (LBS.fromStrict bs, leftover)
  | otherwise = do
    loop bs0 (siz0 - len0)
  where
    len0 = BS.length bs0
    loop b  0   = return (LBS.fromStrict b, "")
    loop buf siz = do
      bs <- tlsRecv
      let len = BS.length bs
      if len == 0 then return ("", "")
        else if (len <= siz) then do
          loop (buf `BS.append` bs) (siz - len)
        else do
          let (bs1,bs2) = BS.splitAt siz bs
          return (LBS.fromStrict (buf `BS.append` bs1), bs2)

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
