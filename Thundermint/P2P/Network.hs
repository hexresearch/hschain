{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE RecordWildCards #-}
-- |
-- Abstract API for network which support
module Thundermint.P2P.Network (
    NetworkAPI(..)
  , Connection(..)
  , RealNetworkConnectOptions(..)
    -- * Real network
  , realNetwork
  , realNetworkUdp
    -- * Real tls network
  , realNetworkTls
  , getCredential
  , getCredentialFromBuffer
    -- * Mock in-memory network
  , MockSocket
  , MockNet
  , newMockNet
  , createMockNode
    -- * Local address detection
  , getLocalAddress
  , isLocalAddress
  , getLocalAddresses
  ) where

import Control.Concurrent.STM

import Control.Concurrent     (forkIO, killThread)
import Control.Exception      (Exception)
import Control.Monad          (filterM, forM_, forever, void, when)
import Control.Monad.Catch    (MonadMask, MonadThrow, bracketOnError, onException, throwM)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Bits              (unsafeShiftL)
import Data.List              (find, intercalate)
import Data.Map               (Map)
import Data.Set               (Set)
import Data.Maybe             (fromMaybe)
import Data.Monoid            ((<>))
import Data.Word              (Word32)
import System.Timeout         (timeout)

import qualified Data.ByteString.Builder        as BB
import qualified Data.ByteString.Lazy           as LBS
import qualified Data.Map                       as Map
import qualified Data.Set                       as Set
import qualified Network.Info                   as Net
import qualified Network.Socket                 as Net
import qualified Network.Socket.ByteString      as NetBS
import qualified Network.Socket.ByteString.Lazy as NetLBS

import Thundermint.Control
import Thundermint.P2P.Consts
import Thundermint.P2P.Network.Local
import Thundermint.P2P.Network.TLS
import Thundermint.P2P.Types


-- | API implementation for real tcp network
realNetwork :: RealNetworkConnectOptions -> Net.ServiceName -> NetworkAPI Net.SockAddr
realNetwork RealNetworkConnectOptions{..} listenPort = NetworkAPI
  { listenOn = do
      let hints = Net.defaultHints
            { Net.addrFlags      = [Net.AI_PASSIVE]
            , Net.addrSocketType = Net.Stream
            }
      addrs <- liftIO $ Net.getAddrInfo (Just hints) Nothing (Just listenPort)

      when (null addrs) $
        throwM NoAddressAvailable
      let addr = fromMaybe (head addrs) $ find isIPv6addr addrs

      bracketOnError (liftIO $ newSocket addr) (liftIO . Net.close) $ \sock -> liftIO $ do
        when (isIPv6addr addr) $
          Net.setSocketOption sock Net.IPv6Only 0
        Net.bind sock (Net.addrAddress addr)
        Net.listen sock 5
        return (liftIO $ Net.close sock, accept sock)
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
        return $ applyConn sock
  , filterOutOwnAddresses = -- TODO: make it batch processing for speed!
        \addrs -> (fmap Set.fromList) $ filterM (fmap not . isLocalAddress) $ Set.toList addrs
  , normalizeNodeAddress = \case
        Net.SockAddrInet _ ha        -> Net.SockAddrInet  thundermintPort ha
        Net.SockAddrInet6 _ fi ha si -> Net.SockAddrInet6 thundermintPort fi ha si
        s -> s
  }
 where
  isIPv6addr = (==) Net.AF_INET6 . Net.addrFamily
  accept sock = do
    (conn, addr) <- liftIO $ Net.accept sock
    if allowConnectFromLocal then
        return (applyConn conn, addr)
    else do
        isLocal <- isLocalAddress addr
        -- liftIO $ putStrLn $ showSockAddr addr <> "  -> " <> show isLocal
        if isLocal then do
            liftIO $ Net.close conn
            accept sock
        else do
            return (applyConn conn, addr)
  applyConn conn = Connection (liftIO . sendBS conn) (liftIO $ recvBS conn) (liftIO $ Net.close conn)
  sendBS sock =  \s -> NetLBS.sendAll sock (BB.toLazyByteString $ toFrame s)
                 where
                   toFrame msg = let len =  fromIntegral (LBS.length msg) :: Word32
                                     hexLen = BB.word32BE len
                                 in (hexLen <> BB.lazyByteString msg)

  recvBS sock = do
    header <- recvAll sock headerSize
    if LBS.null header
    then return Nothing
    else let len = decodeWord16BE header
         in case len of
              Just n  -> Just <$> recvAll sock (fromIntegral n)
              Nothing -> return Nothing


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


emptyBs2Maybe :: LBS.ByteString -> Maybe LBS.ByteString
emptyBs2Maybe bs
  | LBS.null bs = Nothing
  | otherwise  = Just bs

-- | API implementation example for real udp network
realNetworkUdp :: Net.ServiceName -> IO (NetworkAPI Net.SockAddr)
realNetworkUdp listenPort = do
  -- FIXME: prolly HostName fits better than SockAddr
  tChans <- newTVarIO Map.empty
  acceptChan <- newTChanIO :: IO (TChan (Connection, Net.SockAddr))
  let hints = Net.defaultHints
        { Net.addrFlags      = [Net.AI_PASSIVE]
        , Net.addrSocketType = Net.Datagram
        }
  addrInfo:_ <- Net.getAddrInfo (Just hints) Nothing (Just listenPort)
  sock       <- newSocket addrInfo
  tid <- forkIO $
    flip onException (Net.close sock) $ do
      Net.bind sock (Net.addrAddress addrInfo)
      forever $ do
        (bs, addr) <- NetBS.recvFrom sock 4096
        recvChan <- findOrCreateRecvChan tChans addr
        atomically $ writeTChan acceptChan (applyConn sock addr recvChan, addr)
        atomically $ writeTChan recvChan $ LBS.fromStrict bs

  return NetworkAPI
    { listenOn =
        return (liftIO $ killThread tid, liftIO.atomically $ readTChan acceptChan)
      --
    , connect  = \addr ->
         applyConn sock addr <$> findOrCreateRecvChan tChans addr
    , filterOutOwnAddresses = -- TODO: make it batch processing for speed!
        \addrs -> (fmap Set.fromList) $ filterM (fmap not . isLocalAddress) $ Set.toList addrs
    , normalizeNodeAddress = \case -- TODO remove duplication
            Net.SockAddrInet _ ha -> Net.SockAddrInet thundermintPort ha
            Net.SockAddrInet6 _ fi ha si -> Net.SockAddrInet6 thundermintPort fi ha si
            s -> s
    }
 where
  findOrCreateRecvChan tChans addr = liftIO.atomically $ do
    chans <- readTVar tChans
    case Map.lookup addr chans of
      Just chan -> return chan
      Nothing   -> do
        recvChan <- newTChan
        writeTVar tChans $ Map.insert addr recvChan chans
        return recvChan
  applyConn sock addr peerChan = Connection
    (\s -> liftIO.void $ NetBS.sendAllTo sock (LBS.toStrict s) addr)
    (emptyBs2Maybe <$> liftIO (atomically $ readTChan peerChan))
    (return ())


----------------------------------------------------------------
-- Some useful utilities
----------------------------------------------------------------

showSockAddr :: Net.SockAddr -> String
showSockAddr s@(Net.SockAddrInet pn ha) =
    unwords ["SockAddrInet", show pn, show ha, "(" <> show s <> ")"]
showSockAddr s@(Net.SockAddrInet6 pn fi ha si) =
    unwords ["SockAddrInet6 ", show pn, show fi, show ha, show si, "(" <> show s <> ")"]
showSockAddr s = "?? (" <> show s <> ")"


newMockNet :: IO (MockNet addr)
newMockNet = MockNet <$> newTVarIO Map.empty


closeMockSocket :: MockSocket -> STM ()
closeMockSocket MockSocket{..} = writeTVar msckActive False


createMockNode
  :: Ord addr
  => MockNet addr
  -> Net.ServiceName
  -> addr
  -> NetworkAPI (addr, Net.ServiceName)
createMockNode MockNet{..} port addr = NetworkAPI
  { listenOn = liftIO.atomically $ do
      let key = (addr, port)
      -- Start listening on port
      do mListen <- readTVar mnetIncoming
         case key `Map.lookup` mListen of
           Just  _ -> error "MockNet: already listening on port"
           Nothing -> writeTVar mnetIncoming $ Map.insert key [] mListen
      -- Stop listening and close all accepted sockets
      let stopListening = liftIO.atomically $ do
            mListen <- readTVar mnetIncoming
            forM_ (key `Map.lookup` mListen) $
              mapM_ (closeMockSocket . fst)
      -- Accept connection
      let accept = liftIO.atomically $ do
            mList <- readTVar mnetIncoming
            case key `Map.lookup` mList of
              Nothing     -> error "MockNet: cannot accept on closed socket"
              Just []     -> retry
              Just ((conn,addr'):xs) -> do
                writeTVar mnetIncoming $ Map.insert key xs mList
                return (applyConn conn, addr')
      return (stopListening, accept)
    --
  , connect = \loc -> liftIO.atomically $ do
      chA <- newTChan
      chB <- newTChan
      v   <- newTVar True
      let sockTo   = MockSocket { msckActive = v
                                , msckRecv   = chA
                                , msckSend   = chB
                                }
      let sockFrom = MockSocket { msckActive = v
                                , msckRecv   = chB
                                , msckSend   = chA
                                }
      -- Queue connection on server
      cmap <- readTVar mnetIncoming
      case loc `Map.lookup` cmap of
        Nothing -> error "MockNet: Cannot connect to closed socket"
        Just xs -> writeTVar mnetIncoming $ Map.insert loc (xs ++ [(sockFrom,(addr,snd loc))]) cmap
      return $ applyConn sockTo
  , filterOutOwnAddresses = return . Set.filter ((addr /=) . fst)
  , normalizeNodeAddress = id
  }
 where
  applyConn conn = Connection (liftIO . sendBS conn) (liftIO $ recvBS conn) (liftIO $ close conn)
  sendBS MockSocket{..} bs = atomically $
      readTVar msckActive >>= \case
        False -> error "MockNet: Cannot write to closed socket"
        True  -> writeTChan msckSend bs
    --
  recvBS MockSocket{..} = atomically $
      readTVar msckActive >>= \case
        False -> tryReadTChan msckRecv
        True  -> Just <$> readTChan msckRecv
    --
  close = atomically . closeMockSocket
