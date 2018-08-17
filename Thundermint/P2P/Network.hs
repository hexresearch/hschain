{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}
-- |
-- Abstract API for network which support
module Thundermint.P2P.Network (
    NetworkAPI(..)
  , Connection(..)
    -- * Real network
  , realNetwork
  , realNetworkUdp
    -- * Mock in-memory network
  , MockSocket
  , MockNet
  , newMockNet
  , createMockNode
  ) where

import Control.Concurrent.STM
import Control.Exception

import Control.Concurrent (forkIO, killThread)
import Control.Monad      (forever, void, when)
import Data.Bits          (unsafeShiftL)
import Data.Map           (Map)
import Data.Monoid        ((<>))
import Data.Word          (Word32)
import Data.List          (find)
import Data.Maybe (fromMaybe)

import qualified Data.ByteString.Builder        as BB
import qualified Data.ByteString.Lazy           as LBS
import qualified Data.Map                       as Map
import qualified Network.Socket                 as Net
import qualified Network.Socket.ByteString      as NetBS
import qualified Network.Socket.ByteString.Lazy as NetLBS
----------------------------------------------------------------
--
----------------------------------------------------------------

-- | Dictionary with API for network programming. We use it to be able
--   to provide two implementations of networking. One is real network
--   and another is mock in-process network for testing.
data NetworkAPI addr = NetworkAPI
  { listenOn :: IO (IO (), IO (Connection , addr))
    -- ^ Start listening on given port. Returns action to stop listener
    --   and function for accepting new connections
  , connect  :: addr -> IO Connection
    -- ^ Connect to remote address
  }

data Connection = Connection
    { send  :: LBS.ByteString -> IO ()
      -- ^ Send data
    , recv  :: IO (Maybe LBS.ByteString)
      -- ^ Receive data
    , close :: IO ()
      -- ^ Close socket
    }

type HeaderSize = Int

headerSize :: HeaderSize
headerSize = 4

----------------------------------------------------------------
--
----------------------------------------------------------------

newSocket :: Net.AddrInfo -> IO Net.Socket
newSocket ai = Net.socket (Net.addrFamily     ai)
                          (Net.addrSocketType ai)
                          (Net.addrProtocol   ai)

-- | API implementation for real tcp network
realNetwork :: Net.ServiceName -> NetworkAPI Net.SockAddr
realNetwork listenPort = NetworkAPI
  { listenOn = do
      let hints = Net.defaultHints
            { Net.addrFlags      = [Net.AI_PASSIVE]
            , Net.addrSocketType = Net.Stream
            }
      addrs <- Net.getAddrInfo (Just hints) Nothing (Just listenPort)

      when (null addrs) $
        fail "listenOn: No address availible"
      let addr = fromMaybe (head addrs) $ find isIPv6addr addrs

      bracketOnError (newSocket addr) Net.close $ \sock -> do
        when (isIPv6addr addr) $
          Net.setSocketOption sock Net.IPv6Only 0
        Net.bind sock (Net.addrAddress addr)
        Net.listen sock 5
        return (Net.close sock, accept sock)
  , connect  = \addr -> do
      let hints = Just Net.defaultHints
            { Net.addrSocketType = Net.Stream
            }
      (hostName, serviceName) <- Net.getNameInfo [] True True addr
      addrInfo:_ <- Net.getAddrInfo hints hostName serviceName
      bracketOnError (newSocket addrInfo) Net.close $ \ sock -> do
        Net.connect sock addr
        return $ applyConn sock
  }
 where
  isIPv6addr = (==) Net.AF_INET6 . Net.addrFamily
  accept sock = do
    (conn, addr) <- Net.accept sock
    return (applyConn conn, addr)
  applyConn conn = Connection (sendBS conn) (recvBS conn) (Net.close conn)
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
decodeWord16BE bs | LBS.length bs < (fromIntegral headerSize) = Nothing
                  | otherwise =
                      do
                        let w8s = LBS.unpack $ LBS.take (fromIntegral headerSize) bs
                            word32 = foldr (\b (acc, i) ->
                                          let shiftBy = i * 8
                                          in ((fromIntegral b `unsafeShiftL` shiftBy) + acc, i + 1))
                                     (0,0)
                                     w8s
                        (Just $ fst word32)


-- | helper function read given lenght of bytes
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
        return (killThread tid, atomically $ readTChan acceptChan)
      --
    , connect  = \addr -> do
        peerChan <- findOrCreateRecvChan tChans addr
        return $ applyConn sock addr peerChan
    }
 where
  findOrCreateRecvChan tChans addr = atomically $ do
    chans <- readTVar tChans
    case Map.lookup addr chans of
      Just chan -> return chan
      Nothing   -> do
        recvChan <- newTChan
        writeTVar tChans $ Map.insert addr recvChan chans
        return recvChan
  applyConn sock addr peerChan = Connection
    (\s -> void $ NetBS.sendAllTo sock (LBS.toStrict s) addr)
    (emptyBs2Maybe <$> atomically (readTChan peerChan))
    (return ())




----------------------------------------------------------------
-- Mock network
----------------------------------------------------------------

-- | Sockets for mock network
data MockSocket = MockSocket
  { msckActive :: TVar Bool
  , msckSend   :: TChan LBS.ByteString
  , msckRecv   :: TChan LBS.ByteString
  }
  deriving (Eq)

-- | Mock network which uses STM to deliver messages
newtype MockNet addr = MockNet
  { mnetIncoming    :: TVar (Map (addr,Net.ServiceName)
                                 [(MockSocket, (addr,Net.ServiceName))])
    -- ^ Incoming connections for node.
  }



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
  { listenOn = atomically $ do
      let key = (addr, port)
      -- Start listening on port
      do mListen <- readTVar mnetIncoming
         case key `Map.lookup` mListen of
           Just  _ -> error "MockNet: already listening on port"
           Nothing -> writeTVar mnetIncoming $ Map.insert key [] mListen
      -- Stop listening and close all accepted sockets
      let stopListening = atomically $ do
            mListen <- readTVar mnetIncoming
            case key `Map.lookup` mListen of
              Nothing -> return ()
              Just ss -> mapM_ (closeMockSocket . fst) ss
      -- Accept connection
      let accept = atomically $ do
            mList <- readTVar mnetIncoming
            case key `Map.lookup` mList of
              Nothing     -> error "MockNet: cannot accept on closed socket"
              Just []     -> retry
              Just ((conn,addr'):xs) -> do
                writeTVar mnetIncoming $ Map.insert key xs mList
                return (applyConn conn, addr')
      return (stopListening, accept)
    --
  , connect = \loc -> atomically $ do
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
  }
 where
  applyConn conn = Connection (sendBS conn) (recvBS conn) (close conn)
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
