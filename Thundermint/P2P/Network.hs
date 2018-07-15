{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
-- |
-- Abstract API for network which support
module Thundermint.P2P.Network (
    NetworkAPI(..)
  , Connection(..)
    -- * Real network
  , realNetwork
    -- * Mock in-memory network
  , MockSocket
  , MockNet
  , newMockNet
  , createMockNode
  ) where

import Control.Exception
import Control.Concurrent.STM
import qualified Data.Map        as Map
import           Data.Map          (Map)
import qualified Data.ByteString.Lazy       as BS
import qualified Network.Socket             as Net
import qualified Network.Socket.ByteString.Lazy as NetBS

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
    { send  :: BS.ByteString -> IO ()
      -- ^ Send data
    , recv  :: IO (Maybe BS.ByteString)
      -- ^ Receive data
    , close :: IO ()
      -- ^ Close socket
    }

----------------------------------------------------------------
--
----------------------------------------------------------------

-- | API implementation for real network
realNetwork :: Net.ServiceName -> NetworkAPI Net.SockAddr
realNetwork listenPort = NetworkAPI
  { listenOn = do
      let hints = Net.defaultHints
            { Net.addrFlags      = [Net.AI_PASSIVE]
            , Net.addrSocketType = Net.Stream
            }
      addr:_ <- Net.getAddrInfo (Just hints) Nothing (Just listenPort)
      sock   <- Net.socket (Net.addrFamily     addr)
                           (Net.addrSocketType addr)
                           (Net.addrProtocol   addr)
      flip onException (Net.close sock) $ do
        Net.bind sock (Net.addrAddress addr)
        Net.listen sock 5
        return (Net.close sock, accept sock)
    --
  , connect  = \addr -> do
      let hints = Just Net.defaultHints
            { Net.addrSocketType = Net.Stream
            }
      (hostName, serviceName) <- Net.getNameInfo [] True True addr
      addrInfo:_ <- Net.getAddrInfo hints hostName serviceName
      sock <- Net.socket (Net.addrFamily     addrInfo)
                         (Net.addrSocketType addrInfo)
                         (Net.addrProtocol   addrInfo)
      flip onException (Net.close sock) $ do
        Net.connect sock addr
        return $ applyConn sock
  }
 where
  accept sock = do
    (conn, addr) <- Net.accept sock
    return (applyConn conn, addr)
  applyConn conn = Connection (sendBS conn) (recvBS conn) (Net.close conn)
  sendBS = NetBS.sendAll
  recvBS sock = do
      -- FIXME: packet length should be added before message 2 octets in network order BE
      bs <- NetBS.recv sock 4096
      return $ if BS.null bs then Nothing else Just bs




----------------------------------------------------------------
-- Mock network
----------------------------------------------------------------

-- | Sockets for mock network
data MockSocket = MockSocket
  { msckActive :: TVar Bool
  , msckSend   :: TChan BS.ByteString
  , msckRecv   :: TChan BS.ByteString
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
