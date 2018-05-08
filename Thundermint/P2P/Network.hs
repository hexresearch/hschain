{-# LANGUAGE RecordWildCards #-}
-- |
-- Abstract API for network which support
module Thundermint.P2P.Network (
    NetworkAPI(..)
  , SendRecv(..)
  , applySocket
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
data NetworkAPI sock addr = NetworkAPI
  { listenOn :: Net.ServiceName -> IO (IO (), IO (sock, addr))
    -- ^ Start listening on given port. Returns action to close socket
    --   and function for accepting new connections
  , connect  :: addr -> IO sock
    -- ^ Connect to remote address
  , sendBS   :: sock -> BS.ByteString -> IO ()
    -- ^ Send data to socket
  , recvBS   :: sock -> Int -> IO (Maybe BS.ByteString)
    -- ^ Receive data from socket
    --
    -- FIXME: Do we even need size parameter?
  , close    :: sock -> IO ()
  }

data SendRecv = SendRecv
  { send :: BS.ByteString -> IO ()
  , recv :: Int -> IO (Maybe BS.ByteString)
  }

applySocket :: NetworkAPI s a -> s -> SendRecv
applySocket NetworkAPI{..} s = SendRecv
  { send = sendBS s
  , recv = recvBS s
  }

----------------------------------------------------------------
--
----------------------------------------------------------------

-- | API implementation for real network
realNetwork :: NetworkAPI Net.Socket Net.SockAddr
realNetwork = NetworkAPI
  { listenOn = \port -> do
      let hints = Net.defaultHints
            { Net.addrFlags      = [Net.AI_PASSIVE]
            , Net.addrSocketType = Net.Stream
            }
      addr:_ <- Net.getAddrInfo (Just hints) Nothing (Just port)
      sock   <- Net.socket (Net.addrFamily     addr)
                           (Net.addrSocketType addr)
                           (Net.addrProtocol   addr)
      flip onException (Net.close sock) $ do
        Net.bind sock (Net.addrAddress addr)
        return (Net.close sock, Net.accept sock)
    --
  , connect  = \addr -> do
      -- FIXME: we need to support both IP4 & IP6
      -- FIXME: exception safety
      sock <- case addr of
        Net.SockAddrInet port _ -> Net.socket Net.AF_INET Net.Stream (fromIntegral port)
        _                       -> error "Unsupported address"
      flip onException (Net.close sock) $ do
        Net.connect sock addr
        return sock
    --
  , sendBS = NetBS.sendAll
  , recvBS = \sock n -> do
      bs <- NetBS.recv sock (fromIntegral n)
      return $ if BS.null bs then Nothing else Just bs
  , close  = Net.close
  }


----------------------------------------------------------------
-- Mock network
----------------------------------------------------------------

-- | Sockets for mock network
newtype MockSocket = MockSocket Int
  deriving (Show,Eq,Ord)

-- | Mock network which uses STM to deliver messages
data MockNet addr = MockNet
  { mnetSockCounter :: TVar Int
    -- ^ Counter for sockets. We use unique numbers for numbering
    --   sockets
  , mnetConnections :: TVar (Map MockSocket (TChan BS.ByteString))
    -- ^ Channels for transmitting messages between nodes.
  , mnetIncoming    :: TVar (Map (addr,Net.ServiceName)
                                [(MockSocket, (addr,Net.ServiceName))])
    -- ^ Incoming connections for node.
  }


newMockNet :: IO (MockNet addr)
newMockNet = MockNet <$> newTVarIO 0 <*> newTVarIO Map.empty <*> newTVarIO Map.empty

createMockNode
  :: Ord addr
  => MockNet addr
  -> addr
  -> NetworkAPI MockSocket (addr, Net.ServiceName)
createMockNode MockNet{..} addr = NetworkAPI
  { listenOn = \port -> atomically $ do
      let key = (addr, port)
      mListen <- readTVar mnetIncoming
      case key `Map.lookup` mListen of
        Just  _ -> error "MockNet: already listening on port"
        Nothing -> writeTVar mnetIncoming $ Map.insert key [] mListen
      -- Close & accept
      let close  = atomically $ modifyTVar' mnetIncoming $ Map.delete key
          accept = atomically $ do
            mList <- readTVar mnetIncoming
            case key `Map.lookup` mList of
              Nothing     -> error "MockNet: cannot accept on closed socket"
              Just []     -> retry
              Just (x:xs) -> do writeTVar mnetIncoming $ Map.insert key xs mList
                                return x
      return (close, accept)
    --
  , connect = \loc -> atomically $ do
      s  <- do n <- readTVar mnetSockCounter
               writeTVar mnetSockCounter $! n + 1
               return (MockSocket n)
      -- Create connection
      ch   <- newTChan
      modifyTVar' mnetConnections $ Map.insert s ch
      -- Queue connection
      cmap <- readTVar mnetIncoming
      case loc `Map.lookup` cmap of
        Nothing -> error "MockNet: Cannot connect to closed socket"
        Just xs -> writeTVar mnetIncoming $ Map.insert loc (xs ++ [(s,loc)]) cmap
      return s
    --
  , sendBS = \s bs -> atomically $ do
      cmap <- readTVar mnetConnections
      case s `Map.lookup` cmap of
        Nothing -> error "MockNet: Sending to closed socket!"
        Just ch -> writeTChan ch bs
    --
  , recvBS = \s _n -> atomically $ do
      cmap <- readTVar mnetConnections
      case s `Map.lookup` cmap of
        Nothing -> return Nothing
        Just ch -> Just <$> readTChan ch
    --
  , close = \s -> atomically $ do
      modifyTVar' mnetConnections $ Map.delete s
  }
