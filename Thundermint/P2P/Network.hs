{-# LANGUAGE LambdaCase #-}
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
  -> addr
  -> NetworkAPI MockSocket (addr, Net.ServiceName)
createMockNode MockNet{..} addr = NetworkAPI
  { listenOn = \port -> atomically $ do
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
              Just (x:xs) -> do writeTVar mnetIncoming $ Map.insert key xs mList
                                return x
      return (stopListening, accept)
    --
  , connect = \loc -> atomically $ do
      -- let newSocket = do n <- readTVar mnetSockCounter
      --                    writeTVar mnetSockCounter $! n + 1
      --                    return (MockSocket n)
      -- -- Names are relative to server
      -- sockTo   <- newSocket
      -- sockFrom <- newSocket
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
        Just xs -> writeTVar mnetIncoming $ Map.insert loc (xs ++ [(sockFrom,loc)]) cmap
      return sockTo
    --
  , sendBS = \MockSocket{..} bs -> atomically $
      readTVar msckActive >>= \case
        False -> error "MockNet: Cannot write to closed socket"
        True  -> writeTChan msckSend bs
    --
  , recvBS = \MockSocket{..} _n -> atomically $ do
      readTVar msckActive >>= \case
        False -> tryReadTChan msckRecv
        True  -> Just <$> readTChan msckRecv
    --
  , close = atomically . closeMockSocket

  }
