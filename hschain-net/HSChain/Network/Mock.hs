{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE RecordWildCards    #-}
module HSChain.Network.Mock
  ( MockNet
  , newMockNet
  , createMockNode
  , MockNetError(..)
  ) where

import Control.Concurrent.STM

import Control.Monad          (forM_)
import Control.Monad.Catch
import Control.Monad.IO.Class (liftIO)

import qualified Data.ByteString.Lazy as LBS
import qualified Data.Map.Strict      as Map

import HSChain.Network.Types

newtype MockNetError = MockNetError String
  deriving stock    Show
  deriving anyclass Exception

-- | Sockets for mock network
data MockSocket = MockSocket
  { msckActive :: !(TVar Bool)
  , msckSend   :: !(TChan LBS.ByteString)
  , msckRecv   :: !(TChan LBS.ByteString)
  }
  deriving (Eq)

-- | Mock network which uses STM to deliver messages
newtype MockNet = MockNet
  { mnetIncoming :: TVar (Map.Map NetAddr [(MockSocket, NetAddr)])
    -- ^ Incoming connections for node.
  }


newMockNet :: IO MockNet
newMockNet = MockNet <$> newTVarIO Map.empty


closeMockSocket :: MockSocket -> STM ()
closeMockSocket MockSocket{..} = writeTVar msckActive False


createMockNode
  :: MockNet
  -> NetAddr
  -> NetworkAPI
createMockNode MockNet{..} addr = NetworkAPI
  { listenOn = liftIO $ atomically $ do
      let key = addr
      -- Start listening on port
      do mListen <- readTVar mnetIncoming
         case key `Map.lookup` mListen of
           Just  _ -> throwM $ MockNetError "MockNet: already listening on port"
           Nothing -> writeTVar mnetIncoming $ Map.insert key [] mListen
      -- Stop listening
      --  1. Mark all incoming connections as inactive so that other
      --     side of connection knows it.
      --  2. Remove connection from mnetIncoming so it's possible to
      --     start listening on same address again
      let stopListening = liftIO $ atomically $ do
            mListen <- readTVar mnetIncoming
            forM_ (key `Map.lookup` mListen) $ mapM_ (closeMockSocket . fst)
            modifyTVar' mnetIncoming $ Map.delete key
      -- Accept connection
      let accept = liftIO $ atomically $ do
            mList <- readTVar mnetIncoming
            case key `Map.lookup` mList of
              Nothing     -> throwM $ MockNetError "MockNet: cannot accept on closed socket"
              Just []     -> retry
              Just ((conn,addr'):xs) -> do
                writeTVar mnetIncoming $ Map.insert key xs mList
                return (applyConn conn, addr')
      return (stopListening, accept)
    --
  , connect = \loc -> liftIO $ atomically $ do
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
        Nothing -> throwM $ MockNetError "MockNet: Cannot connect to closed socket"
        Just xs -> writeTVar mnetIncoming $ Map.insert loc (xs ++ [(sockFrom,addr)]) cmap
      return $ applyConn sockTo
  , listenPort = 0
  }
 where
  applyConn conn = P2PConnection
    { send          = liftIO . sendBS conn
    , recv          = liftIO $ recvBS conn
    , close         = liftIO $ atomically $ closeMockSocket conn
    }
  sendBS MockSocket{..} bs = atomically $
      readTVar msckActive >>= \case
        False -> throwM $ MockNetError "MockNet: Cannot write to closed socket"
        True  -> writeTChan msckSend bs
    --
  recvBS MockSocket{..} = atomically $
      readTVar msckActive >>= \case
        False -> tryReadTChan msckRecv >>= \case
          Just m  -> return m
          Nothing -> throwM ConnectionClosed
        True  -> readTChan msckRecv
