{-# LANGUAGE BangPatterns    #-}
{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE RecordWildCards #-}
-- |
-- Abstract API for network which support
module Thundermint.P2P.Network (
    NetworkAPI(..)
  , P2PConnection(..)
    -- * Real network
  , newNetworkTcp
  , newNetworkUdp
    -- * Real tls network
  , newNetworkTls
  , getCredential
  , getCredentialFromBuffer
    -- * Mock in-memory network
  , MockNet
  , newMockNet
  , createMockNode
    -- * Local address detection
  , Ip.getLocalAddress
  , Ip.isLocalAddress
  , Ip.getLocalAddresses
  , PeerInfo(..)
  ) where

import Control.Concurrent.STM

import Control.Monad          (forM_)
import Control.Monad.IO.Class (liftIO)

import qualified Data.ByteString.Lazy           as LBS
import qualified Data.Map.Strict                as Map
import qualified Data.Set                       as Set

import Thundermint.P2P.Network.Internal.TLS
import Thundermint.P2P.Network.Internal.TCP
import Thundermint.P2P.Network.Internal.UDP
import Thundermint.P2P.Types

import qualified Thundermint.P2P.Network.IpAddresses as Ip

----------------------------------------------------------------
-- Some useful utilities
----------------------------------------------------------------

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
  { listenOn = liftIO.atomically $ do
      let key = addr
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
                return (applyConn addr' conn, addr')
      return (stopListening, accept)
    --
  , connect = \loc -> do
    liftIO.atomically $ do
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
        Just xs -> writeTVar mnetIncoming $ Map.insert loc (xs ++ [(sockFrom,addr)]) cmap
      return $ applyConn loc sockTo
  , filterOutOwnAddresses = return . Set.filter ((addr /=))
  , normalizeNodeAddress = const
  , listenPort = 0
  , ourPeerInfo = mkPeerInfoFromAddr addr
  }
 where
  mkPeerInfoFromAddr (NetAddrV4 ha _) = PeerInfo (PeerId (fromIntegral ha)) 0 0
  mkPeerInfoFromAddr _ = error "IPv6 addr in mkPeerInfoFromAddr"
  applyConn otherAddr conn = P2PConnection (liftIO . sendBS conn) (liftIO $ recvBS conn) (liftIO $ close conn) (mkPeerInfoFromAddr otherAddr)
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
