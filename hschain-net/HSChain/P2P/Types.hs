{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
-- |
-- Abstract API for network which support
module HSChain.P2P.Types (
    NetworkAPI(..)
  , NetAddr(..)
  , sockAddrToNetAddr
  , netAddrToSockAddr
  , P2PConnection(..)
  , NetworkError(..)
  , NetworkPort
    -- *
  , tcpHints
  , tcpListenHints
  , udpHints
  , netAddrToAddrInfo
  ) where

import Control.Exception      (throwIO,Exception)
import Control.Monad.Catch    (MonadMask, MonadThrow)
import Control.Monad.IO.Class (MonadIO(..))
import qualified Data.ByteString.Lazy as LBS
import qualified Network.Socket       as Net

import HSChain.Network.Types


----------------------------------------------------------------
--
----------------------------------------------------------------

sockAddrToNetAddr :: Net.SockAddr -> NetAddr
sockAddrToNetAddr sa = case sa of
  Net.SockAddrInet  port ha     -> NetAddrV4 ha $ fromIntegral port
  Net.SockAddrInet6 port _ ha _ -> NetAddrV6 ha $ fromIntegral port
  _                             -> error $ "unsupported socket address kind: "++show sa

netAddrToSockAddr :: NetAddr -> Net.SockAddr
netAddrToSockAddr (NetAddrV4 ha port) = Net.SockAddrInet  (fromIntegral port)  ha
netAddrToSockAddr (NetAddrV6 ha port) = Net.SockAddrInet6 (fromIntegral port) 0 ha 0

-- | Network port
type NetworkPort = Net.PortNumber

-- | Dictionary with API for network programming. We use it to be able
--   to provide two implementations of networking. One is real network
--   and another is mock in-process network for testing.
data NetworkAPI = NetworkAPI
  { listenOn :: !(forall m. (MonadIO m, MonadMask m)
             => m (m (), m (P2PConnection, NetAddr)))
    -- ^ Start listening on given port. Returns action to stop listener
    --   and function for accepting new connections
  , connect  :: !(forall m. (MonadIO m, MonadThrow m, MonadMask m)
             => NetAddr -> m P2PConnection)
    -- ^ Connect to remote address
  , listenPort :: !NetworkPort
    -- ^ Listen port.
  }

data P2PConnection = P2PConnection
  { send          :: !(forall m. (MonadIO m) => LBS.ByteString -> m ())
    -- ^ Send data
  , recv          :: !(forall m. (MonadIO m) => m LBS.ByteString)
    -- ^ Receive data. Will throw exception if connection is closed
  , close         :: !(forall m. (MonadIO m) => m ())
  }



----------------------------------------------------------------
-- Hints
----------------------------------------------------------------

tcpHints, tcpListenHints, udpHints :: Net.AddrInfo
tcpHints       = Net.defaultHints
  { Net.addrSocketType = Net.Stream
  }
tcpListenHints = Net.defaultHints
  { Net.addrFlags      = [Net.AI_PASSIVE]
  , Net.addrSocketType = Net.Stream
  }
udpHints       = Net.defaultHints
  { Net.addrFlags      = []
  , Net.addrSocketType = Net.Datagram
  }

-- | Convert 'NetAddr to 'Net.AddrInfo' for creating socket to connect
--   to given address
netAddrToAddrInfo
  :: MonadIO m
  => NetAddr -> m (Net.AddrInfo, Net.SockAddr, Maybe Net.HostName)
netAddrToAddrInfo addr = liftIO $ do
  (hostName, serviceName) <- Net.getNameInfo
    [Net.NI_NUMERICHOST, Net.NI_NUMERICSERV] True True sockAddr
  ai <- Net.getAddrInfo (Just tcpHints) hostName serviceName >>= \case
    a:_ -> return a
    []  -> throwIO NoAddressAvailable
  return (ai,sockAddr,hostName)
  where
    sockAddr = netAddrToSockAddr addr

-- | Some network error.
data NetworkError
  = ConnectionTimedOut          -- ^ Connection timed out
  | NoAddressAvailable          -- ^ 
  | CantReverseLookipHostname   -- ^
  | ConnectionClosed            -- ^
  | ConnectionLoop
  | SelfConnection
  deriving stock    (Show)
  deriving anyclass (Exception)
