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
  , PeerId(..)
  , PeerInfo(..)
  , defPeerInfo
    -- *
  , tcpHints
  , tcpListenHints
  , udpHints
  , netAddrToAddrInfo
  ) where

import Codec.Serialise
import Control.Exception      (throwIO)
import Control.Monad.Catch    (MonadMask, MonadThrow)
import Control.Monad.IO.Class (MonadIO(..))
import Data.Word              (Word16, Word64)
import GHC.Generics           (Generic)
import GHC.Read               (Read(..))

import qualified Data.Aeson           as JSON
import qualified Data.ByteString.Lazy as LBS
import qualified Network.Socket       as Net

import HSChain.Exceptions
import HSChain.Types.Network


----------------------------------------------------------------
--
----------------------------------------------------------------

newtype PeerId = PeerId Word64
  deriving newtype (Show, Read, Eq, Ord, Serialise, JSON.ToJSON, JSON.FromJSON)


data PeerInfo = PeerInfo
    { piPeerId        :: !PeerId
    -- ^ An ID to identify the machine
    , piPeerPort      :: !Word16
    -- ^ Original listening port of the machine of the peer.
    , piPeerSchemeVer :: !Word16
    -- ^ The scheme encoding version. It is not possible tp decode
    --   values safely between two different versions.
    }
    deriving stock    (Show, Generic)
    deriving anyclass (Serialise)

defPeerInfo :: PeerInfo
defPeerInfo = PeerInfo (PeerId 0) 0 0

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
  , normalizeNodeAddress :: !(NetAddr -> Maybe NetworkPort -> NetAddr)
    -- ^ Normalize address, for example, convert '20.15.10.20:24431' to '20.15.10.20:50000'
  , listenPort :: !NetworkPort
    -- ^ Listen port.
  , ourPeerInfo :: !PeerInfo
    -- ^ peer info for easy reference.
  }

data P2PConnection = P2PConnection
  { send          :: !(forall m. (MonadIO m) => LBS.ByteString -> m ())
    -- ^ Send data
  , recv          :: !(forall m. (MonadIO m) => m (Maybe LBS.ByteString))
    -- ^ Receive data
  , close         :: !(forall m. (MonadIO m) => m ())
    -- ^ Close socket
  , connectedPeer :: !PeerInfo
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
