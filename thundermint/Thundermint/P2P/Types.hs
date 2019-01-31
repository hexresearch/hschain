{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RankNTypes #-}

-- |
-- Abstract API for network which support
module Thundermint.P2P.Types (
    NetworkAPI(..)
  , NetAddr(..)
  , sockAddrToNetAddr
  , netAddrToSockAddr
  , P2PConnection(..)
  , NetworkError(..)
  , MockSocket(..)
  , MockNet(..)
  , HeaderSize
  , RecvFun
  , NetworkPort
  , PeerId
  , PeerInfo(..)
  ) where

import Codec.Serialise
import Control.Applicative
import Control.Concurrent.STM
import Control.Exception        (Exception)
import Control.Monad.Catch      (MonadMask, MonadThrow)
import Control.Monad.IO.Class   (MonadIO)
import qualified Data.Aeson as JSON
import Data.ByteString.Internal (ByteString(..))
import qualified Data.List as List
import Data.Map                 (Map)
import Data.Set                 (Set)
import Data.Word
import GHC.Generics             (Generic)


import qualified Data.ByteString.Lazy as LBS
import qualified Network.Socket       as Net

import qualified Thundermint.Utils.Parser as Parse

----------------------------------------------------------------
--
----------------------------------------------------------------

type PeerId = Word64


data PeerInfo = PeerInfo
    { piPeerId        :: !PeerId -- ^An ID to identify the machine
    , piPeerPort      :: !Word16 -- ^Original listening port of the machine of the peer.
    , piPeerSchemeVer :: !Word16 -- ^The scheme encoding version. It is not possible tp decode values safely between two different versions.
    } deriving (Show, Generic)


instance Serialise PeerInfo


----------------------------------------------------------------
--
----------------------------------------------------------------

data NetAddr = NetAddrV4 !Net.HostAddress  !Word16
             | NetAddrV6 !Net.HostAddress6 !Word16
          deriving (Eq, Ord, Generic)

instance Show NetAddr where
  show (NetAddrV4 ha p) = let (a,b,c,d) = Net.hostAddressToTuple ha
                       in ((++show p) . (++":")) $ List.intercalate "." $ map show [a,b,c,d]
  show (NetAddrV6 ha p) = let (a,b,c,d,e,f,g,h) = Net.hostAddress6ToTuple ha
                       in ((++show p) . (++".")) $ List.intercalate ":" $ map show [a,b,c,d,e,f,g,h]

instance Read NetAddr where
  readsPrec _ = Parse.parse (readV4 <|> readV6)
    where
      readV4 = NetAddrV4 <$>
          (Net.tupleToHostAddress <$>
            (Parse.pcheck getTuple4 $ Parse.psepby1 (Parse.pitem '.') Parse.pnum)) <* Parse.pstring ":" <*> Parse.pnum
      readV6 = NetAddrV6 <$>
          (Net.tupleToHostAddress6 <$>
            (Parse.pcheck getTuple6 $ Parse.psepby1 (Parse.pitem ':') Parse.pnum)) <* Parse.pstring "." <*> Parse.pnum
      getTuple4 [a,b,c,d] = [(a,b,c,d)]
      getTuple4 _         = []
      getTuple6 [a,b,c,d,e,f,g,h] = [(a,b,c,d,e,f,g,h)]
      getTuple6 _                 = []

sockAddrToNetAddr :: Net.SockAddr -> NetAddr
sockAddrToNetAddr sa = case sa of
  Net.SockAddrInet  port ha     -> NetAddrV4 ha $ fromIntegral port
  Net.SockAddrInet6 port _ ha _ -> NetAddrV6 ha $ fromIntegral port
  _                             -> error $ "unsupported socket address kind: "++show sa

netAddrToSockAddr :: NetAddr -> Net.SockAddr
netAddrToSockAddr (NetAddrV4 ha port) = Net.SockAddrInet  (fromIntegral port)  ha
netAddrToSockAddr (NetAddrV6 ha port) = Net.SockAddrInet6 (fromIntegral port) 0 ha 0

instance Serialise NetAddr
instance JSON.ToJSON   NetAddr
instance JSON.FromJSON NetAddr

-- | Network port
type NetworkPort = Net.PortNumber

-- | Dictionary with API for network programming. We use it to be able
--   to provide two implementations of networking. One is real network
--   and another is mock in-process network for testing.
data NetworkAPI = NetworkAPI
  { listenOn :: !(forall m. (MonadIO m, MonadThrow m, MonadMask m)
             => m (m (), m (P2PConnection, NetAddr)))
    -- ^ Start listening on given port. Returns action to stop listener
    --   and function for accepting new connections
  , connect  :: !(forall m. (MonadIO m, MonadThrow m, MonadMask m)
             => NetAddr -> m P2PConnection)
    -- ^ Connect to remote address
  , filterOutOwnAddresses :: !(forall m. (MonadIO m, MonadThrow m, MonadMask m)
             => Set NetAddr -> m (Set NetAddr))
    -- ^ Filter out local addresses of node. Batch processing for speed.
  , normalizeNodeAddress :: !(NetAddr -> Maybe NetworkPort -> NetAddr)
    -- ^ Normalize address, for example, convert '20.15.10.20:24431' to '20.15.10.20:50000'
  , listenPort :: !NetworkPort 
    -- ^ Listen port.
  , ourPeerInfo :: !PeerInfo
    -- ^ peer info for easy reference.
  }

data P2PConnection = P2PConnection
  { send  :: !(forall m. (MonadIO m) => LBS.ByteString -> m ())
    -- ^ Send data
  , recv  :: !(forall m. (MonadIO m) => m (Maybe LBS.ByteString))
    -- ^ Receive data
  , close :: !(forall m. (MonadIO m) => m ())
    -- ^ Close socket
  , connectedPeer :: !PeerInfo
  }

type HeaderSize = Int


data NetworkError = ConnectionTimedOut
                  | NoAddressAvailable
  deriving (Show)

instance Exception NetworkError


----------------------------------------------------------------
-- Mock network
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
  { mnetIncoming    :: TVar (Map (NetAddr)
                                 [(MockSocket, NetAddr)])
    -- ^ Incoming connections for node.
  }



----------------------------------------------------------------
-- types used in tls
----------------------------------------------------------------

-- | Type for the action to receive input data
type RecvFun = IO ByteString
