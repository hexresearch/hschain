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

import Thundermint.P2P.Network.Internal.TLS
import Thundermint.P2P.Network.Internal.TCP
import Thundermint.P2P.Network.Internal.UDP
import Thundermint.P2P.Network.Internal.Mock
import Thundermint.P2P.Types

import qualified Thundermint.P2P.Network.IpAddresses as Ip

