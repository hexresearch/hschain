-- |
-- Abstract API for network which support
module HSChain.P2P.Network (
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
  , PeerInfo(..)
  ) where

import HSChain.P2P.Network.Internal.TLS
import HSChain.P2P.Network.Internal.TCP
import HSChain.P2P.Network.Internal.UDP
import HSChain.P2P.Network.Internal.Mock
import HSChain.P2P.Types

