-- | Stub for some 'realNetwork' implementations
module HSChain.P2P.Network.RealNetworkStub
    ( -- * Real network stub
      realNetworkStub
    ) where

import HSChain.P2P.Types

realNetworkStub :: PeerInfo -> NetworkAPI
realNetworkStub peerInfo = NetworkAPI
  { listenOn    = undefined
  , connect     = undefined
  , listenPort  = fromIntegral $ piPeerPort peerInfo
  , ourPeerInfo = peerInfo
  }
