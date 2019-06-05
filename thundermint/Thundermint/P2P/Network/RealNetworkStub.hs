-- | Stub for some 'realNetwork' implementations
module Thundermint.P2P.Network.RealNetworkStub
    ( -- * Real network stub
      realNetworkStub
    ) where

import qualified Data.Set as Set

import Thundermint.P2P.Types
import qualified Thundermint.P2P.Network.IpAddresses as Ip


realNetworkStub :: PeerInfo -> NetworkAPI
realNetworkStub peerInfo = NetworkAPI
  { listenOn = undefined
  , connect  = undefined
  , filterOutOwnAddresses = fmap (Set.map sockAddrToNetAddr)
                          . Ip.filterOutOwnAddresses (fromIntegral (piPeerPort peerInfo))
                          . Set.map netAddrToSockAddr
  , normalizeNodeAddress  = flip setPort
                          . Ip.normalizeNetAddr
  , listenPort            = fromIntegral $ piPeerPort peerInfo
  , ourPeerInfo           = peerInfo
  }
  where
    setPort Nothing a = a
    setPort (Just port) (NetAddrV4 ha _) = NetAddrV4 ha $ fromIntegral port
    setPort (Just port) (NetAddrV6 ha _) = NetAddrV6 ha $ fromIntegral port
