-- | Stub for some 'realNetwork' implementations
module Thundermint.P2P.Network.RealNetworkStub
    ( -- * Real network stub
      realNetworkStub
    ) where

import qualified Data.Set as Set

import qualified Network.Socket                 as Net

import Thundermint.P2P.Types
import qualified Thundermint.P2P.Network.IpAddresses as Ip


realNetworkStub :: Net.ServiceName -> NetworkAPI
realNetworkStub serviceName = NetworkAPI
  { listenOn = undefined
  , connect  = undefined
  , filterOutOwnAddresses = fmap (Set.map sockAddrToNetAddr)
                          . Ip.filterOutOwnAddresses (Ip.serviceNameToPortNumber serviceName)
                          . Set.map netAddrToSockAddr
  , normalizeNodeAddress  = flip setPort
                          . sockAddrToNetAddr
                          . Ip.normalizeIpAddr
                          . netAddrToSockAddr
  , listenPort            = Ip.serviceNameToPortNumber serviceName
  , ourPeerInfo           = PeerInfo 0 0 0
  }
  where
    setPort Nothing a = a
    setPort (Just port) (NetAddrV4 ha _) = NetAddrV4 ha $ fromIntegral port
    setPort (Just port) (NetAddrV6 ha _) = NetAddrV6 ha $ fromIntegral port
