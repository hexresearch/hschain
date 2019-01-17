-- | Stub for some 'realNetwork' implementations
module Thundermint.P2P.Network.RealNetworkStub
    ( -- * Real network stub
      realNetworkStub
    ) where


import qualified Network.Socket                 as Net

import Thundermint.P2P.Types
import qualified Thundermint.P2P.Network.IpAddresses as Ip


realNetworkStub :: Net.ServiceName -> NetworkAPI
realNetworkStub serviceName = NetworkAPI
  { listenOn = undefined
  , connect  = undefined
  , filterOutOwnAddresses = Ip.filterOutOwnAddresses (Ip.serviceNameToPortNumber serviceName)
  , normalizeNodeAddress = flip setPort . Ip.normalizeIpAddr
  , listenPort = Ip.serviceNameToPortNumber serviceName
  }
  where
    setPort Nothing a = a
    setPort (Just port) (Net.SockAddrInet _ ha)        = Net.SockAddrInet port ha
    setPort (Just port) (Net.SockAddrInet6 _ fi ha si) = Net.SockAddrInet6 port fi ha si
    setPort _ s = s
