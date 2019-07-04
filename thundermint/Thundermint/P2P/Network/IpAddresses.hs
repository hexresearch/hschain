-- | Functions for manipulating IP addresses
--
{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE RecordWildCards #-}
module Thundermint.P2P.Network.IpAddresses (
    filterOutOwnAddresses
  , getLocalAddress
  , getLocalAddresses
  , isLocalAddress
  , normalizeNetAddr
  , normalizeIpAddr
  ) where


import Control.Monad
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Word              (Word32)
import qualified Network.Info   as Net
import qualified Network.Socket as Net

import Thundermint.Types.Network


-- | Get local node address
--
getLocalAddress :: IO Net.SockAddr
getLocalAddress =
    return $ Net.SockAddrInet 0 (Net.tupleToHostAddress (0x7f, 0, 0, 1))


defaultPort :: Net.PortNumber
defaultPort = 0

defaultFlow :: Net.FlowInfo
defaultFlow = 0

defaultScope :: Net.ScopeID
defaultScope = 0


getLocalAddresses :: MonadIO m => m [Net.SockAddr]
getLocalAddresses =
    concatMap (\Net.NetworkInterface{..} ->
        let Net.IPv4 ipv4w1 = ipv4
            Net.IPv6 ipv6w1' ipv6w2' ipv6w3' ipv6w4' = ipv6
            ipv6w1 = partOfIpv6ToIpv4 ipv6w1'
            ipv6w2 = partOfIpv6ToIpv4 ipv6w2'
            ipv6w3 = partOfIpv6ToIpv4 ipv6w3'
            ipv6w4 = partOfIpv6ToIpv4 ipv6w4'
        in [ Net.SockAddrInet  defaultPort ipv4w1
           , Net.SockAddrInet6 defaultPort defaultFlow (ipv6w1, ipv6w2, ipv6w3, ipv6w4) defaultScope
           ]
      )
      <$> (liftIO Net.getNetworkInterfaces)


isLocalAddress :: MonadIO m => Net.SockAddr -> m Bool
isLocalAddress sockAddr
  | isLoopback sockAddr = return True
  | otherwise           = do
      let sockAddr' = case sockAddr of
            Net.SockAddrInet  _   ipv4   -> Net.SockAddrInet  defaultPort ipv4
            Net.SockAddrInet6 _ _ ipv6 _ -> Net.SockAddrInet6 defaultPort defaultFlow ipv6 defaultScope
            s -> s
      elem sockAddr' <$> getLocalAddresses
  where
    isLoopback = isLoopback' . normalizeIpAddr
    isLoopback' (Net.SockAddrInet _ 0x100007f) = True
    isLoopback' _ = False


partOfIpv6ToIpv4 :: Word32 -> Word32
partOfIpv6ToIpv4 ipv6part =
    let (i1,i2,i3,i4) = Net.hostAddressToTuple ipv6part
    in Net.tupleToHostAddress (i4,i3,i2,i1)


normalizeIpAddr :: Net.SockAddr -> Net.SockAddr
normalizeIpAddr (Net.SockAddrInet6 p _ (0, 0, 0xFFFF, x) _) = -- IPv4 mapped addreses
    Net.SockAddrInet p (partOfIpv6ToIpv4 x)
normalizeIpAddr a = a

normalizeNetAddr :: NetAddr -> NetAddr
-- IPv4 mapped addreses
normalizeNetAddr (NetAddrV6 (0, 0, 0xFFFF, x) p) = NetAddrV4 (partOfIpv6ToIpv4 x) p
normalizeNetAddr a = a


getPort :: Net.SockAddr -> Net.PortNumber
getPort (Net.SockAddrInet port _) = port
getPort (Net.SockAddrInet6 port _ _ _) = port
getPort _ = defaultPort


filterOutOwnAddresses
    :: forall m. (MonadIO m)
    => Net.PortNumber       -- ^ Own port number
    -> [Net.SockAddr]       -- ^ Some addresses
    -> m [Net.SockAddr]
filterOutOwnAddresses ownPort =
  filterM (\a -> isLocalAddress a >>=
                 (\isLocal -> return $ not (isLocal && ownPort == getPort a))
          )
