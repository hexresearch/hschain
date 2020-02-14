{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE RecordWildCards #-}
-- | Functions for manipulating IP addresses
--
module HSChain.P2P.Network.IpAddresses (
    filterOutOwnAddresses
  , getLocalAddresses
  , isLocalAddress
  , normalizeNetAddr
  , getNetAddrPort
  ) where


import Control.Monad
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Word
import qualified Network.Info   as Net
import qualified Network.Socket as Net

import HSChain.Types.Network


defaultPort :: Word16
defaultPort = 0

getLocalAddresses :: MonadIO m => m [NetAddr]
getLocalAddresses
  = concatMap toAddr <$> liftIO Net.getNetworkInterfaces
  where
    toAddr Net.NetworkInterface{..} =
      [ NetAddrV4 ipv4w1                           defaultPort
      , NetAddrV6 (ipv6w1, ipv6w2, ipv6w3, ipv6w4) defaultPort
      ]
      where
        Net.IPv4 ipv4w1                          = ipv4
        Net.IPv6 ipv6w1' ipv6w2' ipv6w3' ipv6w4' = ipv6
        ipv6w1 = partOfIpv6ToIpv4 ipv6w1'
        ipv6w2 = partOfIpv6ToIpv4 ipv6w2'
        ipv6w3 = partOfIpv6ToIpv4 ipv6w3'
        ipv6w4 = partOfIpv6ToIpv4 ipv6w4'


isLocalAddress :: MonadIO m => NetAddr -> m Bool
isLocalAddress sockAddr
  | isLoopback sockAddr = return True
  | otherwise           = do
      let sockAddr' = case sockAddr of
            NetAddrV4 ip _ -> NetAddrV4 ip defaultPort
            NetAddrV6 ip _ -> NetAddrV6 ip defaultPort
      elem sockAddr' <$> getLocalAddresses
  where
    isLoopback = isLoopback' . normalizeNetAddr
    isLoopback' (NetAddrV4 0x100007f _) = True
    isLoopback' _ = False


partOfIpv6ToIpv4 :: Word32 -> Word32
partOfIpv6ToIpv4 ipv6part =
    let (i1,i2,i3,i4) = Net.hostAddressToTuple ipv6part
    in Net.tupleToHostAddress (i4,i3,i2,i1)

normalizeNetAddr :: NetAddr -> NetAddr
-- IPv4 mapped addreses
normalizeNetAddr (NetAddrV6 (0, 0, 0xFFFF, x) p) = NetAddrV4 (partOfIpv6ToIpv4 x) p
normalizeNetAddr a = a


getNetAddrPort :: NetAddr -> Word16
getNetAddrPort (NetAddrV4 _ p) = p
getNetAddrPort (NetAddrV6 _ p) = p

filterOutOwnAddresses
    :: forall m. (MonadIO m)
    => Word16               -- ^ Own port number
    -> [NetAddr]            -- ^ Some addresses
    -> m [NetAddr]
filterOutOwnAddresses ownPort =
  filterM $ \a -> do isLocal <- isLocalAddress a
                     return $! not $ isLocal && ownPort == getNetAddrPort a
