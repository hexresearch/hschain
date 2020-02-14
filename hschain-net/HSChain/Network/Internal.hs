{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
-- |
-- Abstract API for network which support
module HSChain.Network.Internal (
    tcpHints
  , tcpListenHints
  , udpHints
  , netAddrToAddrInfo
  ) where

import Control.Exception      (throwIO)
import Control.Monad.IO.Class (MonadIO(..))
import qualified Network.Socket       as Net

import HSChain.Network.Types

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
