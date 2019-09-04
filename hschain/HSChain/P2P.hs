{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE UndecidableInstances #-}
-- |
-- Mock P2P
module HSChain.P2P (
    startPeerDispatcher
  , LogGossip(..)
  , NetAddr(..)
  , netAddrToSockAddr
  , sockAddrToNetAddr
  , generatePeerId
  -- * for tests only * --
  , PeerChans(..)
  , newPeerRegistry
  , newGossipCounters
  , GossipMsg(..)
  ) where

import Control.Concurrent.STM

import Control.Monad          (forM_, forever)
import Control.Monad.Catch    (MonadMask, finally, uninterruptibleMask_)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Monoid            ((<>))
import Katip                  (showLS)
import System.Random          (randomIO)


import HSChain.Blockchain.Internal.Engine.Types
import HSChain.Control
import HSChain.Crypto
import HSChain.Debug.Trace
import HSChain.Logger
import HSChain.Monitoring
import HSChain.P2P.Network
import HSChain.P2P.Types
import HSChain.Store
import HSChain.Types.Blockchain
import HSChain.Utils

import HSChain.P2P.Internal
import HSChain.P2P.Internal.Logging


----------------------------------------------------------------
-- Dispatcher
----------------------------------------------------------------

-- | Main process for networking. It manages accepting connections
--   from remote nodes, initiating connections to them, tracking state
--   of nodes and gossip.
startPeerDispatcher
  :: ( MonadMask m, MonadFork m, MonadLogger m, MonadTrace m, MonadReadDB m alg a, MonadTMMonitoring m
     , BlockData a, Crypto alg)
  => NetworkCfg
  -> NetworkAPI               -- ^ API for networking
  -> [NetAddr]                -- ^ Set of initial addresses to connect
  -> AppChans m alg a         -- ^ Channels for communication with main application
  -> Mempool m alg (TX a)
  -> m ()
startPeerDispatcher p2pConfig net addrs AppChans{..} mempool = logOnException $ do
  let PeerInfo peerId _ _ = ourPeerInfo net
  logger InfoS ("Starting peer dispatcher: addrs = " <> showLS addrs <> ", PeerId = " <> showLS peerId) ()
  trace TeNodeStarted
  peerRegistry            <- newPeerRegistry peerId
  peerChanPex             <- liftIO newBroadcastTChanIO
  peerChanPexNewAddresses <- liftIO newTChanIO
  gossipCnts              <- newGossipCounters
  let peerCh = PeerChans { peerChanTx      = appChanTx
                         , peerChanPex     = peerChanPex
                         , peerChanRx      = writeTBQueue appChanRx
                         , proposalStorage = makeReadOnlyPS appPropStorage
                         , consensusState  = readTVar appTMState
                         , ..
                         }
  -- Accepting connection is managed by separate linked thread and
  -- this thread manages initiating connections
  flip finally (uninterruptibleMask_ $ reapPeers peerRegistry) $ runConcurrently
    [ acceptLoop p2pConfig net peerCh mempool peerRegistry
     -- FIXME: we should manage requests for peers and connecting to
     --        new peers here
    , do waitSec 0.1
         forM_ addrs $ \a ->
             connectPeerTo p2pConfig net a peerCh mempool peerRegistry
         forever $ waitSec 0.1
    -- Peer connection monitor
    , descendNamespace "PEX" $
      peerPexMonitor p2pConfig net peerCh mempool peerRegistry
    -- Peer connection capacity monitor for debug purpose
    , descendNamespace "PEX" $
      peerPexCapacityDebugMonitor peerRegistry
    -- Peer new addreses capacity monitor
    , descendNamespace "PEX" $
      peerPexKnownCapacityMonitor peerCh peerRegistry (pexMinKnownConnections p2pConfig) (pexMaxKnownConnections p2pConfig)
    -- Listen for new raw node addresses; normalize it and put into prKnownAddreses
    , descendNamespace "PEX" $
      peerPexNewAddressMonitor peerChanPexNewAddresses peerRegistry net
    -- Output gossip statistics to
    , forever $ do
        logGossip gossipCnts
        waitSec 1.0
    ]


-- | Generate "unique" peer id for current session.
--
generatePeerId :: (MonadIO m) => m PeerId
generatePeerId = PeerId <$> liftIO randomIO



