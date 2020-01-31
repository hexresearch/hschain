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
  , NetAddr(..)
  , netAddrToSockAddr
  , sockAddrToNetAddr
  -- * for tests only * --
  , PeerChans(..)
  , newPeerRegistry
  , GossipMsg(..)
  ) where

import Control.Concurrent.STM

import Control.Monad          (forM_, forever)
import Control.Monad.Catch    (MonadMask)
import Data.Monoid            ((<>))
import Katip                  (showLS)

import HSChain.Blockchain.Internal.Engine.Types
import HSChain.Control
import HSChain.Logger
import HSChain.Monitoring
import HSChain.P2P.Network
import HSChain.P2P.Types
import HSChain.Store
import HSChain.Types.Blockchain
import HSChain.Utils

import HSChain.P2P.Internal


----------------------------------------------------------------
-- Dispatcher
----------------------------------------------------------------

-- | Main process for networking. It manages accepting connections
--   from remote nodes, initiating connections to them, tracking state
--   of nodes and gossip.
startPeerDispatcher
  :: ( MonadMask m, MonadFork m, MonadLogger m, MonadReadDB m a, MonadTMMonitoring m
     , BlockData a)
  => NetworkCfg
  -> NetworkAPI               -- ^ API for networking
  -> [NetAddr]                -- ^ Set of initial addresses to connect
  -> AppChans a               -- ^ Channels for communication with main application
  -> Mempool m (Alg a) (TX a)
  -> m ()
startPeerDispatcher p2pConfig net addrs AppChans{..} mempool = logOnException $ do
  logger InfoS ("Starting peer dispatcher: addrs = " <> showLS addrs) ()
  peerRegistry <- newPeerRegistry
  peerNonceSet <- newNonceSet
  atomicallyIO $ addAddresses peerRegistry addrs
  withShepherd $ \peerShepherd -> do
    let peerCh = PeerChans { peerChanTx      = appChanTx
                           , peerChanRx      = writeTBQueue appChanRx
                           , consensusState  = readTVar appTMState
                           , ..
                           }
    -- Accepting connection is managed by separate linked thread and
    -- this thread manages initiating connections
    runConcurrently
      [ acceptLoop p2pConfig net peerCh mempool
      -- Peer connection monitor
      , descendNamespace "PEX" $ pexFSM p2pConfig net peerCh mempool
      , pexMonitoring peerRegistry
      ]




