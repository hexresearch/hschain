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
import Control.Monad.Catch    (MonadMask)
import Data.Coerce
import Katip                  (sl)

import HSChain.Blockchain.Internal.Engine.Types
import HSChain.Control.Class
import HSChain.Control.Shepherd
import HSChain.Control.Util
import HSChain.Internal.Types.Config
import HSChain.Internal.Types.Consensus
import HSChain.Logger
import HSChain.Mempool
import HSChain.Monitoring
import HSChain.Network.Types
import HSChain.P2P.Internal
import HSChain.Store
import HSChain.Types.Blockchain


----------------------------------------------------------------
-- Dispatcher
----------------------------------------------------------------

-- | Main process for networking. It manages accepting connections
--   from remote nodes, initiating connections to them, tracking state
--   of nodes and gossip.
startPeerDispatcher
  :: ( MonadMask m, MonadFork m, MonadLogger m, MonadReadDB a m, MonadTMMonitoring m
     , BlockData a)
  => NetworkCfg app
  -> NetworkAPI               -- ^ API for networking
  -> [NetAddr]                -- ^ Set of initial addresses to connect
  -> AppChans n a             -- ^ Channels for communication with main application
  -> MempoolHandle (Alg a) (TX a)
  -> m ()
startPeerDispatcher p2pCfg net addrs AppChans{..} mempool = logOnException $ do
  logger InfoS "Starting peer dispatcher" $ sl "seed" addrs
  peerRegistry <- newPeerRegistry
  peerNonceSet <- newNonceSet
  atomicallyIO $ addAddresses peerRegistry addrs
  withShepherd $ \peerShepherd -> do
    let peerCh = PeerChans { peerChanTx      = appChanTx
                           , peerChanRx      = writeTBQueue appChanRx
                           , consensusState  = readTVar appTMState
                           , p2pConfig       = coerce p2pCfg
                           , ..
                           }
    -- Accepting connection is managed by separate linked thread and
    -- this thread manages initiating connections
    runConcurrently
      [ acceptLoop (coerce p2pCfg) net peerCh mempool
      , pexFSM     (coerce p2pCfg) net peerCh mempool
      , pexMonitoring peerRegistry
      ]
