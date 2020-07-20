{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}
-- |
-- Helper function for running mock network of HSChain nodes
module HSChain.Run (
    -- * New node code
    runNode
  -- , makeMempool
  , NodeDescription(..)
  , BlockchainNet(..)
    -- ** Configuration and timeouts
  , Configuration(..)
  , ConsensusCfg(..)
  , NetworkCfg(..)
    -- * Standard callbacks
  , nonemptyMempoolCallback
  ) where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Catch
import Control.Concurrent.STM         (atomically)
import Control.Concurrent.STM.TBQueue (lengthTBQueue)

import HSChain.Blockchain.Internal.Engine
import HSChain.Blockchain.Internal.Engine.Types
import HSChain.Control.Class
import HSChain.Control.Delay
import HSChain.Internal.Types.Config
import HSChain.Internal.Types.Consensus
import HSChain.Logger
import HSChain.Mempool
import HSChain.Monitoring
import HSChain.Network.Types
import HSChain.P2P
import HSChain.Store
import HSChain.Types


----------------------------------------------------------------
--
----------------------------------------------------------------

-- | Specification of node
data NodeDescription m a = NodeDescription
  { nodeValidationKey :: !(Maybe (PrivValidator (Alg a)))
    -- ^ Private key of validator.
  , nodeGenesis       :: !(Genesis a)
    -- ^ Genesis block of node
  , nodeCallbacks     :: !(AppCallbacks m a)
    -- ^ Callbacks with monoidal structure
  , nodeStateView     :: !(StateView m a)
    -- ^ Object that allows to process 
  , nodeNetwork       :: !BlockchainNet
    -- ^ Dictionary of functions to communicate with other nodes over
    --   network.
  }

-- | Specification of network
data BlockchainNet = BlockchainNet
  { bchNetwork      :: !NetworkAPI
  , bchInitialPeers :: ![NetAddr]
  }


-- | Start node. Function returns list of monadic actions which then
--   should be run concurrently, for example with
--   'runConcurrently'. List of actions is returned in case when we
--   need to run something else along them.
runNode
  :: ( MonadDB m, MonadCached a m, MonadMask m, MonadFork m, MonadLogger m, MonadTMMonitoring m
     , BlockData a, Eq a, Show a
     )
  => Configuration app         -- ^ Timeouts for network and consensus
  -> NodeDescription m a       -- ^ Description of node.
  -> m [m ()]
runNode Configuration{..} NodeDescription{..} = do
  let BlockchainNet{..}  = nodeNetwork
  appCh <- newAppChans cfgConsensus
  initDatabase
  st    <- initializeBlockchain nodeGenesis nodeStateView
  return
    [ id $ descendNamespace "net"
         $ startPeerDispatcher cfgNetwork bchNetwork bchInitialPeers appCh
           (mempoolHandle $ stateMempool st)
    , id $ descendNamespace "consensus"
         $ runApplication cfgConsensus nodeValidationKey st nodeCallbacks appCh
    -- , forever $ do
    --     MempoolInfo{..} <- mempoolStats appMempool
    --     usingGauge prometheusMempoolSize      mempool'size
    --     usingGauge prometheusMempoolDiscarded mempool'discarded
    --     usingGauge prometheusMempoolFiltered  mempool'filtered
    --     usingGauge prometheusMempoolAdded     mempool'added
    --     waitSec 1.0
    , forever $ do
        n <- liftIO $ atomically $ lengthTBQueue $ appChanRx appCh
        usingGauge prometheusMsgQueue n
        waitSec 1.0
    ]

----------------------------------------------------------------
-- Callbacks
----------------------------------------------------------------

-- | Callback which allow block creation only if mempool is not empty
nonemptyMempoolCallback :: (MonadIO m) => Mempool m alg tx -> AppCallbacks m a
nonemptyMempoolCallback mempool = mempty
  { appCanCreateBlock = \_ -> do
      n <- mempoolSize mempool
      return $! Just $! n > 0
  }
