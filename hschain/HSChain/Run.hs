{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
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
import Control.Concurrent             (threadDelay)
import Control.Concurrent.STM         (atomically)
import Control.Concurrent.STM.TBQueue (lengthTBQueue)
import Katip (sl)

import HSChain.Crypto (Hashed)
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
data NodeDescription m view = NodeDescription
  { nodeValidationKey :: !(Maybe (PrivValidator (AlgOf view)))
    -- ^ Private key of validator.
  , nodeGenesis       :: !(Genesis (BlockType view))
    -- ^ Genesis block of node
  , nodeCallbacks     :: !(AppCallbacks m (BlockType view))
    -- ^ Callbacks with monoidal structure
  , nodeStateView     :: !view
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
     , Eq a, Show a, a ~ BlockType view, StateView view, ViewConstraints view m
     )
  => Configuration app         -- ^ Timeouts for network and consensus
  -> NodeDescription m view    -- ^ Description of node.
  -> Mempool (Hashed (Alg a) (TX a)) (TX a)
  -> m [m ()]
runNode Configuration{..} NodeDescription{..} mempool = do
  let BlockchainNet{..}  = nodeNetwork
  appCh <- newAppChans cfgConsensus
  initDatabase
  st    <- initializeBlockchain nodeGenesis nodeStateView
  return
    [ id $ descendNamespace "net"
         $ startPeerDispatcher cfgNetwork bchNetwork bchInitialPeers appCh mempool
    , id $ descendNamespace "consensus"
         $ runApplication cfgConsensus nodeValidationKey st nodeCallbacks appCh
    , descendNamespace "mempool" $ forever $ do
        MempoolInfo{..} <- mempoolInfo mempool
        usingGauge prometheusMempoolSize      mempool'size
        usingGauge prometheusMempoolDiscarded mempool'discarded
        usingGauge prometheusMempoolFiltered  mempool'filtered
        usingGauge prometheusMempoolAdded     mempool'added
        logger InfoS "Mempool stats" ( sl "size"      mempool'size
                                    <> sl "added"     mempool'added
                                    <> sl "discarded" mempool'discarded
                                    <> sl "filtered"  mempool'filtered
                                     )
        liftIO $ threadDelay $ 1000 * mempoolLogInterval cfgNetwork
    , forever $ do
        n <- liftIO $ atomically $ lengthTBQueue $ appChanRx appCh
        usingGauge prometheusMsgQueue n
        waitSec 1.0
    ]

----------------------------------------------------------------
-- Callbacks
----------------------------------------------------------------

-- | Callback which allow block creation only if mempool is not empty
nonemptyMempoolCallback :: (MonadIO m) => Mempool alg tx -> AppCallbacks m a
nonemptyMempoolCallback mempool = mempty
  { appCanCreateBlock = \_ -> do
      n <- mempoolSize mempool
      return $! Just $! n > 0
  }
