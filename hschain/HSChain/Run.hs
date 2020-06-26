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
  -- , nonemptyMempoolCallback
  -- , mempoolFilterCallback
  ) where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Catch
import Control.Monad.Trans.Except
import Control.Concurrent.STM         (atomically)
import Control.Concurrent.STM.TBQueue (lengthTBQueue)
import Data.Either                    (isRight)

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
import HSChain.Types.Merkle.Types


----------------------------------------------------------------
--
----------------------------------------------------------------


-- -- | Create default mempool which checks transactions against current
-- --   state
-- makeMempool
--   :: forall m a. (MonadIO m, MonadReadDB m a, Ord (TX a), Show (TX a), BlockData a)
--   => BChStore m a
--   -> (forall x. BChMonad a x -> ExceptT (BChError a) m x)
--   -> m (Mempool m (Alg a) (TX a))
-- makeMempool store runner =
--   newMempool $ \tx -> do
--     (mH, st) <- bchCurrentState store
--     mvalSet  <- queryRO $ retrieveValidatorSet $ case mH of
--       Nothing -> Height 0
--       Just h  -> succ h
--     case mvalSet of
--       Nothing -> return False
--       Just vs -> fmap isRight
--                $ runExceptT
--                $ processTx BChEval { bchValue        = tx
--                                    , blockchainState = st
--                                    , validatorSet    = merkled vs
--                                    }
--   where
--     BChLogic{..} = hoistDict runner (bchLogic @a)

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
  :: ( MonadDB a m, MonadMask m, MonadFork m, MonadLogger m, MonadTMMonitoring m
     , BlockData a, Eq a, Show a
     )
  => Configuration app         -- ^ Timeouts for network and consensus
  -> NodeDescription m a       -- ^ Description of node.
  -> m [m ()]
runNode Configuration{..} NodeDescription{..} = do
  let BlockchainNet{..}  = nodeNetwork
  appCh <- newAppChans cfgConsensus
  initializeBlockchain nodeGenesis nodeStateView
  return
    [ id $ descendNamespace "net"
         $ startPeerDispatcher cfgNetwork bchNetwork bchInitialPeers appCh
           (mempoolHandle $ stateMempool nodeStateView)
    , id $ descendNamespace "consensus"
         $ runApplication cfgConsensus nodeValidationKey nodeStateView nodeCallbacks appCh
    -- , forever $ do
    --     MempoolInfo{..} <- mempoolStats appMempool
    --     usingGauge prometheusMempoolSize      mempool'size
    --     usingGauge prometheusMempoolDiscarded mempool'discarded
    --     usingGauge prometheusMempoolFiltered  mempool'filtered
    --     usingGauge prometheusMempoolAdded     mempool'added
    --     waitSec 1.0
        -- n <- liftIO $ atomically $ lengthTBQueue $ appChanRx appCh
        -- usingGauge prometheusMsgQueue n
        -- waitSec 1.0
    ]

----------------------------------------------------------------
-- Callbacks
----------------------------------------------------------------

-- -- | Callback which removes from mempool all transactions which are
-- --   not longer valid.
-- mempoolFilterCallback :: (MonadLogger m) => Mempool m alg tx -> AppCallbacks m a
-- mempoolFilterCallback mempool = mempty
--   { appCommitCallback = \_ -> descendNamespace "mempool" $ do
--       filterMempool mempool
--   }

-- -- | Callback which allow block creation only if mempool is not empty
-- nonemptyMempoolCallback :: (Monad m) => Mempool m alg tx -> AppCallbacks m a
-- nonemptyMempoolCallback mempool = mempty
--   { appCanCreateBlock = \_ -> do
--       n <- mempoolSize mempool
--       return $! Just $! n > 0
--   }
