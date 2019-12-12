{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}
-- |
-- Helper function for running mock network of HSChain nodes
module HSChain.Run (
    -- * New node code
    runNode
  , makeAppLogic
  , makeMempool
  , NodeDescription(..)
  , BlockchainNet(..)
  , Interpreter(..)
    -- ** Configuration and timeouts
  , DefaultConfig(..)
  , Configuration(..)
  , ConsensusCfg(..)
  , NetworkCfg(..)
    -- * Standard callbacks
  , nonemptyMempoolCallback
  , mempoolFilterCallback
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
import HSChain.Control
import HSChain.Crypto
import HSChain.Debug.Trace
import HSChain.Logger
import HSChain.Monitoring
import HSChain.P2P
import HSChain.P2P.Network
import HSChain.Store
import HSChain.Store.STM
import HSChain.Types
import HSChain.Utils


----------------------------------------------------------------
--
----------------------------------------------------------------

-- | Interpreter for mond in which evaluation of blockchain is
--   performed
newtype Interpreter m a = Interpreter
  { interpretBCh :: forall x. BChMonad a x -> ExceptT (BChError a) m x
  }


-- | Create default mempool which checks transactions against current
--   state
makeMempool
  :: (MonadIO m, MonadReadDB m a, Ord (TX a), Show (TX a), Crypto alg, BlockData a)
  => BChStore m a
  -> AppLogic m a
  -> m (Mempool m alg (TX a))
makeMempool store BChLogic{..} =
  newMempool $ \tx -> do
    (mH, st) <- bchCurrentState store
    mvalSet  <- queryRO $ retrieveValidatorSet $ case mH of
      Nothing -> Height 0
      Just h  -> succ h
    case mvalSet of
      Nothing -> return False
      Just vs -> fmap isRight
               $ runExceptT
               $ processTx BChEval { bchValue        = tx
                                   , blockchainState = st
                                   , validatorSet    = vs
                                   }


-- | Create 'AppLogic' which should be then passed to 'runNode' from
--   description of blockchain logic and storage of blockchain state.
makeAppLogic
  :: BChLogic (BChMonad a) a -- ^ Blockchain logic
  -> Interpreter m a         -- ^ Runner for logic
  -> AppLogic m a
makeAppLogic logic Interpreter{..} = hoistDict interpretBCh logic

-- | Specification of node
data NodeDescription m a = NodeDescription
  { nodeValidationKey :: !(Maybe (PrivValidator (Alg a)))
    -- ^ Private key of validator.
  , nodeGenesis       :: !(Genesis a)
    -- ^ Genesis block of node
  , nodeRunner        :: !(Interpreter m a)
    -- ^ Function for evaluation of blockchain transitions
  , nodeStore         :: !(AppStore m a)
    -- ^ Storage for state of blockchain.
  , nodeCallbacks     :: !(AppCallbacks m a)
    -- ^ Callbacks with monoidal structure
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
  :: ( MonadDB m a, MonadMask m, MonadFork m, MonadLogger m, MonadTrace m, MonadTMMonitoring m
     , BlockData a, Eq a, Show a
     )
  => Configuration app          -- ^ Timeouts for network and consensus
  -> NodeDescription m a    -- ^ Description of node.
  -> m [m ()]
runNode cfg NodeDescription{..} = do
  let logic@BChLogic{..} = makeAppLogic bchLogic nodeRunner
      AppStore{..}       = nodeStore
      BlockchainNet{..}  = nodeNetwork
      appCall = mempoolFilterCallback appMempool
             <> nodeCallbacks
  appCh <- newAppChans (cfgConsensus cfg)
  return
    [ id $ descendNamespace "net"
         $ startPeerDispatcher (cfgNetwork cfg) bchNetwork bchInitialPeers appCh appMempool
    , id $ descendNamespace "consensus"
         $ runApplication (cfgConsensus cfg) nodeValidationKey nodeGenesis logic nodeStore appCall appCh
    , forever $ do
        MempoolInfo{..} <- mempoolStats appMempool
        usingGauge prometheusMempoolSize      mempool'size
        usingGauge prometheusMempoolDiscarded mempool'discarded
        usingGauge prometheusMempoolFiltered  mempool'filtered
        usingGauge prometheusMempoolAdded     mempool'added
        waitSec 1.0
    , forever $ do
        n <- liftIO $ atomically $ lengthTBQueue $ appChanRx appCh
        usingGauge prometheusMsgQueue n
        waitSec 1.0
    ]

----------------------------------------------------------------
-- Callbacks
----------------------------------------------------------------

-- | Callback which removes from mempool all transactions which are
--   not longer valid.
mempoolFilterCallback :: (MonadLogger m) => Mempool m alg tx -> AppCallbacks m a
mempoolFilterCallback mempool = mempty
  { appCommitCallback = \_ -> descendNamespace "mempool" $ do
      do before <- mempoolStats mempool
         logger InfoS "Mempool before filtering" before
      filterMempool mempool
      do after <- mempoolStats mempool
         logger InfoS "Mempool after filtering" after
  }

-- | Callback which allow block creation only if mempool is not empty
nonemptyMempoolCallback :: (Monad m) => Mempool m alg tx -> AppCallbacks m a
nonemptyMempoolCallback mempool = mempty
  { appCanCreateBlock = \_ -> do
      n <- mempoolSize mempool
      return $! Just $! n > 0
  }
