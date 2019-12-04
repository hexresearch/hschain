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
import Control.Concurrent.STM         (atomically)
import Control.Concurrent.STM.TBQueue (lengthTBQueue)
import Data.Maybe                     (isJust)

import HSChain.Blockchain.Internal.Engine
import HSChain.Blockchain.Internal.Engine.Types
import HSChain.Control
import HSChain.Crypto
import HSChain.Debug.Trace
import HSChain.Exceptions
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
newtype Interpreter q m alg a = Interpreter
  { interpretBCh :: forall x.
                    BlockchainState alg a
                 -> q x
                 -> m (Maybe (x, BlockchainState alg a))
  }


-- | Create default mempool which checks transactions against current
--   state
makeMempool
  :: (MonadIO m, MonadReadDB m alg a, Ord (TX a), Show (TX a), Crypto alg, BlockData a)
  => BChStore m a
  -> BChLogic q   alg a
  -> Interpreter q m alg a
  -> m (Mempool m alg (TX a))
makeMempool store BChLogic{..} Interpreter{..} =
  newMempool $ \tx -> do
    (mH, st) <- bchCurrentState store
    valSet   <- queryRO $ mustRetrieveValidatorSet $ case mH of
      Nothing -> Height 0
      Just h  -> succ h
    isJust <$> interpretBCh (BlockchainState st valSet) (processTx tx)


-- | Create 'AppLogic' which should be then passed to 'runNode' from
--   description of blockchain logic and storage of blockchain state.
makeAppLogic
  :: ( MonadDB m alg a, MonadMask m, MonadIO m
     , BlockData a, Show (TX a), Ord (TX a), Crypto alg
     )
  => BChLogic    q   alg a      -- ^ Blockchain logic
  -> Interpreter q m alg a      -- ^ Runner for logic
  -> AppLogic m alg a
makeAppLogic BChLogic{..} Interpreter{..} = AppLogic
  { appValidationFun  = \b bst -> (fmap . fmap) snd
                                $ interpretBCh bst
                                $ processBlock b
  , appBlockGenerator = \newB txs -> do
      mb <- interpretBCh (newBlockState newB)
          $ generateBlock newB txs
      case mb of
        Just b  -> return b
        Nothing -> throwM InvalidBlockGenerated
  }


-- | Specification of node
data NodeDescription m alg a = NodeDescription
  { nodeValidationKey :: !(Maybe (PrivValidator alg))
    -- ^ Private key of validator.
  , nodeGenesis       :: !(Block alg a, ValidatorSet alg)
    -- ^ Genesis block of node
  , nodeLogic         :: !(AppLogic m alg a)
    -- ^ Callbacks for validation of block, transaction and generation
    --   of new block.
  , nodeStore         :: !(AppStore m alg a)
  , nodeCallbacks     :: !(AppCallbacks m alg a)
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
  :: ( MonadDB m alg a, MonadMask m, MonadFork m, MonadLogger m, MonadTrace m, MonadTMMonitoring m
     , Crypto alg, BlockData a, Eq a, Show a
     )
  => Configuration app          -- ^ Timeouts for network and consensus
  -> NodeDescription m alg a    -- ^ Description of node.
  -> m [m ()]
runNode cfg NodeDescription{..} = do
  let AppLogic{..}      = nodeLogic
      AppStore{..}      = nodeStore
      BlockchainNet{..} = nodeNetwork
      appCall = mempoolFilterCallback appMempool
             <> nodeCallbacks
  appCh <- newAppChans (cfgConsensus cfg)
  return
    [ id $ descendNamespace "net"
         $ startPeerDispatcher (cfgNetwork cfg) bchNetwork bchInitialPeers appCh appMempool
    , id $ descendNamespace "consensus"
         $ runApplication (cfgConsensus cfg) nodeValidationKey nodeGenesis nodeLogic nodeStore appCall appCh
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
mempoolFilterCallback :: (MonadLogger m) => Mempool m alg tx -> AppCallbacks m alg a
mempoolFilterCallback mempool = mempty
  { appCommitCallback = \_ -> descendNamespace "mempool" $ do
      do before <- mempoolStats mempool
         logger InfoS "Mempool before filtering" before
      filterMempool mempool
      do after <- mempoolStats mempool
         logger InfoS "Mempool after filtering" after
  }

-- | Callback which allow block creation only if mempool is not empty
nonemptyMempoolCallback :: (Monad m) => Mempool m alg tx -> AppCallbacks m alg a
nonemptyMempoolCallback mempool = mempty
  { appCanCreateBlock = \_ -> do
      n <- mempoolSize mempool
      return $! Just $! n > 0
  }
