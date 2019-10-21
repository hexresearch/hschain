{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeOperators              #-}
-- |
-- Helper function for running mock network of HSChain nodes
module HSChain.Run (
    DBT(..)
  , dbtRO
  , runDBT
    -- * New node code
  , makeAppLogic
  , NodeDescription(..)
  , BlockchainNet(..)
  , runNode
    -- * Standard callbacks
  , nonemptyMempoolCallback
  , mempoolFilterCallback
    -- * Running nodes
  , defCfg
  ) where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Catch
import Control.Concurrent.STM         (atomically)
import Control.Concurrent.STM.TBQueue (lengthTBQueue)
import Data.Maybe                     (isJust,fromJust)

import HSChain.Blockchain.Internal.Engine
import HSChain.Blockchain.Internal.Engine.Types
import HSChain.Blockchain.Interpretation
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

makeAppLogic
  :: ( MonadDB m alg a, MonadMask m, MonadIO m
     , BlockData a, Show (TX a), Ord (TX a), Crypto alg
     )
  => BChStore m a
  -> BChLogic    q   alg a
  -> Interpreter q m alg a
  -> m (AppLogic m alg a)
makeAppLogic store BChLogic{..} Interpreter{..} = do
  -- Create mempool
  let checkTx tx = do
        (mH, st) <- bchCurrentState store
        valSet <- case mH of
          Nothing -> return emptyValidatorSet
          Just h  -> throwNothingM (DBMissingValSet (succ h))
                  $  queryRO $ retrieveValidatorSet (succ h)
        isJust <$> interpretBCh (BlockchainState st valSet) (processTx tx)
  mempool <- newMempool checkTx
  --
  return AppLogic
    { appValidationFun  = \b bst -> (fmap . fmap) snd
                                  $ interpretBCh bst
                                  $ processBlock b
    , appBlockGenerator = \b txs -> fmap fromJust -- FIXME
                                 $ interpretBCh (newBlockState b)
                                 $ generateBlock b txs
    , appMempool        = mempool
    , appBchState       = store
    , appProposerChoice = randomProposerSHA512
    }


-- | Specification of node
data NodeDescription m alg a = NodeDescription
  { nodeValidationKey :: !(Maybe (PrivValidator alg))
    -- ^ Private key of validator.
  , nodeLogic         :: !(AppLogic m alg a)
    -- ^ Callbacks for validation of block, transaction and generation
    --   of new block.
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


-- | Create list of threads which should be executed in parallel.
runNode
  :: ( MonadDB m alg a, MonadMask m, MonadFork m, MonadLogger m, MonadTrace m, MonadTMMonitoring m
     , Crypto alg, BlockData a
     )
  => Configuration app
  -> NodeDescription m alg a
  -> m [m ()]
runNode cfg NodeDescription{..} = do
  let AppLogic{..}      = nodeLogic
      BlockchainNet{..} = nodeNetwork
      appCall = mempoolFilterCallback appMempool
             <> nodeCallbacks
  appCh <- newAppChans (cfgConsensus cfg)
  return
    [ id $ descendNamespace "net"
         $ startPeerDispatcher (cfgNetwork cfg) bchNetwork bchInitialPeers appCh appMempool
    , id $ descendNamespace "consensus"
         $ runApplication (cfgConsensus cfg) nodeValidationKey nodeLogic appCall appCh
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
