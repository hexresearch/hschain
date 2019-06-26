{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
-- |
-- Helper function for running mock network of thundermint nodes
module Thundermint.Run (
    DBT(..)
  , dbtRO
  , runDBT
    -- * Validators
  , makePrivateValidators
  , makeValidatorSetFromPriv
    -- * Network connectivity
  , connectAll2All
  , connectRing
    -- * New node code
  , Abort(..)
  , Topology(..)
  , logicFromFold
  , NodeDescription(..)
  , BlockchainNet(..)
  , runNode
    -- * Standard callbacks
  , callbackAbortAtH
  , nonemptyMempoolCallback
  , mempoolFilterCallback
    -- * Running nodes
  , defCfg
  ) where

import Codec.Serialise
import Control.Monad
import Control.Monad.Fail hiding (fail)
import Control.Monad.IO.Class
import Control.Monad.Catch
import Control.Concurrent.STM         (atomically)
import Control.Concurrent.STM.TBQueue (lengthTBQueue)
import Data.Maybe      (isJust)

import Thundermint.Blockchain.Internal.Engine
import Thundermint.Blockchain.Interpretation
import Thundermint.Blockchain.Internal.Engine.Types
import Thundermint.Types
import Thundermint.Crypto
import Thundermint.Debug.Trace
import Thundermint.Logger
import Thundermint.Mock.KeyList
import Thundermint.Mock.Types
import Thundermint.P2P
import Thundermint.P2P.Network
import Thundermint.Store
import Thundermint.Store.STM
import Thundermint.Monitoring
import Thundermint.Utils

import Thundermint.Control (MonadFork)
import Thundermint.Types (CheckSignature(..))

----------------------------------------------------------------
--
----------------------------------------------------------------

logicFromFold
  :: (MonadDB m alg a, MonadMask m, MonadIO m, MonadFail m
     , BlockData a, Show (TX a), Ord (TX a), Crypto alg, Serialise st
     )
  => BlockFold st alg a
  -> m (BChState m st, AppLogic m alg a)
logicFromFold transitions@BlockFold{..} = do
  hChain   <- queryRO blockchainHeight
  bchState <- newBChState transitions
  _        <- stateAtH bchState (succ hChain)
  -- Create mempool
  let checkTx tx = do
        st <- currentState bchState
        -- FIXME: We need real height here!
        return $ isJust $ processTx CheckSignature (Height 1) tx st
  mempool <- newMempool checkTx
  --
  return ( bchState
         , AppLogic { appValidationFun    = \valset b -> do
                         let h = headerHeight $ blockHeader b
                         st <- stateAtH bchState h
                         return $ valset <$ processBlock CheckSignature b st
                     , appCommitQuery     = SimpleQuery $ \valset _ -> return valset
                     , appBlockGenerator  = \txs h _ _ _ valset -> do
                         st  <- stateAtH bchState h
                         return (transactionsToBlock h st txs, valset)
                     , appMempool         = mempool
                     }
         )


-- | Specification of node
data NodeDescription m alg a = NodeDescription
  { nodeValidationKey :: !(Maybe (PrivValidator alg))
    -- ^ Private key of validator
  , nodeLogic         :: !(AppLogic m alg a)
    -- ^ Callbacks for validation of block, transaction and generation
    --   of new block
  , nodeCallbacks     :: !(AppCallbacks m alg a)
    -- ^ Callbacks with monoidal structure
  , nodeNetwork       :: !BlockchainNet
  }

-- | Specification of network
data BlockchainNet = BlockchainNet
  { bchNetwork      :: !NetworkAPI
  , bchInitialPeers :: ![NetAddr]
  }


-- | Create list of threads which should be executed in parallel.
runNode
  :: ( MonadDB m alg a, MonadMask m, MonadFork m, MonadLogger m, MonadTrace m, MonadTMMonitoring m, MonadFail m
     , Crypto alg, Show a, BlockData a
     )
  => Configuration app
  -> NodeDescription m alg a
  -> m [m ()]
runNode cfg NodeDescription{..} = do
  let appLogic@AppLogic{..} = nodeLogic
      BlockchainNet{..}     = nodeNetwork
      appCall = mempoolFilterCallback appMempool
             <> nodeCallbacks
  appCh <- newAppChans (cfgConsensus cfg)
  return
    [ id $ descendNamespace "net"
         $ startPeerDispatcher (cfgNetwork cfg) bchNetwork bchInitialPeers appCh appMempool
    , id $ descendNamespace "consensus"
         $ runApplication (cfgConsensus cfg) nodeValidationKey appLogic appCall appCh
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

-- | Callback which aborts execution when blockchain exceed given
--   height. It's done by throwing 'Abort'.
callbackAbortAtH :: MonadThrow m => Height -> AppCallbacks m alg a
callbackAbortAtH hMax = mempty
  { appCommitCallback = \b ->
      when (headerHeight (blockHeader b) > hMax) $ throwM Abort
  }

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
  { appCanCreateBlock = \_ _ -> do
      n <- mempoolSize mempool
      return $! Just $! n > 0
  }
