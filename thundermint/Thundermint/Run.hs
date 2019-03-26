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
  , NodeLogic(..)
  , logicFromFold
  , logicFromPersistent
  , NodeDescription(..)
  , BlockchainNet(..)
  , runNode
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
import Thundermint.Control
import Thundermint.Crypto
import Thundermint.Debug.Trace
import Thundermint.Logger
import Thundermint.Mock.KeyList
import Thundermint.Mock.Types
import Thundermint.P2P
import Thundermint.P2P.Network
import Thundermint.Store
import Thundermint.Store.SQL
import Thundermint.Store.STM
import Thundermint.Monitoring
import Thundermint.Utils

import Thundermint.Control (MonadFork)
import Thundermint.Types (CheckSignature(..))

----------------------------------------------------------------
--
----------------------------------------------------------------

data NodeLogic m alg a = NodeLogic
  { nodeBlockValidation :: !(Block alg a -> m (Maybe [ValidatorChange alg]))
    -- ^ Callback used for validation of blocks
  , nodeCommitQuery     :: !(CommitCallback m alg a)
    -- ^ Query for modifying user state.
  , nodeBlockGenerator  :: !(Height
                          -> Time
                          -> Maybe (Commit alg a)
                          -> [ByzantineEvidence alg a]
                          -> m (a, [ValidatorChange alg]))
    -- ^ Generator for a new block
  , nodeMempool         :: !(Mempool m alg (TX a))
    -- ^ Mempool of node
  }

logicFromFold
  :: (MonadDB m alg a, MonadMask m, MonadIO m, BlockData a, Ord (TX a), Crypto alg, MonadFail m, Serialise st)
  => BlockFold st alg a
  -> m (BChState m st, NodeLogic m alg a)
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
         , NodeLogic { nodeBlockValidation = \b -> do
                         let h = headerHeight $ blockHeader b
                         st <- stateAtH bchState h
                         return $ [] <$ processBlock CheckSignature b st
                     , nodeCommitQuery     = SimpleQuery $ \_ -> return []
                     , nodeBlockGenerator  = \h _ _ _ -> do
                         st  <- stateAtH bchState h
                         txs <- peekNTransactions mempool Nothing
                         return (transactionsToBlock h st txs, [])
                     , nodeMempool         = mempool
                     }
         )

logicFromPersistent
  :: (MonadDB m alg a, MonadIO m, BlockData a, Ord (TX a), Crypto alg, FloatOut dct)
  => PersistentState dct alg a
  -> m (NodeLogic m alg a)
logicFromPersistent PersistentState{..} = do
  -- Create mempool
  let checkTx tx = do
        -- FIXME: we need real height here
        r <- queryRO $ runEphemeralQ persistedData (processTxDB (Height 1) tx)
        return $! isJust r
  mempool <- newMempool checkTx
  -- Now we need to update state using genesis block.
  do r <- queryRW $ do
       Just genesis <- retrieveBlock (Height 0)
       runBlockUpdate (Height 0) persistedData $ processBlockDB genesis
     case r of
       Just () -> return ()
       Nothing -> error "Cannot initialize persistent storage"
  --
  return NodeLogic
    { nodeBlockValidation = \b -> do
        r <- queryRO $ runEphemeralQ persistedData (processBlockDB b)
        return $! [] <$ r
    , nodeCommitQuery     = SimpleQuery $ \b -> do
        runBlockUpdate (headerHeight (blockHeader b)) persistedData $ processBlockDB b
        return []
    , nodeBlockGenerator  = \h _ _ _ -> do
        txs <- peekNTransactions mempool Nothing
        r   <- queryRO $ runEphemeralQ persistedData (transactionsToBlockDB h txs)
        case r of
          -- FIXME: This should not happen!
          Nothing -> error "Cannot generate block!"
          Just a  -> return (a, [])
    , nodeMempool         = mempool
    }



-- | Specification of node
data NodeDescription m alg a = NodeDescription
  { nodeValidationKey   :: !(Maybe (PrivValidator alg))
    -- ^ Private key of validator
  , nodeCommitCallback  :: !(Block alg a -> m ())
    -- ^ Callback called immediately after block was commit and user
    --   state in database is updated
  , nodeReadyCreateBlock :: !(Height -> Time -> m Bool)
    -- ^ It's called with height of blockchain and time of topmost block.
    --
    --   Called when node enters NEW_HEIGHT step. If it returns true
    --   it will continue to create new block. If False it will it
    --   call repeatedly (with timeout) until it becomes True
  }

-- | Specification of network
data BlockchainNet = BlockchainNet
  { bchNetwork      :: !NetworkAPI
  , bchLocalAddr    :: !NetAddr
  , bchInitialPeers :: ![NetAddr]
  }

runNode
  :: ( MonadDB m alg a, MonadMask m, MonadFork m, MonadLogger m, MonadTrace m, MonadTMMonitoring m, MonadFail m
     , Crypto alg, Show a, BlockData a
     )
  => Configuration app
  -> BlockchainNet
  -> NodeDescription m alg a
  -> NodeLogic m alg a
  -> m [m ()]
runNode cfg BlockchainNet{..} NodeDescription{..} NodeLogic{..} = do
  -- Build application state of consensus algorithm
  let appSt = AppState
        { appValidationFun  = nodeBlockValidation
        , appBlockGenerator = nodeBlockGenerator
        , appCommitQuery    = nodeCommitQuery
        , appCommitCallback = \b -> descendNamespace "mempool" $ do
            do before <- mempoolStats nodeMempool
               logger InfoS "Mempool before filtering" before
            filterMempool nodeMempool
            do after <- mempoolStats nodeMempool
               logger InfoS "Mempool after filtering" after
            nodeCommitCallback b
          --
        , appValidator        = nodeValidationKey
        }
  -- Networking
  appCh <- newAppChans (cfgConsensus cfg)
  return
    [ id $ descendNamespace "net"
         $ startPeerDispatcher (cfgNetwork cfg)
              bchNetwork bchLocalAddr bchInitialPeers appCh nodeMempool
    , id $ descendNamespace "consensus"
         $ runApplication (cfgConsensus cfg) nodeReadyCreateBlock appSt appCh
    , forever $ do
        MempoolInfo{..} <- mempoolStats nodeMempool
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
