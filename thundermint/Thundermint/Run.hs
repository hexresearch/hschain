{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
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

import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Class
import Codec.Serialise (Serialise)
import Data.Maybe      (isJust)

import Thundermint.Blockchain.Internal.Engine
import Thundermint.Blockchain.Interpretation
import Thundermint.Blockchain.Internal.Engine.Types
import Thundermint.Blockchain.Types
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
import qualified Thundermint.Store.Internal.Query as DB

import Thundermint.Control (MonadFork)

----------------------------------------------------------------
--
----------------------------------------------------------------

newtype DBT rw alg a m x = DBT (ReaderT (DB.Connection rw alg a) m x)
  deriving ( Functor, Applicative, Monad
           , MonadIO, MonadThrow, MonadCatch, MonadMask
           , MonadFork, MonadLogger, MonadTrace
           )

instance MonadTrans (DBT rw alg a) where
  lift = DBT . lift

dbtRO :: DBT 'RO alg a m x -> DBT rw alg a m x
dbtRO (DBT m) = DBT (withReaderT DB.connectionRO m)

runDBT :: Monad m => DB.Connection rw alg a -> DBT rw alg a m x -> m x
runDBT c (DBT m) = runReaderT m c

instance MonadIO m => MonadReadDB (DBT rw alg a m) alg a where
  askConnectionRO = DB.connectionRO <$> DBT ask
instance MonadIO m => MonadDB (DBT 'RW alg a m) alg a where
  askConnectionRW = DBT ask


----------------------------------------------------------------
--
----------------------------------------------------------------

data NodeLogic m alg a = NodeLogic
  { nodeBlockValidation :: !(Block alg a -> m Bool)
    -- ^ Callback used for validation of blocks
  , nodeCommitQuery     :: !(CommitCallback m alg a)
    -- ^ Query for modifying user state.
  , nodeBlockGenerator  :: !(Height -> m a)
    -- ^ Generator for a new block
  , nodeMempool         :: !(Mempool m alg (TX a))
    -- ^ Mempool of node
  }

logicFromFold
  :: (MonadDB m alg a, MonadMask m, BlockData a, Ord (TX a), Crypto alg)
  => BlockFold st a
  -> m (BChState m st, NodeLogic m alg a)
logicFromFold transitions@BlockFold{..} = do
  hChain   <- queryRO blockchainHeight
  bchState <- newBChState transitions
  _        <- stateAtH bchState (succ hChain)
  -- Create mempool
  let checkTx tx = do
        st <- currentState bchState
        -- FIXME: We need real height here!
        return $ isJust $ processTx (Height 1) tx st
  mempool <- newMempool checkTx
  --
  return ( bchState
         , NodeLogic { nodeBlockValidation = \b -> do
                         let h = headerHeight $ blockHeader b
                         st <- stateAtH bchState h
                         return $ isJust $ processBlock h (blockData b) st
                     , nodeCommitQuery     = SimpleQuery $ \b -> do
                         Just vset <- retrieveValidatorSet $ headerHeight $ blockHeader b
                         return vset
                     , nodeBlockGenerator  = \h -> do
                         st  <- stateAtH bchState h
                         txs <- peekNTransactions mempool Nothing
                         return $ transactionsToBlock h st txs
                     , nodeMempool         = mempool
                     }
         )

logicFromPersistent
  :: (MonadDB m alg a, MonadMask m, BlockData a, Ord (TX a), Crypto alg, FloatOut dct)
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
        return $! isJust r
    , nodeCommitQuery     = SimpleQuery $ \b -> do
        runBlockUpdate (headerHeight (blockHeader b)) persistedData $ processBlockDB b
        Just vset <- retrieveValidatorSet $ headerHeight $ blockHeader b
        return vset
    , nodeBlockGenerator  = \h -> do
        txs <- peekNTransactions mempool Nothing
        r   <- queryRO $ runEphemeralQ persistedData (transactionsToBlockDB h txs)
        case r of
          -- FIXME: This should not happen!
          Nothing -> error "Cannot generate block!"
          Just a  -> return a
    , nodeMempool         = mempool
    }



-- | Specification of node
data NodeDescription m alg a = NodeDescription
  { nodeValidationKey   :: !(Maybe (PrivValidator alg))
    -- ^ Private key of validator
  , nodeCommitCallback  :: !(Block alg a -> m ())
    -- ^ Callback called immediately after block was commit and user
    --   state in database is updated
  , nodeMonitoring      :: !(PrometheusGauges m)
  }

-- | Specification of network
data BlockchainNet addr = BlockchainNet
  { bchNetwork      :: !(NetworkAPI addr)
  , bchLocalAddr    :: !addr
  , bchInitialPeers :: ![addr]
  }

runNode
  :: ( MonadDB m alg a, MonadMask m, MonadFork m, MonadLogger m, MonadTrace m
     , Crypto alg, Ord addr, Show addr, Serialise addr, Show a, BlockData a
     )
  => Configuration app
  -> BlockchainNet addr
  -> NodeDescription m alg a
  -> NodeLogic m alg a
  -> m [m ()]
runNode cfg BlockchainNet{..} NodeDescription{..} NodeLogic{..} = do
  -- Build application state of consensus algorithm
  let appSt = AppState
        { appValidationFun  = nodeBlockValidation
        , appBlockGenerator = nodeBlockGenerator
        , appCommitQuery    = nodeCommitQuery
        , appCommitCallback = \b -> setNamespace "mempool" $ do
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
  appCh <- newAppChans nodeMonitoring
  return
    [ id $ setNamespace "net"
         $ startPeerDispatcher (cfgNetwork cfg)
              bchNetwork bchLocalAddr bchInitialPeers appCh nodeMempool
    , id $ setNamespace "consensus"
         $ runApplication (cfgConsensus cfg) appSt appCh
    ]
