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
  , NodeDescription(..)
  , BlockchainNet(..)
  , runNode
    -- * Running nodes
  , defCfg
  ) where

import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
import Codec.Serialise (Serialise)
import Data.Maybe      (isJust)

import Thundermint.Blockchain.Internal.Engine
import Thundermint.Blockchain.Interpretation
import Thundermint.Blockchain.Internal.Engine.Types
import Thundermint.Blockchain.Types
import Thundermint.Crypto
import Thundermint.Debug.Trace
import Thundermint.Logger
import Thundermint.Mock.KeyList
import Thundermint.Mock.Types
import Thundermint.P2P
import Thundermint.P2P.Network
import Thundermint.Store
import Thundermint.Store.STM
import qualified Thundermint.Store.Internal.Query as DB

import Thundermint.Control (MonadFork)

----------------------------------------------------------------
--
----------------------------------------------------------------

newtype DBT m a = DBT (ReaderT DB.Connection m a)
  deriving ( Functor, Applicative, Monad
           , MonadIO, MonadThrow, MonadCatch, MonadMask
           , MonadFork, MonadLogger, MonadTrace
           )

runDBT :: Monad m => DB.Connection -> DBT m a -> m a
runDBT c (DBT m) = runReaderT m c

instance MonadIO m => MonadDB (DBT m) where
  askConnection = DBT ask


----------------------------------------------------------------
--
----------------------------------------------------------------

data NodeLogic m alg a = NodeLogic
  { nodeBlockValidation :: Height -> a -> m Bool
    -- ^ Callback used for validation of blocks
  , nodeCommitQuery     :: Height -> a -> Query 'RW ()
    -- ^ Query for modifying user state.
  , nodeBlockGenerator  :: Height -> m a
    -- ^ Generator for a new block
  , nodeMempool         :: Mempool m alg (TX a)
    -- ^ Mempool of node
  }

logicFromFold
  :: forall m st alg a. (MonadDB m, MonadMask m, BlockData a, Ord (TX a), Crypto alg)
  => BlockFold st a
  -> m (BChState m st, NodeLogic m alg a)
logicFromFold transitions@BlockFold{..} = do
  -- Create blockchain state and rewind it
  let storage = blockStorage :: BlockStorage alg a
  hChain   <- queryRO $ blockchainHeight storage
  bchState <- newBChState transitions storage
  _        <- stateAtH bchState (succ hChain)
  -- Create mempool
  let checkTx tx = do
        st <- currentState bchState
        -- FIXME: We need real height here!
        return $ isJust $ processTx (Height 1) tx st
  mempool <- newMempool checkTx
  --
  return ( bchState
         , NodeLogic { nodeBlockValidation = \h a -> do
                         st <- stateAtH bchState h
                         return $ isJust $ processBlock h a st
                     , nodeCommitQuery     = \_ _ -> return ()
                     , nodeBlockGenerator  = \h -> do
                         st  <- stateAtH bchState h
                         txs <- peekNTransactions mempool Nothing
                         return $ transactionsToBlock h st txs
                     , nodeMempool         = mempool
                     }
         )

-- | Specification of node
data NodeDescription m alg a = NodeDescription
  { nodeValidationKey   :: Maybe (PrivValidator alg)
    -- ^ Private key of validator
  , nodeCommitCallback  :: Height -> m ()
    -- ^ Callback called immediately after block was commit and user
    --   state in database is updated
  }

-- | Specification of network
data BlockchainNet addr = BlockchainNet
  { bchNetwork      :: NetworkAPI addr
  , bchLocalAddr    :: addr
  , bchInitialPeers :: [addr]
  }

runNode
  :: ( MonadDB m, MonadMask m, MonadFork m, MonadLogger m, MonadTrace m
     , Crypto alg, Ord addr, Show addr, Serialise addr, Show a, BlockData a
     )
  => Configuration
  -> BlockchainNet addr
  -> NodeDescription m alg a
  -> NodeLogic m alg a
  -> m [m ()]
runNode cfg BlockchainNet{..} NodeDescription{..} NodeLogic{..} = do
  -- Create state of blockchain & Update it to current state of
  -- blockchain
  let nodeStorage = blockStorage
  hChain      <- queryRO $ blockchainHeight     nodeStorage
  Just valSet <- queryRO $ retrieveValidatorSet nodeStorage (succ hChain)
  -- Build application state of consensus algorithm
  let appSt = AppState
        { appStorage        = nodeStorage
        , appValidationFun  = nodeBlockValidation
        , appBlockGenerator = nodeBlockGenerator
        , appCommitCallback = \h -> setNamespace "mempool" $ do
            do before <- mempoolStats nodeMempool
               logger InfoS "Mempool before filtering" before
            filterMempool nodeMempool
            do after <- mempoolStats nodeMempool
               logger InfoS "Mempool after filtering" after
            nodeCommitCallback h
          --
        , appValidator        = nodeValidationKey
        , appNextValidatorSet = \_ _ -> return valSet
        }
  -- Networking
  appCh <- newAppChans
  return
    [ id $ setNamespace "net"
         $ startPeerDispatcher cfg bchNetwork bchLocalAddr bchInitialPeers appCh
                               nodeStorage
                               nodeMempool
    , id $ setNamespace "consensus"
         $ runApplication cfg appSt appCh
    ]
