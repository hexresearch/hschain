{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
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

-- | Specification of node
data NodeDescription m alg st a = NodeDescription
  { nodeBlockChainLogic :: BlockFold st a
    -- ^ Logic of blockchain
  , nodeBchState        :: BChState m st
    -- ^ Current state of blockchain
  , nodeMempool         :: Mempool m alg (TX a)
    -- ^ Mempool of node
  , nodeValidationKey   :: Maybe (PrivValidator alg)
    -- ^ Private key of validator
  , nodeCommitCallback  :: Height -> m ()
    -- ^ Callback which is called on each commit
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
  -> NodeDescription m alg st a
  -> m [m ()]
runNode cfg BlockchainNet{..} NodeDescription{nodeBlockChainLogic=BlockFold{..}, ..} = do
  -- Create state of blockchain & Update it to current state of
  -- blockchain
  let nodeStorage = blockStorage
  hChain      <- queryRO $ blockchainHeight     nodeStorage
  Just valSet <- queryRO $ retrieveValidatorSet nodeStorage (succ hChain)
  -- Build application state of consensus algorithm
  let appSt = AppState
        { appStorage     = nodeStorage
          --
        , appValidationFun = \h a -> do
            st <- stateAtH nodeBchState h
            return $ isJust $ processBlock h a st
          --
        , appBlockGenerator = \h -> do
            st  <- stateAtH nodeBchState h
            txs <- peekNTransactions nodeMempool Nothing
            return $ transactionsToBlock h st txs
          --
        , appCommitCallback = \h -> setNamespace "mempool" $ do
            before <- mempoolStats nodeMempool
            logger InfoS "Mempool before filtering" before
            filterMempool nodeMempool
            after  <- mempoolStats nodeMempool
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
