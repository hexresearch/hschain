{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
-- |
-- Helper function for running mock network of thundermint nodes
module Thundermint.Mock (
    -- * Validators
    makePrivateValidators
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

import Control.Monad
import Control.Monad.Catch
import Control.Monad.IO.Class
import Text.Printf

import Control.Concurrent.Async hiding (runConcurrently)

import Codec.Serialise (Serialise)
import Data.Maybe      (isJust)
import Data.Word       (Word64)
import System.Random   (randomIO)

import qualified Data.ByteString.Base58 as Base58
import qualified Data.ByteString.Char8  as BC8
import qualified Katip

import Thundermint.Blockchain.App
import Thundermint.Blockchain.Interpretation
import Thundermint.Blockchain.Types
import Thundermint.Consensus.Types
import Thundermint.Crypto
import Thundermint.Debug.Trace
import Thundermint.Logger
import Thundermint.Mock.KeyList
import Thundermint.Mock.Types
import Thundermint.P2P
import Thundermint.P2P.Network
import Thundermint.Store

import Thundermint.Control (MonadFork, runConcurrently)

----------------------------------------------------------------
--
----------------------------------------------------------------

-- | Specification of node
data NodeDescription m alg st tx a = NodeDescription
  { nodeBlockChainLogic :: BlockFold st tx a
    -- ^ Logic of blockchain
  , nodeStorage         :: BlockStorage 'RW m alg a
    -- ^ Storage for commited blocks
  , nodeBchState        :: BChState m st
    -- ^ Current state of blockchain
  , nodeMempool         :: Mempool m alg tx
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
  :: ( MonadMask m, MonadFork m, MonadLogger m, MonadTrace m
     , Serialise tx
     , Crypto alg, Ord addr, Show addr, Serialise addr, Show a, LogBlock a
     , Serialise a)
  => Configuration
  -> BlockchainNet addr
  -> NodeDescription m alg st tx a
  -> m [m ()]
runNode cfg BlockchainNet{..} NodeDescription{nodeBlockChainLogic=BlockFold{..}, ..} = do
  -- Create state of blockchain & Update it to current state of
  -- blockchain
  hChain      <- blockchainHeight     nodeStorage
  Just valSet <- retrieveValidatorSet nodeStorage (succ hChain)
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
                               (makeReadOnly   nodeStorage)
                               nodeMempool
    , id $ setNamespace "consensus"
         $ runApplication cfg appSt appCh
    ]
