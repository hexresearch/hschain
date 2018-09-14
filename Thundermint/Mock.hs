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
  , runNode
  , newBlockStorage
    -- * Running nodes
  , startNode
  , runNodeSet
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
import Thundermint.Mock.Store
import Thundermint.Mock.Types
import Thundermint.P2P
import Thundermint.P2P.Network
import Thundermint.Store

import Thundermint.Control (MonadFork, runConcurrently)

----------------------------------------------------------------
--
----------------------------------------------------------------
-- | Specification of node
data NodeDescription addr m alg st tx a = NodeDescription
  { nodeBlockChainLogic :: BlockFold st tx a
    -- ^ Logic of blockchain
  , nodeStorage         :: BlockStorage 'RW m alg a
    -- ^ Storage for commited blocks
  , nodeBchState        :: BChState m st
    -- ^ Current state of blockchain
  , nodeMempool         :: Mempool m alg tx
    -- ^ Mempool of node
  , nodeNetwork         :: NetworkAPI addr
    -- ^ Network API
  , nodeAddr            :: addr
    -- ^ Node address
  , nodeInitialPeers    :: [addr]
    -- ^ Initial peers
  , nodeValidationKey   :: Maybe (PrivValidator alg)
    -- ^ Private key of validator
  , nodeCommitCallback  :: Height -> m ()
    -- ^ Callback which is called on each commit
  }


runNode
  :: ( MonadIO m, MonadMask m, MonadFork m, MonadLogger m, MonadTrace m
     , Ord tx, Serialise tx
     , Crypto alg, Ord addr, Show addr, Serialise addr, Show a, LogBlock a
     , Serialise a)
  => Configuration
  -> NodeDescription addr m alg st tx a
  -> m [m ()]
runNode cfg NodeDescription{nodeBlockChainLogic=BlockFold{..}, ..} = do
  -- Create state of blockchain & Update it to current state of
  -- blockchain
  hChain      <- blockchainHeight     nodeStorage
  Just valSet <- retrieveValidatorSet nodeStorage (next hChain)
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
         $ startPeerDispatcher cfg nodeNetwork nodeAddr nodeInitialPeers appCh
                               (makeReadOnly   nodeStorage)
                               nodeMempool
    , id $ setNamespace "consensus"
         $ runApplication cfg appSt appCh
    ]



----------------------------------------------------------------
--
----------------------------------------------------------------

-- | Start node which will now run consensus algorithm
startNode
  :: (Ord addr, Show addr, Serialise addr, Crypto alg, Serialise a, Serialise tx, Show a, LogBlock a)
  => NetworkAPI addr
  -> addr
  -> [addr]
  -> AppState IO alg a
  -> Mempool IO alg tx
  -> IO ()
startNode net addr addrs appState@AppState{..} mempool = do
  -- Initialize logging
  logfile <- case appValidator of
    Just (PrivValidator pk) ->
      let Address nm = address $ publicKey pk
      in return $ "val-" ++ BC8.unpack (Base58.encodeBase58 Base58.bitcoinAlphabet nm)
    Nothing -> do
      w1 <- randomIO
      w2 <- randomIO
      return $ printf "node-%016x-%016x" (w1 :: Word64) (w2 :: Word64)
  scribe <- Katip.mkFileScribe ("logs/" ++ logfile) Katip.DebugS Katip.V2
  logenv <- Katip.registerScribe "log" scribe Katip.defaultScribeSettings
        =<< Katip.initLogEnv "TM" "DEV"
  flip finally (Katip.closeScribes logenv) $ runLoggerT "" logenv $ do
    appCh <- newAppChans
    let netRoutine = setNamespace "net"
                   $ startPeerDispatcher defCfg net addr addrs appCh
                       (hoistBlockStorageRO liftIO $ makeReadOnly appStorage)
                       (hoistMempool liftIO mempool)
    runConcurrently
      [ netRoutine
      , setNamespace "consensus"
          $ runApplication defCfg (hoistAppState liftIO appState) appCh
      ]

-- | Start set of nodes and return their corresponding storage. Will
--   return their storage after all nodes finish execution
runNodeSet
  :: (Ord addr, Show addr, Serialise addr, Crypto alg, Serialise a, Show a, Serialise tx, LogBlock a)
  => [( NetworkAPI addr, addr, [addr], AppState IO alg a, Mempool IO alg tx)]
  -> IO [BlockStorage 'RO IO alg a]
runNodeSet nodes = do
  withAsyncs [ startNode net addr addrs appSt mp
             | (net,addr,addrs,appSt,mp) <- nodes
             ]
    $ void . waitAny
  return [ makeReadOnly $ appStorage a | (_,_,_,a,_) <- nodes ]

withAsyncs :: [IO a] -> ([Async a] -> IO b) -> IO b
withAsyncs ios function
  = recur ([],ios)
  where
    recur (as,[])   = function (reverse as)
    recur (as,i:is) = withAsync i $ \a -> recur (a:as, is)
