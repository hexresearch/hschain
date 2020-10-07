-- |
-- Mempool API for the PoW blockchain.
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
module HSChain.PoW.Mempool
  ( -- * Data types
    -- ** Dictionaries of channels
    MempoolAPI(..)
  , MempoolCh(..)
    -- ** Messages
  , MempCmdConsensus(..)
  , MempCmdGossip(..)
    -- * Creation of mempool
  , startMempool
  ) where

import Control.Concurrent.STM
import Control.Monad
import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.Cont
import Control.Monad.Trans.Maybe
import Data.Either
import Data.Maybe
import Data.Foldable
import Data.Functor.Contravariant
import Data.Map.Strict (Map)
import Data.Set        (Set)
import qualified Data.Set        as Set
import qualified Data.Map.Strict as Map

import HSChain.Mempool
import HSChain.Control.Class
import HSChain.Control.Channels
import HSChain.Control.Util
import HSChain.PoW.Types
import HSChain.PoW.Consensus
import HSChain.PoW.P2P.Types

----------------------------------------------------------------
-- Data types
----------------------------------------------------------------

-- | Command sent to the mempool from the consensus engine
data MempCmdConsensus m b
  = MempHeadChange (BH b) (BH b) (StateView m b)
  -- ^ Blockchain head has been changed. We need to

-- | Command sent to the mempool from gossip
data MempCmdGossip b
  = MempPushTx     (Tx b)                     -- ^ Add transaction to mempool
  | MempPushTxSync (BlockingCall (Tx b) Bool) -- ^ Add Tx synchronously

-- | External API or interacting with mempool
data MempoolAPI m b = MempoolAPI
  { postTransaction :: Sink (Tx b)
    -- ^ Post transaction into mempool. This function is black
    --   hole. There's no way to learn whether transaction was
    --   accepted or rejected
  , postTransactionSync :: Sink (BlockingCall (Tx b) Bool)
    -- ^ Post transaction into mempool. This function blocks until
    --   transaction is accepted or rejected from mempool. Returns
    --   True if transaction is accepted.
    --
    --   Note that acceptance to mempool doesn't even guarantee that
    --   transaction will be eventually mined. And certainly not that
    --   it will be mined promptly.
  , mempoolUpdates  :: STM (Src (BH b, StateView m b, [Tx b]))
    -- ^ Channel for receiving updates to mempool when
  , mempoolContent  :: STM [Tx b]
    -- ^ Obtain current content of mempool
  , mempoolState    :: STM (MempoolState (TxID b) (Tx b))
    -- ^ State of the mempool
  }

-- | Internal API for sending messages from consensus engine to the
--   mempool
data MempoolCh m b = MempoolCh
  { mempoolConsensusCh :: Sink (MempCmdConsensus m b)
    -- ^ Channel for sending updates from consensus engine
  , mempoolAnnounces   :: STM (Src (MsgTX b))
  }

-- | Collection of channels and TVars for mempool thread.
data InternalCh m b = InternalCh
  { srcMempoolCns     :: Src (MempCmdConsensus m b)
    -- ^ Messages from consensus engine
  , srcMempoolGossip  :: Src (MempCmdGossip b)
    -- ^ Messages from gossip
  , bcastNewTx        :: Sink (Tx b)
    -- ^ Sink for valid transaction which just learned about,
  , bcastMempoolState :: Sink (BH b, StateView m b, [Tx b])
    -- ^ Broadcast change of blockchain head and corresponding update of
  , pendingFiltering  :: TVar [TxID b]
    -- ^ Messages which we need to be rechecked
  , currentMempool    :: TVar (MempoolState (TxID b) (Tx b))
  }


-- | Start new thread running mempool
startMempool
  :: (MonadIO m, MonadFork m, MonadMask m, BlockData b)
  => BlockDB   m b
  -- ^ Block database
  -> StateView m b
  -- ^ Current view on blockchain state. Will be used for transaction
  --   validation until state will be changed
  -> ContT r m (MempoolAPI m b, MempoolCh m b)
startMempool db state = do
  (mempoolConsensusCh, srcMempoolCns)    <- queuePair
  (bcastMempoolState,  mempoolUpdates)   <- broadcastPair
  (bcastNewTx,         mkSrcNewTx)       <- broadcastPair
  (sinkGossip,         srcMempoolGossip) <- queuePair
  pendingFiltering <- liftIO $ newTVarIO []
  let mempool = emptyMempoolState txID
  currentMempool <- liftIO $ newTVarIO mempool
  --
  cforkLinked $ runMempool db InternalCh{..} MempoolDict
    { stateView = state
    , ..
    }
  return ( MempoolAPI{ postTransaction     = contramap MempPushTx     sinkGossip
                     , postTransactionSync = contramap MempPushTxSync sinkGossip
                     , mempoolContent      = toList <$> readTVar currentMempool
                     , mempoolState        = readTVar currentMempool
                     , ..
                     }
         , MempoolCh{ mempoolAnnounces = fmap AnnNewTX <$> mkSrcNewTx
                    , ..
                    }
         )


----------------------------------------------------------------

-- State of mempool. It contains both mempool contrnent and current
-- state of blockchain which is used for transaction validation
data MempoolDict m b = MempoolDict
  { mempool   :: !(MempoolState (TxID b) (Tx b))
  , stateView :: !(StateView m b)
  }

runMempool
  :: (MonadIO m, BlockData b)
  => BlockDB m b
  -> InternalCh m b
  -> MempoolDict m b
  -> m ()
runMempool db ch@InternalCh{..} st0 = iterateSTM st0 $ \s -> store <$> asum
  [ handleConsensus db ch s <$> await srcMempoolCns
  , handleGossip       ch s <$> await srcMempoolGossip
  , handlePending      ch s
  ]
  where
    store action = do m <- action
                      atomicallyIO $ writeTVar currentMempool (mempool m)
                      return m


----------------------------------------
-- Change of blockchain head

handleConsensus
  :: forall m b. (MonadIO m, BlockData b)
  => BlockDB m b
  -> InternalCh m b
  -> MempoolDict m b
  -> MempCmdConsensus m b
  -> m (MempoolDict m b)
handleConsensus db@BlockDB{..} InternalCh{..} MempoolDict{..} = \case
  MempHeadChange bhFrom bhTo state -> do
    TxChange{..} <- computeMempoolChange db bhFrom bhTo
    let add m tx = fromMaybe m
                <$> mempoolAddTX
                      (isRight . validateTxContextFree @b)
                      (fmap isRight . checkTx state)
                      tx m
    mem' <- foldM add (mempoolRemoveTX toRemove mempool) toAdd
    atomicallyIO $ writeTVar pendingFiltering $ Map.keys $ mempRevMap mem'
    sinkIO bcastMempoolState (bhTo, state, toList mem')
    return MempoolDict{ mempool   = mem'
                      , stateView = state
                      }

-- Changes to a transaction set in a mempool. It contains transactions
-- which should be readded to mempool because of block rollback and
-- IDs of ones that should be removed
data TxChange b = TxChange
  { toAdd    :: !(Map (TxID b) (Tx b))
  , toRemove :: !(Set (TxID b))
  }

computeMempoolChange
  :: (BlockData b, Monad m) => BlockDB m b -> BH b -> BH b -> m (TxChange b)
computeMempoolChange db bhFrom bhTo
  =  reduceTxChange
 <$> traverseBlockIndexM rollback update bhFrom bhTo (TxChange mempty mempty)
  where
    rollback bh TxChange{..} = do
      txs <- retrieveTxList db (bhBID bh)
      return TxChange { toAdd = toAdd <> Map.fromList [(txID tx, tx) | tx <- txs]
                      , ..
                      }
    update bh TxChange{..} = do
      tids <- retrieveTidList db (bhBID bh)
      return TxChange{ toRemove = toRemove <> Set.fromList tids
                     , ..
                     }

reduceTxChange :: (Ord (TxID b)) => TxChange b -> TxChange b
reduceTxChange TxChange{..} = TxChange
  { toAdd    = Map.withoutKeys toAdd toRemove
  , toRemove = toRemove `Set.difference` Map.keysSet toAdd
  }


retrieveTxList
  :: (BlockData b, Functor m)
  => BlockDB m b -> BlockID b -> m [Tx b]
retrieveTxList db bid =
  maybe [] blockTransactions <$> retrieveBlock db bid

retrieveTidList
  :: (BlockData b, Functor m)
  => BlockDB m b -> BlockID b -> m [TxID b]
retrieveTidList db bid =
  maybe [] (map txID . blockTransactions) <$> retrieveBlock db bid


----------------------------------------
-- Other changes to mempool

handleGossip
  :: forall b m. (MonadIO m, BlockData b)
  => InternalCh  m b
  -> MempoolDict m b
  -> MempCmdGossip b
  -> m (MempoolDict m b)
handleGossip InternalCh{..} st@MempoolDict{..} = \case
  MempPushTx     tx   -> fromMaybe st <$> addTx tx
  MempPushTxSync call -> handleBlockingCall call $ \tx ->
    addTx tx >>= \case
      Nothing  -> return (False, st )
      Just st' -> return (True,  st')
  where
    addTx tx = runMaybeT $ do
      st' <- MaybeT
           $ mempoolAddTX (isRight . validateTxContextFree @b)
                          (fmap isRight . checkTx stateView)
                          tx
                          mempool
      sinkIO bcastNewTx tx
      return MempoolDict { mempool = st', .. }

handlePending
  :: (MonadIO m, BlockData b)
  => InternalCh m b
  -> MempoolDict m b
  -> STM (m (MempoolDict m b))
handlePending InternalCh{..} MempoolDict{..}
  = doFilter <$> awaitPending
  where
    doFilter tidList = do
      badTxs <- filterM (fmap isLeft . checkTx stateView . snd)
              $ mapMaybe (\tid -> do (_,tx) <- Map.lookup tid (mempRevMap mempool)
                                     pure (tid,tx)
                         ) tidList
      return $ MempoolDict { mempool = mempoolRemoveTX (fst <$> badTxs) mempool
                           , ..
                           }
    --
    awaitPending = do
        txs <- readTVar pendingFiltering
        case splitAt 10 txs of
          ([],_)         -> retry
          (toCheck,rest) -> toCheck <$ writeTVar pendingFiltering rest



----------------------------------------------------------------
--
----------------------------------------------------------------

iterateSTM :: (MonadIO m) => s -> (s -> STM (m s)) -> m x
iterateSTM s0 step = loop s0
  where
    loop = loop <=< join . atomicallyIO . step
