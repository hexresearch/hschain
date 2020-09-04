{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE MultiWayIf                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE ViewPatterns               #-}
-- |
module HSChain.Mempool (
    -- * Mempool
    Mempool(..)
  , newMempool
  , nullMempool
  , mempoolSize
  , MempoolCursor(..)
  , MempoolHandle(..)
    -- * Mempool data structures
  , MempoolState(..)
  , emptyMempoolState
  , mempoolAddTX
  , mempoolFilterTX
  , mempoolRemoveTX
  , mempoolSelfTest
    -- * Logging information
  , MempoolInfo(..)
  ) where

import Control.Applicative
import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Reader
import Control.Monad.Trans.Maybe

import Data.Foldable
import Data.Maybe                (fromMaybe,mapMaybe)
import Data.List                 (nub,sort)
import Data.IORef
import Data.Map.Strict           (Map)
import Data.IntMap.Strict        (IntMap)
import qualified Data.Aeson         as JSON
import qualified Data.Aeson.TH      as JSON
import qualified Data.Map.Strict    as Map
import qualified Data.IntMap.Strict as IMap
import qualified Data.Set           as Set
import qualified Katip
import Text.Printf
import GHC.Generics              (Generic)

import HSChain.Crypto
import HSChain.Control.Util
import HSChain.Control.Channels

----------------------------------------------------------------
-- Mempool
----------------------------------------------------------------

-- | Dictionary of functions for working with mempool object. All
--   operations are performed asynchronously
data Mempool m tid tx = Mempool
  { getMempoolState       :: forall f. MonadIO f => f (MempoolState tid tx)
    -- ^ Get current state of mempool. It's updated after every
    --   command to mempool is processed.
  , removeTxByHashes      :: [tid] -> m ()
    -- ^ Remove transactions from mempool with given hashes.
  , startMempoolFiltering :: (tx -> m Bool) -> m ()
    -- ^ Command mempool to start filtering transactions asynchronously
  , mempoolHandle         :: MempoolHandle tid tx
    -- ^ Mempool handle for use in gossip. It's will to be removed
    --   if\/when gossip logic changes.
  , makeNewTxBroadcast    :: STM (Src tx)
    -- ^ Create broadcast channel for new transactions
  , mempoolInfo           :: forall f. MonadIO f => f MempoolInfo
  }

nullMempool :: Monad m => (tx -> tid) -> Mempool m tid tx
nullMempool toTID = Mempool
  { getMempoolState       = return $ emptyMempoolState toTID
  , removeTxByHashes      = \_ -> return ()
  , startMempoolFiltering = \_ -> return ()
  , mempoolHandle         = MempoolHandle $ return $ MempoolCursor
    { pushTxSync    = \_ -> return Nothing
    , pushTxAsync   = \_ -> return ()
    , advanceCursor = return Nothing
    }
  , makeNewTxBroadcast    = return (Src retry)
  , mempoolInfo           = return $ MempoolInfo 0 0 0 0
  }

-- | Compute current size of a mempool
mempoolSize :: MonadIO f => Mempool m tid tx -> f Int
mempoolSize m = IMap.size . mempFIFO <$> getMempoolState m

-- | Handle for working with mempool
data MempoolHandle tid tx = MempoolHandle
  { getMempoolCursor :: forall m. MonadIO m => m (MempoolCursor tid tx)
  }

-- | Cursor into mempool which is used for gossiping data
data MempoolCursor tid tx = MempoolCursor
  { pushTxSync      :: !(forall m. MonadIO m => tx -> m (Maybe tid))
    -- ^ Add transaction to the mempool. It's checked against current
    --   state of blockchain and if check fails it's immediately
    --   discarded. If transaction is accepted and not in mempool
    --   already its hash is computed and returned.
  , pushTxAsync     :: !(forall m. MonadIO m => tx -> m ())
    -- ^ Add transaction to the mempool wihtout waiting for validation result.
  , advanceCursor   :: !(forall m. MonadIO m => m (Maybe tx))
    -- ^ Take transaction from front and advance cursor. If cursor
    -- points at the end of queue nothing happens.
  }


-- | Create new mempool.
newMempool
  :: (CryptoHashable tx, Ord tid, MonadIO m)
  => (tx -> tid)
  -> (tx -> Bool)
  -> m (Mempool m tid tx, m ())
newMempool toTID basicValidation = do
  (sinkTx, mkTx) <- broadcastPair
  dict <- newMempoolDict toTID basicValidation sinkTx
  let mempool = Mempool
        { getMempoolState       = liftIO $ readTVarIO $ varMempool dict
        , removeTxByHashes      = atomicallyIO . writeTChan (chPushTx dict) . CmdRemoveTx
        , startMempoolFiltering = atomicallyIO . writeTChan (chPushTx dict) . CmdStartFiltering
        , mempoolHandle         = makeMempoolHandle dict
        , makeNewTxBroadcast    = mkTx
        , mempoolInfo           = liftIO $ do
            mempool'size      <- readIORef $ counterSize dict
            mempool'added     <- readIORef $ counterAdded dict
            mempool'discarded <- readIORef $ counterDiscarded dict
            mempool'filtered  <- readIORef $ counterFiltered dict
            return MempoolInfo{..}
        }
  return ( mempool , runMempool dict )


----------------------------------------------------------------
-- Mempool validation
----------------------------------------------------------------


data MempoolDict m tid tx = MempoolDict
  { basicValidation  :: tx -> Bool
  , dictToTID        :: tx -> tid
  , varMempool       :: TVar (MempoolState tid tx)
  , varValidate      :: TVar (tx -> m Bool)
  , varPending       :: TVar [tid]
  , chPushTx         :: TChan (MempoolCmd m tid tx)
  , broadcastTx      :: Sink tx
  , counterAdded     :: IORef Int
  , counterSize      :: IORef Int
  , counterDiscarded :: IORef Int
  , counterFiltered  :: IORef Int
  }

-- | Command to push new TX to mempool
data MempoolCmd m tid tx
  = CmdAddTx !(Maybe (MVar (Maybe tid)))
             !tx
    -- ^ Add transaction to mempool.
  | CmdRemoveTx [tid]
    -- ^ Remove given transactions from mempool.
  | CmdStartFiltering (tx -> m Bool)
    -- ^ Start Remove now invalid TXs from mempool using given
    --   predicate.
  | CmdAskForState !(MVar (MempoolState tid tx))
    -- ^ Request state from mempool. While it could be obtained from
    --   'varMempool' variable using this method ensures that previous
    --   command completed.

newMempoolDict :: MonadIO m => (tx -> tid) -> (tx -> Bool) -> Sink tx -> m (MempoolDict m tid tx)
newMempoolDict dictToTID basicValidation broadcastTx = liftIO $ do
  varMempool   <- newTVarIO $ emptyMempoolState dictToTID
  varValidate  <- newTVarIO $ const $ return False
  varPending   <- newTVarIO []
  chPushTx     <- newTChanIO
  counterAdded     <- newIORef 0
  counterSize      <- newIORef 0
  counterDiscarded <- newIORef 0
  counterFiltered  <- newIORef 0
  return MempoolDict{..}

makeMempoolHandle :: MempoolDict m tid tx -> MempoolHandle tid tx
makeMempoolHandle MempoolDict{..} = MempoolHandle
  { getMempoolCursor = do
      varN <- liftIO $ newTVarIO 0
      return MempoolCursor
        { pushTxSync    = \tx -> liftIO $ do
            reply <- newEmptyMVar
            atomicallyIO $ writeTChan chPushTx $ CmdAddTx (Just reply) tx
            takeMVar reply
        --
        , pushTxAsync =
            atomicallyIO . writeTChan chPushTx . CmdAddTx Nothing
        --
        , advanceCursor = atomicallyIO $ do
            mem <- readTVar varMempool
            n   <- readTVar varN
            case n `IMap.lookupGT` mempFIFO mem of
              Nothing           -> return Nothing
              Just (n', (_,tx)) -> do writeTVar varN $! n'
                                      return $ Just tx
        }
  }

runMempool
  :: (CryptoHashable tx, Ord tid, MonadIO m)
  => MempoolDict m tid tx
  -> m ()
runMempool dict@MempoolDict{..} = do
  let awaitPending = do
        txs <- readTVar varPending
        case splitAt 4 txs of
          ([],_)         -> retry
          (toCheck,rest) -> toCheck <$ writeTVar varPending rest
  --
  forever $ join $ atomicallyIO
    $  handleCommand      dict <$> readTChan chPushTx
   <|> handlePendingCheck dict <$> awaitPending


handleCommand
  :: (CryptoHashable tx, MonadIO m, Ord tid)
  => MempoolDict m tid tx -> MempoolCmd m tid tx -> m ()
handleCommand MempoolDict{..} = \case
  CmdAddTx retVar tx -> do
    let tid = dictToTID tx
    validation <- liftIO $ readTVarIO varValidate
    mmem <- mempoolAddTX basicValidation validation tx
        =<< liftIO (readTVarIO varMempool)
    case mmem of
      Nothing   -> inc counterDiscarded
      Just mem' -> do atomicallyIO $ writeTVar varMempool mem'
                      sinkIO broadcastTx tx
                      inc counterAdded >> size mem'
    liftIO $ forM_ retVar $ \v -> tryPutMVar v (tid <$ mmem)
  --
  CmdRemoveTx txs -> do
    nDel <- atomicallyIO $ do
      mem <- readTVar varMempool
      let mem' = mempoolRemoveTX txs mem
      writeTVar varMempool mem'
      return $ length mem - length mem'
    incBy counterFiltered nDel
  --
  CmdStartFiltering validation -> atomicallyIO $ do
    writeTVar varValidate validation
    st <- readTVar varMempool
    writeTVar varPending $ fmap fst $ toList $ mempFIFO st
  --
  CmdAskForState reply ->
    liftIO $ void $ tryPutMVar reply =<< readTVarIO varMempool
  where
    inc   ref   = liftIO $ atomicModifyIORef' ref (\i -> (i+1, ()))
    incBy ref n = liftIO $ atomicModifyIORef' ref (\i -> (i+n, ()))
    size st = liftIO $ atomicWriteIORef counterSize (length st)

handlePendingCheck
  :: (MonadIO m, Ord tid)
  => MempoolDict m tid tx -> [tid] -> m ()
handlePendingCheck MempoolDict{..} tidList = do
  m   <- liftIO $ readTVarIO varMempool
  val <- liftIO $ readTVarIO varValidate
  badTxs <- filterM (fmap not . val . snd)
          $ mapMaybe (\tid -> do (_,tx) <- Map.lookup tid (mempRevMap m)
                                 pure (tid,tx)
                     ) tidList
  atomicallyIO $ writeTVar varMempool $ mempoolRemoveTX (fst <$> badTxs) m
  liftIO $ atomicModifyIORef' counterFiltered (\i -> (i+length badTxs, ()))


----------------------------------------------------------------
-- Mempool data structures
----------------------------------------------------------------

-- | State of mempool
data MempoolState tid tx = MempoolState
  { mempToTID  :: tx -> tid
    -- ^ Convert transaction to transaction ID
  , mempFIFO   :: !(IntMap (tid, tx))
    -- ^ Transactions arranged in FIFO order
  , mempRevMap :: !(Map tid (Int, tx))
    -- ^ Reverse map of transactions
  , mempMaxN   :: !Int
    -- ^ Maximum key for FIFO
  }

instance Foldable (MempoolState tid) where
  length      = IMap.size . mempFIFO
  foldMap f m = foldMap (f . snd) (mempFIFO m)

emptyMempoolState :: (tx -> tid) -> MempoolState tid tx
emptyMempoolState toTID = MempoolState
  { mempToTID  = toTID
  , mempFIFO   = IMap.empty
  , mempRevMap = Map.empty
  , mempMaxN   = 0
  }

-- | Add transaction to mempool
mempoolAddTX
  :: (Monad m, Ord tid)
  => (tx -> Bool)
  -> (tx -> m Bool)
  -> tx
  -> MempoolState tid tx
  -> m (Maybe (MempoolState tid tx))
mempoolAddTX basicValidation validation tx MempoolState{..} =
  runMaybeT $ do
    -- Ignore TX that we already have
    guard $ tid `Map.notMember` mempRevMap
    guard $ basicValidation tx
    -- Validate TX
    lift (validation tx) >>= \case
      False -> empty
      True  -> do
        let n = mempMaxN + 1
        return $! MempoolState
          { mempFIFO   = IMap.insert n   (tid,tx) mempFIFO
          , mempRevMap = Map.insert  tid (n,  tx) mempRevMap
          , mempMaxN   = n
          , ..
          }
  where
    tid = mempToTID tx

-- | Remove all transaction from mepool that doesn't satisfy predicate
mempoolFilterTX
  :: (Monad m, Ord tid)
  => (tx -> m Bool)
  -> MempoolState tid tx
  -> m (MempoolState tid tx)
mempoolFilterTX validation MempoolState{..} = do
  invalidTx <- filterM (\(_,(_,tx)) -> not <$> validation tx)
             $ IMap.toList mempFIFO
  return MempoolState
    { mempFIFO   = foldl' (\m (i,_)       -> IMap.delete i m)  mempFIFO   invalidTx
    , mempRevMap = foldl' (\m (_,(tid,_)) -> Map.delete tid m) mempRevMap invalidTx
    , ..
    }

mempoolRemoveTX
  :: (Foldable f, Ord tid)
  => f tid
  -> MempoolState tid tx
  -> MempoolState tid tx
mempoolRemoveTX hashes MempoolState{..} = MempoolState
  { mempFIFO   = IMap.filter (\(tid,_) -> tid `Set.notMember` hashSet) mempFIFO
  , mempRevMap = Map.withoutKeys mempRevMap hashSet
  , ..
  }
  where
    hashSet = Set.fromList $ toList hashes

-- | Check mempool for internal consistency. Each returned string
--   is internal inconsistency
mempoolSelfTest
  :: (Show tx, Ord tx, Ord tid)
  => MempoolState tid tx -> [String]
mempoolSelfTest MempoolState{..} = concat
  [ [ printf "Mismatch in rev.map. nFIFO=%i nRev=%i" nFIFO nRev
    | nFIFO /= nRev
    ]
  , [ "Duplicate transactions present"
    | let txs = toList mempFIFO
    , nub txs /= txs
    ]
  , [ printf "RevMap is not reverse of FIFO.\nFIFO: %s\nRevMap: %s" (show lFifo) (show lRev)
    | lFifo /= lRev
    ]
  , [ printf "Max N less than max FIFO key. maxN=%i maxKey=%s" mempMaxN (show maxKey)
    | mempMaxN < fromMaybe 0 maxKey
    ]
  ]
  where
    nFIFO  = IMap.size mempFIFO
    nRev   = Map.size mempRevMap
    lFifo  = fmap snd <$> IMap.toAscList mempFIFO
    lRev   = sort $ toList mempRevMap
    maxKey = fst <$> IMap.lookupMax mempFIFO


----------------------------------------------------------------
-- Logging helpers
----------------------------------------------------------------


-- | Statistics about mempool
data MempoolInfo = MempoolInfo
  { mempool'size      :: !Int
  -- ^ Number of transactions currently in mempool
  , mempool'added     :: !Int
  -- ^ Number of transactions added to mempool since program start
  , mempool'discarded :: !Int
  -- ^ Number of transaction discarded immediately since program start
  , mempool'filtered  :: !Int
  -- ^ Number of transaction removed during filtering
  }
  deriving (Show,Generic)
JSON.deriveJSON JSON.defaultOptions
  { JSON.fieldLabelModifier = drop 8 } ''MempoolInfo

instance Katip.ToObject MempoolInfo
instance Katip.LogItem  MempoolInfo where
  payloadKeys Katip.V0 _ = Katip.SomeKeys []
  payloadKeys _        _ = Katip.AllKeys
