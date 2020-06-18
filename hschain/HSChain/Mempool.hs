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
    MempoolCursor(..)
  , Mempool(..)
  , MempoolInfo(..)
  , hoistMempool
    -- * Concrete mempool implementations
  , nullMempool
  , newMempool
  ) where

import Control.Applicative
import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Reader
import Control.Monad.Trans.Maybe

import Data.Foldable
import Data.Maybe                (fromMaybe)
import Data.List                 (nub,sort)
import Data.Tuple
import Data.Map.Strict           (Map)
import Data.IntMap.Strict        (IntMap)
import Data.Set                  (Set)
import qualified Data.Aeson         as JSON
import qualified Data.Aeson.TH      as JSON
import qualified Data.Set           as Set
import qualified Data.Map.Strict    as Map
import qualified Data.IntMap.Strict as IMap
import qualified Katip
import Text.Printf
import GHC.Generics              (Generic)

import HSChain.Crypto
import HSChain.Control.Util


----------------------------------------------------------------
-- Mempool
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

-- | Cursor into mempool which is used for gossiping data
data MempoolCursor alg tx = MempoolCursor
  { pushTxSync      :: !(forall m. MonadIO m => tx -> m (Maybe (Hashed alg tx)))
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

-- | Mempool which is used for storing transactions before they're
--   added into blockchain. Transactions are stored in FIFO manner
data Mempool m alg tx = Mempool
  { peekNTransactions :: !(m [tx])
    -- ^ Return transactions in mempool as lazy list. This operation
    --   does not alter mempool state
  , filterMempool     :: !(m ())
    -- ^ Remove transactions that are no longer valid from mempool
  , getMempoolCursor  :: !(m (MempoolCursor alg tx))
    -- ^ Get cursor pointing to be
  , txInMempool       :: !(Hashed alg tx -> m Bool)
    -- ^ Checks whether transaction is mempool
  , mempoolStats      :: !(m MempoolInfo)
    -- ^ Number of elements in mempool
  , mempoolSize       :: !(m Int)
    -- ^ Number of transactions in mempool
  , mempoolSelfTest   :: !(m [String])
    -- ^ Check mempool for internal consistency. Each returned string
    --   is internal inconsistency
  }

hoistMempool :: (forall a. m a -> n a) -> Mempool m alg tx -> Mempool n alg tx
hoistMempool fun Mempool{..} = Mempool
  { peekNTransactions = fun peekNTransactions
  , filterMempool     = fun filterMempool
  , getMempoolCursor  = fun getMempoolCursor
  , txInMempool       = fun . txInMempool
  , mempoolStats      = fun mempoolStats
  , mempoolSize       = fun mempoolSize
  , mempoolSelfTest   = fun mempoolSelfTest
  }


-- | Mempool which does nothing. It doesn't contain any transactions
--   and discard every transaction is pushed into it. Useful if one
--   doesn't need any mempool.
nullMempool :: (Monad m) => Mempool m alg tx
nullMempool = Mempool
  { peekNTransactions = return []
  , filterMempool     = return ()
  , mempoolStats      = return $ MempoolInfo 0 0 0 0
  , mempoolSize       = return 0
  , txInMempool       = const (return False)
  , getMempoolCursor  = return MempoolCursor
      { pushTxSync    = const $ return Nothing
      , pushTxAsync   = const $ return ()
      , advanceCursor = return Nothing
      }
  , mempoolSelfTest   = return []
  }


----------------------------------------------------------------
-- Real mempool implemenatation
----------------------------------------------------------------


data MempoolState alg tx = MempoolState
  { mempFIFO   :: !(IntMap tx)
  , mempRevMap :: !(Map tx Int)
  , mempTxSet  :: !(Set (Hashed alg tx))
  , mempMaxN   :: !Int
  }

data MempoolDict m alg tx = MempoolDict
  { varMempool   :: TVar (MempoolState alg tx)
  -- Counters for tracking number of transactions
  , varAdded     :: TVar Int
  , varDiscarded :: TVar Int
  , varFiltered  :: TVar Int
  -- Communication channel
  , chPushTx     :: Chan (Push alg tx)
  }

-- Command to push new TX to mempool
data Push alg tx
  = Push !(Maybe (MVar (Maybe (Hashed alg tx))))
         !tx
  | Filter

newMempoolDict :: MonadIO m => m (MempoolDict n alg tx)
newMempoolDict = liftIO $ do
  varMempool   <- newTVarIO MempoolState
    { mempFIFO   = IMap.empty
    , mempRevMap = Map.empty
    , mempTxSet  = Set.empty
    , mempMaxN   = 0
    }
  varAdded     <- newTVarIO 0
  varDiscarded <- newTVarIO 0
  varFiltered  <- newTVarIO 0
  chPushTx     <- newChan
  return MempoolDict{..}

newMempool
  :: ( Show tx, Ord tx, Crypto alg, CryptoHashable tx, MonadIO m )
  => (tx -> m Bool)
  -> m (Mempool m alg tx, m ())
newMempool validation = do
  dict@MempoolDict{..} <- newMempoolDict
  return (
    Mempool
    -- TX manipulations
    { peekNTransactions = toList . mempFIFO <$> liftIO (readTVarIO varMempool)
    , filterMempool     = liftIO $ writeChan chPushTx Filter
    , txInMempool       = \txHash -> liftIO $ do
        mem <- readTVarIO varMempool
        return $! txHash `Set.member` mempTxSet mem
    -- Stats
    , mempoolStats = liftIO $ atomically $ do
        mempool'size      <- IMap.size . mempFIFO <$> readTVar varMempool
        mempool'added     <- readTVar varAdded
        mempool'discarded <- readTVar varDiscarded
        mempool'filtered  <- readTVar varFiltered
        return MempoolInfo{..}
    , mempoolSize = liftIO $ do m <- readTVarIO varMempool
                                return $! IMap.size $ mempFIFO m
    -- Cursor
    , getMempoolCursor = liftIO $ atomically $ do
        varN <- newTVar 0
        return MempoolCursor
          { pushTxSync = \tx -> liftIO $ do
              reply <- newEmptyMVar
              writeChan chPushTx $ Push (Just reply) tx
              takeMVar reply
          , pushTxAsync = liftIO . writeChan chPushTx . Push Nothing
            --
          , advanceCursor = liftIO $ atomically $ do
              mem <- readTVar varMempool
              n   <- readTVar varN
              case n `IMap.lookupGT` mempFIFO mem of
                Nothing       -> return Nothing
                Just (n', tx) -> do writeTVar varN $! n'
                                    return (Just tx)
          }
    --
    , mempoolSelfTest = selfTest dict
    }
    , mempoolThread validation dict
    )

selfTest
  :: (MonadIO m, Show a, Ord a, CryptoHash alg, CryptoHashable a)
  => MempoolDict m alg a -> m [String]
selfTest MempoolDict{..} = liftIO $ atomically $ do
  mem <- readTVar varMempool
  let fifo = mempFIFO   mem
      rev  = mempRevMap mem
      tx   = mempTxSet  mem
      maxN = mempMaxN   mem
  let nFIFO  = IMap.size fifo
      nRev   = Map.size rev
      nTX    = Set.size tx
      lFifo  = IMap.toAscList fifo
      lRev   = sort $ swap <$> Map.toList rev
      maxKey = fst <$> IMap.lookupMax fifo
  --
  return $ concat
    [ [ printf "Mismatch in rev.map. nFIFO=%i nRev=%i" nFIFO nRev
      | nFIFO /= nRev
      ]
    , [ printf "Mismatch in tx.set. nFIFO=%i nTX=%i" nFIFO nTX
      | nFIFO /= nTX
      ]
    , [ unlines [ "True TX / cached set"
                , show trueTX
                , show tx
                ]
      | let trueTX = Set.fromList (hashed <$> toList fifo)
      , trueTX /= tx
      ]
    , [ "Duplicate transactions present"
      | let txs = toList fifo
      , nub txs /= txs
      ]
    , [ printf "RevMap is not reverse of FIFO.\nFIFO: %s\nRevMap: %s" (show lFifo) (show lRev)
      | lFifo /= lRev
      ]
    , [ printf "Max N less than max FIFO key. maxN=%i maxKey=%s" maxN (show maxKey)
      | maxN < fromMaybe 0 maxKey
      ]
    ]


----------------------------------------------------------------
-- Mempool state transitions
----------------------------------------------------------------

mempoolThread
  :: (Ord tx, CryptoHash alg, CryptoHashable tx, MonadIO m)
  => (tx -> m Bool)
  -> MempoolDict m alg tx
  -> m ()
mempoolThread validation dict = forever $
  liftIO (readChan (chPushTx dict)) >>= \case
    Push retVar tx -> handlePush   validation dict retVar tx
    Filter         -> handleFilter validation dict

handlePush
  :: (MonadIO m, Ord tx, CryptoHash alg, CryptoHashable tx)
  => (tx -> m Bool)
  -> MempoolDict m alg tx
  -> Maybe (MVar (Maybe (Hashed alg tx)))
  -> tx
  -> m ()
handlePush validation MempoolDict{..} retVar tx = do
  MempoolState{..} <- liftIO $ readTVarIO varMempool
  res <- runMaybeT $ do
    -- Abort if tx is already in mempool
    when (tx `Map.member` mempRevMap) discard
    -- Validate TX
    lift (validation tx) >>= \case
      False -> discard
      True  -> MaybeT $ atomicallyIO $ do
        let txHash = hashed tx
            n      = mempMaxN + 1
        modifyTVar' varAdded succ
        writeTVar varMempool $! MempoolState
          { mempFIFO   = IMap.insert n  tx mempFIFO
          , mempRevMap = Map.insert  tx n  mempRevMap
          , mempTxSet  = Set.insert txHash mempTxSet
          , mempMaxN   = n
          }
        return $ Just txHash
  liftIO $ forM_ retVar $ \v -> tryPutMVar v res
  where
    discardSTM = do modifyTVar' varAdded     succ
                    modifyTVar' varDiscarded succ
    discard    = atomicallyIO discardSTM *> empty


-- Filtering of transactions is tricky! Validation function is
-- monadic so we cannot call it inside atomically block. And when
-- we call it outside atomically block content of maps could change!
--
-- In order to overcome this problem we make following assumtions:
--
--  * All transactions added to mempool were at some point valid
--  * If transaction become invalid for whatever reason it stays
--    invalid forever
--
--  So basic algorithm is:
--   1) We take all transactions in mempool
--   2) Select invalid ones
--   3) Remove them in separate atomically block
--
--  This way any new transactions which were added during checking
--  are not affected.
handleFilter
  :: (MonadIO m, Ord a, CryptoHash alg, CryptoHashable a)
  => (a -> m Bool) -> MempoolDict m alg a -> m ()
handleFilter validation MempoolDict{..} = do
  MempoolState{..} <- liftIO $ readTVarIO varMempool
  -- Invalid transactions
  invalidTx <- filterM (\(_,tx) -> not <$> validation tx)
             $ IMap.toList mempFIFO
  let !mem = MempoolState
        { mempFIFO   = foldl' (\m (i,_)  -> IMap.delete i m) mempFIFO   invalidTx
        , mempRevMap = foldl' (\m (_,tx) -> Map.delete tx m) mempRevMap invalidTx
        , mempTxSet  = foldl' (\s (_,tx) -> Set.delete (hashed tx) s) mempTxSet invalidTx
        , ..
        }
  -- Remove invalid transactions
  atomicallyIO $ do
    writeTVar varMempool mem
    modifyTVar' varFiltered (+ length invalidTx)
