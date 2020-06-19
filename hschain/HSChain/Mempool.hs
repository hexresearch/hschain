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
    -- * Mempool data structures
  , MempoolState(..)
  , emptyMempoolState
  , mempoolAddTX
  , mempoolFilterTX
  , mempoolRemoveTX
  , mempoolSelfTest
  ) where

import Control.Applicative
import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Reader
import Control.Monad.Trans.Maybe

import Data.Foldable
import Data.Function
import Data.Maybe                (fromMaybe)
import Data.List                 (nub,sort)
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
import HSChain.Types.Merkle.Types
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
  }

hoistMempool :: (forall a. m a -> n a) -> Mempool m alg tx -> Mempool n alg tx
hoistMempool fun Mempool{..} = Mempool
  { peekNTransactions = fun peekNTransactions
  , filterMempool     = fun filterMempool
  , getMempoolCursor  = fun getMempoolCursor
  , txInMempool       = fun . txInMempool
  , mempoolStats      = fun mempoolStats
  , mempoolSize       = fun mempoolSize
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
  }


----------------------------------------------------------------
-- Real mempool implemenatation
----------------------------------------------------------------

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
  varMempool   <- newTVarIO emptyMempoolState
  varAdded     <- newTVarIO 0
  varDiscarded <- newTVarIO 0
  varFiltered  <- newTVarIO 0
  chPushTx     <- newChan
  return MempoolDict{..}

newMempool
  :: forall tx alg m. ( Show tx, Ord tx, Crypto alg, CryptoHashable tx, MonadIO m )
  => (tx -> m Bool)
  -> m (Mempool m alg tx, m ())
newMempool validation = do
  dict@MempoolDict{..} <- newMempoolDict :: m (MempoolDict m alg tx)
  return (
    Mempool
    -- TX manipulations
    { peekNTransactions =  map merkleValue . toList . mempFIFO
                       <$> liftIO (readTVarIO varMempool)
    , filterMempool     = liftIO $ writeChan chPushTx Filter
    , txInMempool       = \txHash -> liftIO $ do
        mem <- readTVarIO varMempool
        return $! txHash `Map.member` mempRevMap mem
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
                                    return $ Just $ merkleValue tx
          }
    }
    , mempoolThread validation dict
    )

-- | Check mempool for internal consistency. Each returned string
--   is internal inconsistency
mempoolSelfTest
  :: (Show a, Ord a)
  => MempoolState alg a -> [String]
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
    lFifo  = IMap.toAscList mempFIFO
    lRev   = sort $ toList mempRevMap
    maxKey = fst <$> IMap.lookupMax mempFIFO


----------------------------------------------------------------
-- Mempool state transitions
----------------------------------------------------------------

mempoolThread
  :: (CryptoHash alg, CryptoHashable tx, MonadIO m)
  => (tx -> m Bool)
  -> MempoolDict m alg tx
  -> m ()
mempoolThread validation MempoolDict{..} = forever $
  liftIO (readChan chPushTx) >>= \case
    Push retVar tx -> do
      let txNode = merkled tx
      mem  <- liftIO $ readTVarIO varMempool
      mmem <- mempoolAddTX validation txNode mem
      case mmem of
        Nothing   -> atomicallyIO $ do modifyTVar' varAdded     succ
                                       modifyTVar' varDiscarded succ
        Just mem' -> atomicallyIO $ do modifyTVar' varAdded     succ
                                       writeTVar   varMempool   mem'
      liftIO $ forM_ retVar $ \v -> tryPutMVar v (merkleHashed txNode <$ mmem)
    --
    Filter         -> do
      mem  <- liftIO $ readTVarIO varMempool
      mem' <- mempoolFilterTX validation mem
      atomicallyIO $ do
        let removed = ((-) `on` (IMap.size . mempFIFO)) mem mem'
        writeTVar varMempool mem
        modifyTVar' varFiltered (+ removed)


----------------------------------------------------------------
-- Mempool data structures
----------------------------------------------------------------

-- | State of mempool
data MempoolState alg tx = MempoolState
  { mempFIFO   :: !(IntMap (MerkleNode Identity alg tx))
    -- ^ Transactions arranged in FIFO order
  , mempRevMap :: !(Map (Hashed alg tx) (Int, MerkleNode Identity alg tx))
    -- ^ Reverse map of transactions
  , mempMaxN   :: !Int
    -- ^ Maximum key for FIFO
  }

instance Foldable (MempoolState alg) where
  foldMap f m = foldMap (f . merkleValue) (mempFIFO m)

emptyMempoolState :: MempoolState alg tx
emptyMempoolState = MempoolState
  { mempFIFO   = IMap.empty
  , mempRevMap = Map.empty
  , mempMaxN   = 0
  }

-- | Add transaction to mempool
mempoolAddTX
  :: (MonadIO m)
  => (tx -> m Bool)
  -> MerkleNode Identity alg tx
  -> MempoolState alg tx
  -> m (Maybe (MempoolState alg tx))
mempoolAddTX validation txNode MempoolState{..} = runMaybeT $ do
  -- Ignore TX that we already have
  guard $ txHash `Map.notMember` mempRevMap
  -- Validate TX
  lift (validation $ merkleValue txNode) >>= \case
    False -> empty
    True  -> do
      let n = mempMaxN + 1
      return $! MempoolState
        { mempFIFO   = IMap.insert n      txNode     mempFIFO
        , mempRevMap = Map.insert  txHash (n,txNode) mempRevMap
        , mempMaxN   = n
        }
  where
    txHash = merkleHashed txNode

-- | Remove all transaction from mepool that doesn't satisfy predicate
mempoolFilterTX
  :: (MonadIO m)
  => (tx -> m Bool)
  -> MempoolState alg tx
  -> m (MempoolState alg tx)
mempoolFilterTX validation MempoolState{..} = do
  invalidTx <- filterM (\(_,tx) -> not <$> validation (merkleValue tx))
             $ IMap.toList mempFIFO
  return MempoolState
    { mempFIFO   = foldl' (\m (i,_)  -> IMap.delete i m) mempFIFO invalidTx
    , mempRevMap = foldl' (\m (_,tx) -> Map.delete (merkleHashed tx) m) mempRevMap invalidTx
    , ..
    }

mempoolRemoveTX
  :: (Foldable f)
  => f (Hashed alg tx)
  -> MempoolState alg tx
  -> MempoolState alg tx
mempoolRemoveTX hashes MempoolState{..} = MempoolState
  { mempFIFO   = IMap.filter (\tx -> merkleHashed tx `Set.notMember` hashSet) mempFIFO
  , mempRevMap = Map.withoutKeys mempRevMap hashSet
  , ..
  }
  where
    hashSet = Set.fromList $ toList hashes
