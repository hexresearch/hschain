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
  , hoistMempoolCursor
  , hoistMempool
  , nullMempool
  , newMempool
  ) where

import Control.Applicative
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

import HSChain.Control.Class          (MonadFork)
import HSChain.Crypto


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
data MempoolCursor m alg tx = MempoolCursor
  { pushTransaction :: !(tx -> m (Maybe (Hashed alg tx)))
    -- ^ Add transaction to the mempool. It's preliminary checked and
    --   if check fails it immediately discarded. If transaction is
    --   accepted its hash is computed and returned
  , advanceCursor   :: !(m (Maybe tx))
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
  , getMempoolCursor  :: !(m (MempoolCursor m alg tx))
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

hoistMempoolCursor :: (forall a. m a -> n a) -> MempoolCursor m alg tx -> MempoolCursor n alg tx
hoistMempoolCursor fun MempoolCursor{..} = MempoolCursor
  { pushTransaction = fun . pushTransaction
  , advanceCursor   = fun advanceCursor
  }

hoistMempool :: Functor n => (forall a. m a -> n a) -> Mempool m alg tx -> Mempool n alg tx
hoistMempool fun Mempool{..} = Mempool
  { peekNTransactions = fun peekNTransactions
  , filterMempool     = fun filterMempool
  , getMempoolCursor  = hoistMempoolCursor fun <$> fun getMempoolCursor
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
      { pushTransaction = const $ return Nothing
      , advanceCursor   = return Nothing
      }
  , mempoolSelfTest   = return []
  }






newMempool
  :: forall m alg tx. (Show tx, Ord tx, Crypto alg, CryptoHashable tx, MonadIO m)
  => (tx -> m Bool)
  -> m (Mempool m alg tx)
newMempool validation = do
  varFIFO   :: TVar (IntMap tx)           <- liftIO $ newTVarIO IMap.empty
  varRevMap :: TVar (Map tx Int)          <- liftIO $ newTVarIO Map.empty
  varTxSet  :: TVar (Set (Hashed alg tx)) <- liftIO $ newTVarIO Set.empty
  varMaxN   :: TVar Int                   <- liftIO $ newTVarIO 0
  -- Counters for tracking number of transactions
  varAdded     :: TVar Int <- liftIO $ newTVarIO 0
  varDiscarded :: TVar Int <- liftIO $ newTVarIO 0
  varFiltered  :: TVar Int <- liftIO $ newTVarIO 0
  return Mempool
    { peekNTransactions = toList <$> liftIO (readTVarIO varFIFO)
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
    , filterMempool = do
        -- Invalid transactions
        invalidTx <- filterM (\(_,tx) -> not <$> validation tx)
                   . IMap.toList
                 =<< liftIO (readTVarIO varFIFO)
        -- Remove invalid transactions
        liftIO $ atomically $ do
          modifyTVar' varFIFO   $ \m0 ->
            foldl' (\m (i,_) -> IMap.delete i m) m0 invalidTx
          modifyTVar' varRevMap $ \m0 ->
            foldl' (\m (_,tx) -> Map.delete tx m) m0 invalidTx
          modifyTVar' varTxSet  $ \s0 ->
            foldl' (\s(_,tx) -> Set.delete (hashed tx) s) s0 invalidTx
          modifyTVar' varFiltered (+ length invalidTx)
    --
    , mempoolStats = liftIO $ atomically $ do
        mempool'size      <- IMap.size <$> readTVar varFIFO
        mempool'added     <- readTVar varAdded
        mempool'discarded <- readTVar varDiscarded
        mempool'filtered  <- readTVar varFiltered
        return MempoolInfo{..}
    --
    , mempoolSize = do m <- liftIO $ readTVarIO varFIFO
                       return $! IMap.size m
    --
    , txInMempool = \txHash -> liftIO $ do
        txSet <- readTVarIO varTxSet
        return $ txHash `Set.member` txSet
    --
    , getMempoolCursor = liftIO $ atomically $ do
        varN <- newTVar 0
        return MempoolCursor
          { pushTransaction = \tx -> runMaybeT $ do
              let discardSTM = do modifyTVar' varAdded     succ
                                  modifyTVar' varDiscarded succ
                  discard    = do liftIO $ atomically discardSTM
                                  empty
              -- Abort if tx is already in mempool
              do revMap <- liftIO $ readTVarIO varRevMap
                 when (tx `Map.member` revMap) discard
              -- Validate TX
              lift (validation tx) >>= \case
                False -> discard
                True  -> MaybeT $ liftIO $ atomically $ do
                  -- Still it's possible that tx was added while we
                  -- were checking its validity
                  rmap <- readTVar varRevMap
                  case tx `Map.notMember` rmap of
                    False -> Nothing <$ discardSTM
                    True  -> do
                      let txHash = hashed tx
                      modifyTVar' varAdded     succ
                      n <- succ <$> readTVar varMaxN
                      modifyTVar' varFIFO   $ IMap.insert n tx
                      modifyTVar' varRevMap $ Map.insert tx n
                      modifyTVar' varTxSet  $ Set.insert txHash
                      writeTVar   varMaxN   $! n
                      return $ Just txHash
            --
          , advanceCursor = liftIO $ atomically $ do
              fifo <- readTVar varFIFO
              n    <- readTVar varN
              case n `IMap.lookupGT` fifo of
                Nothing       -> return Nothing
                Just (n', tx) -> do writeTVar varN $! n'
                                    return (Just tx)
          }
    --
    , mempoolSelfTest = liftIO $ atomically $ do
        fifo <- readTVar varFIFO
        rev  <- readTVar varRevMap
        tx   <- readTVar varTxSet
        maxN <- readTVar varMaxN
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
    }
