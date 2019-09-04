{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- |
module HSChain.Store.STM (
    -- * Proposal storage
    newSTMPropStorage
    -- * Mempool
  , newMempool
    -- * Blockchain state storge
  , newSTMBchStorage
  , snapshotState
  ) where

import Control.Applicative
import Control.Concurrent.STM
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe
import Data.Foldable
import Text.Printf

import Codec.Serialise (Serialise)
import Data.List       (nub, sort)
import Data.Maybe      (fromMaybe)
import Data.Tuple      (swap)

import qualified Data.IntMap.Strict as IMap
import           Data.IntMap.Strict   (IntMap)
import qualified Data.Map.Strict    as Map
import           Data.Map.Strict      (Map)
import qualified Data.Set           as Set
import           Data.Set             (Set)

import HSChain.Crypto
import HSChain.Store
import HSChain.Types.Blockchain


newSTMPropStorage
  :: (Crypto alg, MonadIO m)
  => m (ProposalStorage 'RW m alg a)
newSTMPropStorage = fmap (hoistPropStorageRW liftIO) $ liftIO $ do
  varH    <- newTVarIO (Height 0) -- Current height
  varPBlk <- newTVarIO Map.empty  -- Proposed blocks
  varRMap <- newTVarIO Map.empty  -- Map of rounds to block IDs
  return ProposalStorage
    { resetPropStorage = \h -> atomically $ do
        writeTVar varH    h
        writeTVar varPBlk Map.empty
        writeTVar varRMap Map.empty
    --
    , setPropValidation = \bid mSt -> do
        let action = atomically . modifyTVar' varPBlk . flip Map.adjust bid
        case mSt of
          Nothing -> action $ \case 
            UntestedBlock _ -> InvalidBlock
            InvalidBlock    -> InvalidBlock
            _               -> error "CANT HAPPEN"
          Just bst -> action $ \case
            UntestedBlock b -> GoodBlock b bst
            b@GoodBlock{}   -> b
            _               -> error "CANT HAPPEN"
    --
    , storePropBlock = \blk -> atomically $ do
        h <- readTVar varH
        when (headerHeight (blockHeader blk) == h) $
          modifyTVar' varPBlk $ flip Map.adjust (blockHash blk) $ \case
            UnknownBlock -> UntestedBlock blk
            b            -> b
    --
    , allowBlockID = \r bid -> atomically $ do
        modifyTVar' varRMap $ Map.insert r   bid
        modifyTVar' varPBlk $ flip Map.alter bid $ \case
          Nothing -> Just UnknownBlock
          Just b  -> Just b
    --
    , retrievePropByID = \h0 bid -> atomically $ do
        h <- readTVar varH
        if h == h0 then fromMaybe UnknownBlock . Map.lookup bid <$> readTVar varPBlk
                   else return UnknownBlock
    --
    , retrievePropByR = \h r -> atomically $ do
        h0 <- readTVar varH
        if h /= h0
          then return Nothing
          else do rMap <- readTVar varRMap
                  pBlk <- readTVar varPBlk
                  return $ do bid <- r   `Map.lookup` rMap
                              blk <- bid `Map.lookup` pBlk
                              return (bid, blk)
    }

newMempool
  :: forall m alg tx. (Show tx, Ord tx, Serialise tx, Crypto alg, MonadIO m)
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
          modifyTVar' varFiltered (+ (length invalidTx))
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

----------------------------------------------------------------
-- Blockchain storage
----------------------------------------------------------------

-- | Create new storage for blockchain 
newSTMBchStorage :: (MonadIO m) => InterpreterState a -> m (BChStore m a)
newSTMBchStorage st0 = do
  varSt <- liftIO $ newTVarIO (Nothing, st0)
  return BChStore
    { bchCurrentState = liftIO (readTVarIO varSt)
    --
    , bchStoreRetrieve = \h -> 
        liftIO (readTVarIO varSt) >>= \case
          (Just h', s) | h == h' -> return (Just s)
          _                      -> return Nothing
    --
    , bchStoreStore   = \h st ->
        liftIO $ atomically $ writeTVar varSt $ (Just h, st)
    }

-- | Store complete snapshot of state every N
snapshotState
  :: (MonadIO m, MonadDB m alg a, BlockData a)
  => Int           -- ^ Store snapshot every n height
  -> BChStore m a  -- ^ Store to modify
  -> m (BChStore m a)
snapshotState everyN BChStore{..} = do
  queryRO retrieveSavedState >>= mapM_ (\(h,s) -> bchStoreStore h s)
  return BChStore
    { bchStoreStore = \h@(Height n) st -> do
        when (fromIntegral n `mod` everyN == 0) $
          queryRW (storeStateSnapshot h st) >>= \case
            Just () -> return ()
            Nothing -> error "Cannot store snapshot"
        bchStoreStore h st
    , ..
    }
