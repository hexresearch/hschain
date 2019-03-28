{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}
-- |
module Thundermint.Store.STM (
    newSTMPropStorage
  , newMempool
  ) where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe
import Control.Concurrent.STM
import Data.Foldable
import Data.List (nub)
import qualified Data.Map             as Map
import qualified Data.IntMap          as IMap
import qualified Data.Set             as Set
import Text.Printf

import Thundermint.Types.Blockchain
import Thundermint.Crypto
import Thundermint.Store


newSTMPropStorage
  :: (Crypto alg, MonadIO m)
  => m (ProposalStorage 'RW m alg a)
newSTMPropStorage = fmap (hoistPropStorageRW liftIO) $ liftIO $ do
  varH    <- newTVarIO (Height 0) -- Current height
  varPBlk <- newTVarIO Map.empty  -- Proposed blocks
  varRMap <- newTVarIO Map.empty  -- Map of rounds to block IDs
  varBids <- newTVarIO Set.empty  -- Allowed block IDs
  return ProposalStorage
    { currentHeight = readTVarIO varH
    --
    , retrievePropByID = \height bid -> atomically $ do
        h <- readTVar varH
        if h == height then Map.lookup bid <$> readTVar varPBlk
                       else return Nothing
    --
    , advanceToHeight = \h -> atomically $ do
        h0 <- readTVar varH
        when (h /= h0) $ do writeTVar varH    h
                            writeTVar varPBlk Map.empty
                            writeTVar varRMap Map.empty
                            writeTVar varBids Set.empty
    --
    , storePropBlock = \blk -> atomically $ do
        h <- readTVar varH
        when ((headerHeight $ pet $ blockHeader $ pet blk) == h) $ do
          let bid = blockHash blk
          bids <- readTVar varBids
          when (bid `Set.member` bids) $
            modifyTVar' varPBlk $ Map.insert bid blk
    --
    , allowBlockID = \r bid -> atomically $ do
        modifyTVar' varRMap $ Map.insert r bid
        modifyTVar' varBids $ Set.insert bid
    --
    , retrievePropByR = \h r -> atomically $ do
        h0 <- readTVar varH
        runMaybeT $ do
          guard (h == h0)
          Just bid <- fmap (Map.lookup r)   $ lift $ readTVar varRMap
          Just b   <- fmap (Map.lookup bid) $ lift $ readTVar varPBlk
          return (b,bid)
    }

newMempool
  :: (Ord tx, Crypto alg, MonadIO m)
  => (Pet tx -> m Bool)
  -> m (Mempool m alg tx)
newMempool validation = do
  varFIFO      <- liftIO $ newTVarIO IMap.empty
  varRevMap    <- liftIO $ newTVarIO Map.empty
  varTxSet     <- liftIO $ newTVarIO Set.empty
  varMaxN      <- liftIO $ newTVarIO 0
  -- Counters for tracking number of transactions
  varAdded     <- liftIO $ newTVarIO 0
  varDiscarded <- liftIO $ newTVarIO 0
  varFiltered  <- liftIO $ newTVarIO 0
  return Mempool
    { peekNTransactions = \mn -> do
        fifo <- liftIO (readTVarIO varFIFO)
        return $ case mn of
          Nothing -> toList fifo
          Just n  -> take n $ toList fifo
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
          { pushTransaction = \tx -> validation tx >>= \case
              False -> liftIO $ atomically $ do
                modifyTVar' varAdded     succ
                modifyTVar' varDiscarded succ
                return Nothing
              True  -> liftIO $ atomically $ do
                rmap <- readTVar varRevMap
                case tx `Map.notMember` rmap of
                  True -> do
                    let txHash = hashed tx
                    modifyTVar' varAdded     succ
                    n <- succ <$> readTVar varMaxN
                    modifyTVar' varFIFO   $ IMap.insert n tx
                    modifyTVar' varRevMap $ Map.insert tx n
                    modifyTVar' varTxSet  $ Set.insert txHash
                    writeTVar   varMaxN   $! n
                    return $ Just txHash
                  False -> return Nothing
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
        let nFIFO = IMap.size fifo
            nRev  = Map.size rev
            nTX   = Set.size tx
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
          ]
    }
