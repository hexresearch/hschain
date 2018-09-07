{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}
-- |
module Thundermint.Store.STM (
    newSTMBlockStorage
  , newSTMPropStorage
  , newMempool
  ) where

import Codec.Serialise (Serialise)
import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe
import Control.Concurrent.STM
import Data.Foldable
import qualified Data.Map             as Map
import qualified Data.IntMap          as IMap
import qualified Data.Set             as Set

import Thundermint.Consensus.Types
import Thundermint.Crypto
import Thundermint.Crypto.Containers
import Thundermint.Store


-- |
newSTMBlockStorage
  :: (Crypto alg, Serialise a)
  => Block alg a
  -> ValidatorSet alg
  -> IO (BlockStorage 'RW IO alg a)
newSTMBlockStorage gBlock initalVals = do
  -- FIXME: we MUST require correct genesis block
  varBlocks <- newTVarIO $ Map.singleton (Height 0) gBlock
  varLCmt   <- newTVarIO Nothing
  varVals   <- newTVarIO $ Map.fromList [ (Height 0, initalVals)
                                        , (Height 1, initalVals)
                                        ]
  let currentH = do
        Just (h,_) <- Map.lookupMax <$> readTVar varBlocks
        return h
  let retrieveBlk h = do m <- readTVarIO varBlocks
                         return $ Map.lookup h m
  let retrieveCmt h = do bmap <- readTVar varBlocks
                         return $ blockLastCommit =<< Map.lookup (next h) bmap
  --
  return BlockStorage
    { blockchainHeight = atomically currentH
    , retrieveBlock    = retrieveBlk
    , retrieveBlockID  = (fmap . fmap) blockHash . retrieveBlk
    , retrieveCommitRound = \h -> do
        let getRound (Commit _ (v:_)) = voteRound (signedValue v)
            getRound _                = error "Impossible"
        mc <- atomically $ runMaybeT
            $  MaybeT (retrieveCmt h)
           <|> MaybeT (do hBC <- currentH
                          guard (hBC == h)
                          readTVar varLCmt
                      )
        return $ fmap getRound mc
    , retrieveCommit      = atomically . retrieveCmt
    , retrieveLocalCommit = \h -> atomically $ do
        ourH <- currentH
        if h == ourH then readTVar varLCmt else return Nothing
    , storeCommit = \vals cmt blk -> atomically $ do
        h <- currentH
        modifyTVar' varBlocks $ Map.insert (next h) blk
        modifyTVar' varVals   $ Map.insert (next (next h)) vals
        writeTVar   varLCmt (Just cmt)
    , retrieveValidatorSet = \h -> do vals <- readTVarIO varVals
                                      return $ Map.lookup h vals
    , closeBlockStorage = return ()
    }


newSTMPropStorage
  :: (Crypto alg, Serialise a, MonadIO m)
  => m (ProposalStorage 'RW m alg a)
newSTMPropStorage = fmap (hoistPropStorageRW liftIO) $ liftIO $ do
  varH    <- newTVarIO (Height 0) -- Current height
  varPBlk <- newTVarIO Map.empty  -- Proposed blocks
  varRMap <- newTVarIO Map.empty  -- Map of rounds to block IDs
  varBids <- newTVarIO Set.empty  -- Allowed block IDs
  return ProposalStorage
    { currentHeight = readTVarIO varH
    --
    , advanceToHeight = \h -> atomically $ do
        h0 <- readTVar varH
        when (h /= h0) $ do writeTVar varH    h
                            writeTVar varPBlk Map.empty
                            writeTVar varRMap Map.empty
                            writeTVar varBids Set.empty
    --
    , retrievePropBlocks = \height -> atomically $ do
        h <- readTVar varH
        if h == height then readTVar varPBlk
                       else return Map.empty
    --
    , waitForBlockID = \bid -> atomically $ do
        bmap <- readTVar varPBlk
        case bid `Map.lookup` bmap of
          Nothing -> retry
          Just b  -> return b
    --
    , storePropBlock = \blk -> atomically $ do
        h <- readTVar varH
        when (headerHeight (blockHeader blk) == h) $ do
          let bid = blockHash blk
          bids <- readTVar varBids
          when (bid `Set.member` bids) $
            modifyTVar' varPBlk $ Map.insert bid blk
    --
    , allowBlockID = \r bid -> atomically $ do
        modifyTVar' varRMap $ Map.insert r bid
        modifyTVar' varBids $ Set.insert bid
    --
    , blockAtRound = \h r -> atomically $ do
        h0 <- readTVar varH
        runMaybeT $ do
          guard (h == h0)
          Just bid <- fmap (Map.lookup r)   $ lift $ readTVar varRMap
          Just b   <- fmap (Map.lookup bid) $ lift $ readTVar varPBlk
          return (b,bid)
    }

newMempool
  :: (Ord tx, Serialise tx, Crypto alg, MonadIO m)
  => (tx -> m Bool)
  -> m (Mempool m alg tx)
newMempool validation = do
  varFIFO      <- liftIO $ newTVarIO IMap.empty
  varRevMap    <- liftIO $ newTVarIO Map.empty
  varMaxN      <- liftIO $ newTVarIO 0
  varAdded     <- liftIO $ newTVarIO 0
  varDiscarded <- liftIO $ newTVarIO 0
  varFiltered  <- liftIO $ newTVarIO 0
  return Mempool
    { peekNTransactions = \mn -> do
        fifo <- liftIO (readTVarIO varFIFO)
        return $ case mn of
          Nothing -> toList fifo
          Just n  -> take n $ toList fifo
    -- We remove prefix of transactions which aren't valid
    -- anymore. Main source of invalidation of formerly valid
    -- transactions is commits in blocks proposed by other nodes.
    , filterMempool = do
        let stepCheck []              = return (-1)
            stepCheck ((k,tx) : txs ) =
              validation tx >>= \case
                True  -> return k
                False -> stepCheck txs
        firstGood <- stepCheck . IMap.toList
                 =<< liftIO (readTVarIO varFIFO)
        liftIO $ atomically $ do
          fifo <- readTVar varFIFO
          let (dropped,point,retained) = IMap.splitLookup (firstGood - 1) fifo
          modifyTVar' varFiltered (+ (length point + IMap.size dropped))
          modifyTVar' varRevMap $  Map.filter (>= firstGood)
          writeTVar   varFIFO   $! retained
    --
    , mempoolStats = liftIO $ atomically $ do
        mempool'size      <- IMap.size <$> readTVar varFIFO
        mempool'added     <- readTVar varAdded
        mempool'discarded <- readTVar varDiscarded
        mempool'filtered  <- readTVar varFiltered
        return MempoolInfo{..}
    --
    , getMempoolCursor = liftIO $ atomically $ do
        varN <- newTVar 0
        return MempoolCursor
          { pushTransaction = \tx -> validation tx >>= \case
              False -> liftIO $ atomically $ do
                modifyTVar varAdded succ
                modifyTVar varDiscarded succ
                return Nothing
              True  -> liftIO $ atomically $ do
                rmap <- readTVar varRevMap
                case tx `Map.notMember` rmap of
                  True -> do
                    modifyTVar varAdded     succ
                    n <- succ <$> readTVar varMaxN
                    modifyTVar' varFIFO   $ IMap.insert n tx
                    modifyTVar' varRevMap $ Map.insert tx n
                    writeTVar   varMaxN n
                    return $ Just $ hash tx
                  False -> return Nothing
            --
          , advanceCursor = liftIO $ atomically $ do
              fifo <- readTVar varFIFO
              n    <- readTVar varN
              case n `IMap.lookupGT` fifo of
                Nothing       -> return Nothing
                Just (n', tx) -> do writeTVar varN n'
                                    return (Just tx)
          }
    }
