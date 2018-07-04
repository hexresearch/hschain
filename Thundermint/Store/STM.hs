{-# LANGUAGE DataKinds  #-}
{-# LANGUAGE LambdaCase #-}
-- |
module Thundermint.Store.STM (
    newSTMBlockStorage
  , newSTMPropStorage
  , newMempool
  ) where

import Codec.Serialise (Serialise)
import Control.Applicative
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe
import Control.Concurrent.STM
import Data.Foldable
import qualified Data.Map             as Map
import qualified Data.Set             as Set
import qualified Data.Sequence        as Seq

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
  :: (Crypto alg, Serialise a)
  => IO (ProposalStorage 'RW IO alg a)
newSTMPropStorage = do
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
  :: ()
  => (tx -> IO Bool)
  -> IO (Mempool IO tx)
newMempool validation = do
  varFIFO <- newTVarIO Seq.empty
  varCnt  <- newTVarIO (0 :: Int)
  return Mempool
    { takeNTransactiona = \mn -> atomically $ do
        txs <- readTVar varFIFO
        let (pick,leave) = case mn of
              Nothing -> (txs,Seq.empty)
              Just n  -> Seq.splitAt n txs
        writeTVar varFIFO leave
        modifyTVar' varCnt (+ Seq.length pick)
        return $ toList pick
    --
    , getMempoolCursor = atomically $ do
        varN <- newTVar =<< readTVar varCnt
        return MempoolCursor
          { pushTransaction = \tx -> validation tx >>= \case
              False -> return ()
              True  -> atomically $ modifyTVar' varFIFO (Seq.|> tx)
          , advanceCursor = atomically $ do
              off <- readTVar varCnt
              i   <- readTVar varN
              txs <- readTVar varFIFO
              case (i - off) `Seq.lookup` txs of
                Nothing -> return Nothing
                Just tx -> do modifyTVar varN (+1)
                              return (Just tx)
          }
    }
