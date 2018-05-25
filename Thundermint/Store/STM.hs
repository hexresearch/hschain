{-# LANGUAGE DataKinds #-}
-- |
module Thundermint.Store.STM (
    newSTMBlockStorage
  , newSTMPropStorage
  ) where

import Codec.Serialise (Serialise)
import Control.Monad
import Control.Concurrent.STM
import qualified Data.Map             as Map

import Thundermint.Consensus.Types
import Thundermint.Crypto
import Thundermint.Store


-- | 
newSTMBlockStorage
  :: (Crypto alg, Serialise a)
  => Block alg a
  -> IO (BlockStorage 'RW IO alg a)
newSTMBlockStorage gBlock = do
  -- FIXME: we MUST require correct genesis block
  varBlocks <- newTVarIO $ Map.singleton (Height 0) gBlock
  varLCmt   <- newTVarIO Nothing
  let currentH = do
        Just (h,_) <- Map.lookupMax <$> readTVar varBlocks
        return h
  let retrieveBlk h = do m <- readTVarIO varBlocks
                         return $ Map.lookup h m
  return BlockStorage
    { blockchainHeight = atomically currentH
    , retrieveBlock    = retrieveBlk
    , retrieveBlockID  = (fmap . fmap) blockHash . retrieveBlk
    , retrieveCommit   = \h -> atomically $ do
        hMax <- currentH
        if h == hMax then readTVar varLCmt
                     else do bmap <- readTVar varBlocks
                             return $ blockLastCommit =<< Map.lookup (next h) bmap
    , retrieveLastCommit = readTVarIO varLCmt
    , storeCommit = \cmt blk -> atomically $ do
        h <- currentH
        modifyTVar' varBlocks $ Map.insert (next h) blk
        writeTVar   varLCmt (Just cmt)
    , closeBlockStorage = return ()
    }


newSTMPropStorage
  :: (Crypto alg, Serialise a)
  => IO (ProposalStorage 'RW IO alg a)
newSTMPropStorage = do
  varH    <- newTVarIO (Height 0)
  varPBlk <- newTVarIO Map.empty
  return ProposalStorage
    { currentHeight = readTVarIO varH
    --
    , advanceToHeight = \h -> atomically $ do
        h0 <- readTVar varH
        when (h /= h0) $ do writeTVar varH    h
                            writeTVar varPBlk Map.empty
    --
    , retrievePropBlocks = \height -> atomically $ do
        h <- readTVar varH
        if h == height then readTVar varPBlk
                       else return Map.empty
    --
    , storePropBlock = \blk -> atomically $ do
        h <- readTVar varH
        when (headerHeight (blockHeader blk) == next h) $ do
          let bid = blockHash blk
          modifyTVar' varPBlk $ Map.insert bid blk
    }
