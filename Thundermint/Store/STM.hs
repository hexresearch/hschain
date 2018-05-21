{-# LANGUAGE DataKinds #-}
-- |
module Thundermint.Store.STM (
    newSTMBlockStorage
  ) where

import Codec.Serialise (Serialise)
import Control.Monad
import Control.Concurrent.STM
import           Data.Map               (Map)
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
  varPBlk   <- newTVarIO $ Map.empty
  varLCmt   <- newTVarIO Nothing
  let currentHeight = do
        Just (h,_) <- Map.lookupMax <$> readTVar varBlocks
        return h
  let bs = BlockStorage
        { blockchainHeight = atomically currentHeight
        , retrieveBlock    = \h -> do m <- readTVarIO varBlocks
                                      return $ Map.lookup h m
        , retrieveBlockID  = (fmap . fmap) blockHash . retrieveBlock bs
        , retrieveCommit   = \h -> atomically $ do
            hMax <- currentHeight
            if h == hMax then readTVar varLCmt
                         else do bmap <- readTVar varBlocks
                                 return $ blockLastCommit =<< Map.lookup (next h) bmap
        , retrieveLastCommit = readTVarIO varLCmt
        , storeCommit = \cmt blk -> atomically $ do
            h <- currentHeight
            modifyTVar' varBlocks $ Map.insert (next h) blk
            writeTVar   varLCmt (Just cmt)
            writeTVar   varPBlk Map.empty
        --
        , retrievePropBlocks = \height -> atomically $ do
            h <- currentHeight
            if h == height then readTVar varPBlk
                           else return Map.empty
        , retrieveStoredProps = atomically $ do
            h  <- currentHeight
            bs <- readTVar varPBlk
            return (h, Map.keysSet bs)
        , storePropBlock = \height blk -> atomically $ do
            h <- currentHeight
            when (height == h) $ do
              let bid = blockHash blk
              modifyTVar varPBlk $ Map.insert bid blk
        }
  return bs
