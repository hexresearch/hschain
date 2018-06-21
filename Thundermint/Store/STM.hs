{-# LANGUAGE DataKinds #-}
-- |
module Thundermint.Store.STM (
    newSTMBlockStorage
  , newSTMPropStorage
  ) where

import Codec.Serialise (Serialise)
import Control.Applicative
import Control.Monad
import Control.Monad.Trans.Maybe
import Control.Concurrent.STM
import qualified Data.Map             as Map

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
        when (headerHeight (blockHeader blk) == h) $ do
          let bid = blockHash blk
          modifyTVar' varPBlk $ Map.insert bid blk
    }
