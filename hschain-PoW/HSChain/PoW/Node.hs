{-# LANGUAGE TypeFamilies #-}
-- |HSChain.PoW.Node.hs
--
-- Main node loop.
--
-- You may use it directly or copy and tailor.
--
-- Copyright (C) ... 2020
module HSChain.PoW.Node
  ( genericMiningLoop
    -- * Block storage
  , inMemoryDB
  , blockDatabase
  ) where

import Control.Concurrent
import Control.Monad
import Control.Monad.Cont

import HSChain.Control.Channels
import HSChain.PoW.Consensus
import HSChain.PoW.P2P
import HSChain.PoW.Types
import HSChain.PoW.Store
import HSChain.Control.Util
import HSChain.Control.Class



genericMiningLoop
  :: (Mineable b, MonadFork m, BlockType view ~ b, MonadOf view ~ m)
  => (view -> BH b -> Time -> [Tx b] -> m (Block b))
  -> PoW view
  -> m x
genericMiningLoop makeCandidate pow = start
  where
    --
    start = do
      c  <- atomicallyIO $ currentConsensus pow
      ch <- atomicallyIO $ mempoolUpdates $ mempoolAPI pow
      let (bh, st, _) = _bestHead c
      loop ch =<< mine bh st Nothing
    --
    loop ch tid = do
      (bh, st, txs) <- awaitIO ch
      liftIO $ killThread tid
      loop ch =<< mine bh st (Just txs)
    -- Here we simply try again to create new block in case we wasnt'
    -- able to create one by fiddling nonce. At very least time should
    -- change
    mine bh st = fork . tryMine
      where
        tryMine mtxs = do
          t      <- getCurrentTime
          txs    <- case mtxs of
            Just txs -> return txs
            Nothing  -> atomicallyIO $ mempoolContent $ mempoolAPI pow
          bCand  <- makeCandidate st bh t txs
          (bMined,_) <- adjustPuzzle bCand
          case bMined of
            Just b  -> void $ sendNewBlock pow b
            Nothing -> tryMine Nothing

