{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
-- |
module HSChain.PoW.P2P.Handler.Consensus
  ( ConsensusCh(..)
  , threadConsensus
  ) where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.State.Strict
import Control.Monad.Except
import Data.Set (Set)
import Lens.Micro
import Lens.Micro.Mtl

import HSChain.Control.Channels
import HSChain.PoW.Consensus
import HSChain.PoW.P2P.Types
import HSChain.PoW.Types

-- | Channels for sending data to and from consensus thread
data ConsensusCh m b = ConsensusCh
  { bcastAnnounce :: Sink (MsgAnn b)
  , sinkBlockIdx  :: Sink (BlockIndex b)
  , sinkReqBlocks :: Sink (Set (BlockID b))
  , srcRX         :: Src  (BoxRX m b)
  }

-- | Thread that reacts to messages from peers and updates consensus
--   accordingly
threadConsensus 
  :: (MonadIO m, BlockData b)
  => BlockDB m b
  -> Consensus m b
  -> ConsensusCh m b
  -> m x
threadConsensus db consensus0 ConsensusCh{..}
  = flip evalStateT consensus0
  $ forever
  $ do bh <- use $ bestHead . _1
       consensusMonitor db =<< awaitIO srcRX
       sinkIO sinkBlockIdx  =<< use blockIndex
       sinkIO sinkReqBlocks =<< use requiredBlocks
       bh' <- use $ bestHead . _1
       when (bhBID bh /= bhBID bh') $ sinkIO bcastAnnounce $ AnnBestHead $ asHeader bh'


-- Handler for messages coming from peer.
consensusMonitor
  :: (Monad m, BlockData b)
  => BlockDB m b
  -> BoxRX m b
  -> StateT (Consensus m b) m ()
consensusMonitor db (BoxRX message)
  = message $ \case
      RxAnn     m  -> handleAnnounce m
      RxBlock   b  -> handleBlock    b
      RxHeaders hs -> handleHeaders hs
  where
    -- Handler for announces coming from peers (they come unrequested)
    handleAnnounce (AnnBestHead h) =
      runExceptT (processHeader h) >>= \case
        Right () -> return Peer'Noop
        Left  e  -> case e of
          ErrH'KnownHeader       -> return Peer'Noop
          ErrH'HeightMismatch    -> return Peer'Punish
          ErrH'ValidationFailure -> return Peer'Punish
          ErrH'BadParent         -> return Peer'Punish
          -- We got announce that we couldn't attach to block tree. So
          -- we need that peer to catch up
          ErrH'UnknownParent     -> return Peer'EnterCatchup
    -- Handle block that we got from peer
    --
    -- FIXME: Handle announcements
    handleBlock b =
      runExceptT (processBlock db b) >>= \case
        Right () -> return Peer'Noop
        Left  e  -> case e of
          ErrB'UnknownBlock -> error "Impossible: we should'n get unknown block"
          ErrB'InvalidBlock -> return Peer'Noop
    -- Handle headers that we got from peer.
    handleHeaders [] = return Peer'Noop
    handleHeaders (h:hs) =
      runExceptT (processHeader h) >>= \case
        Right () -> handleHeaders hs
        Left  e  -> case e of
          ErrH'KnownHeader       -> handleHeaders hs
          ErrH'HeightMismatch    -> return Peer'Punish
          ErrH'UnknownParent     -> return Peer'Punish
          ErrH'ValidationFailure -> return Peer'Punish
          ErrH'BadParent         -> return Peer'Punish
