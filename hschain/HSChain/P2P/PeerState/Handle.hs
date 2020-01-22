{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs            #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE NamedFieldPuns   #-}
{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE RecordWildCards  #-}
module HSChain.P2P.PeerState.Handle
 ( module HSChain.P2P.PeerState.Types
 , handlerTimeout
 , handlerTx
 , handlerGossip
 ) where

import Control.Monad (foldM)
import Control.Monad.IO.Class
import Lens.Micro.Mtl

import HSChain.Control (atomicallyIO)
import HSChain.Blockchain.Internal.Types
import HSChain.Types.Blockchain
import HSChain.P2P.Internal.Types
import HSChain.P2P.PeerState.Monad
import HSChain.P2P.PeerState.Types
import HSChain.P2P.PeerState.Handle.Utils (HandlerDict(..))
import qualified HSChain.P2P.PeerState.Handle.Ahead   as Ahead
import qualified HSChain.P2P.PeerState.Handle.Current as Current
import qualified HSChain.P2P.PeerState.Handle.Lagging as Lagging
import qualified HSChain.P2P.PeerState.Handle.Unknown as Unknown



----------------------------------------------------------------
-- Handler of timeouts
----------------------------------------------------------------

handlerTimeout
  :: (BlockData a, HandlerCtx a m)
  => Config a
  -> State a
  -> GossipTimeout
  -> m (State a, [Command a])
handlerTimeout cfg st msg = do
  gossip <- case st of
    Lagging s -> generateGossip cfg Lagging.handler s msg
    Current s -> generateGossip cfg Current.handler s msg
    Ahead   s -> generateGossip cfg Ahead.handler   s msg
    Unknown s -> generateGossip cfg Unknown.handler s msg
  st' <- foldM (handleIssuedGossip cfg) st gossip
  return (st', Push2Gossip <$> gossip)

-- | Generate gossip messages to be sent to peer.
generateGossip :: (HandlerCtx a m)
               => Config a
               -> HandlerDict s a m
               -> s a
               -> GossipTimeout
               -> m [GossipMsg a]
generateGossip cfg dict st = \case
  TimeoutProposal  -> call handlerProposalTimeout
  TimeoutPrevote   -> call handlerPrevoteTimeout
  TimeoutPrecommit -> call handlerPrecommitTimeout
  TimeoutBlock     -> call handlerBlocksTimeout
  TimeoutAnnounce  -> handlerAnnounceTimeout cfg
  where
    call f = f dict cfg st

handlerAnnounceTimeout :: (MonadIO m) => Config a -> m [GossipMsg a]
handlerAnnounceTimeout cfg = do
  st <- atomicallyIO $ view consensusSt cfg
  return $ case st of
    Nothing -> []
    Just (h,TMState{smRound,smStep}) -> do
        (GossipAnn $ AnnStep $ FullStep h smRound smStep)
      : case smStep of
          StepAwaitCommit r -> [GossipAnn $ AnnHasProposal h r]
          _                 -> []


----------------------------------------------------------------
-- Handling of messages from consensus
----------------------------------------------------------------

handlerTx
  :: (BlockData a, HandlerCtx a m)
  => Config a
  -> State a
  -> MessageTx a
  -> m (State a, [Command a])
handlerTx cfg st msgTx = do
  st' <- handleIssuedGossip cfg st msgGsp
  return (st', [Push2Gossip msgGsp])
  where
    msgGsp = case msgTx of
      TxAnn       a -> GossipAnn       a
      TxProposal  p -> GossipProposal  p
      TxPreVote   v -> GossipPreVote   v
      TxPreCommit v -> GossipPreCommit v

handleIssuedGossip
  :: (BlockData a, HandlerCtx a m)
  => Config a
  -> State a
  -> GossipMsg a
  -> m (State a)
handleIssuedGossip cfg st msg = do
  case st of
    Lagging s -> runTransitionT (issuedGossipHandlerGeneric cfg Lagging.handler msg) s
    Current s -> runTransitionT (issuedGossipHandlerGeneric cfg Current.handler msg) s
    Ahead   s -> runTransitionT (issuedGossipHandlerGeneric cfg Ahead.handler   msg) s
    Unknown s -> runTransitionT (issuedGossipHandlerGeneric cfg Unknown.handler msg) s

issuedGossipHandlerGeneric
  :: (HandlerCtx a m)
  => Config a
  -> HandlerDict s a m
  -> GossipMsg a
  -> TransitionT s a m ()
issuedGossipHandlerGeneric cfg HandlerDict{..} m = case m of
  GossipProposal {}     -> handlerGossipMsg cfg m
  GossipPreVote {}      -> handlerGossipMsg cfg m
  GossipPreCommit {}    -> handlerGossipMsg cfg m
  GossipBlock {}        -> handlerGossipMsg cfg m
  GossipAnn (AnnStep s) -> advanceOurHeight s
  GossipAnn _           -> return ()
  GossipTx{}            -> return ()
  GossipPex{}           -> return ()


----------------------------------------------------------------
-- Handler for incoming gossip
----------------------------------------------------------------

handlerGossip
  :: (BlockData a, HandlerCtx a m)
  => Config a
  -> State a
  -> GossipMsg a
  -> m (State a, [Command a])
handlerGossip cfg st msg = do
  st' <- handleIssuedGossip cfg st msg
  return (st', msgRx)
  where
    msgRx = case msg of
      (GossipPreVote   v) -> [SendRX $ RxPreVote   v]
      (GossipPreCommit v) -> [SendRX $ RxPreCommit v]
      (GossipProposal  p) -> [SendRX $ RxProposal  p]
      (GossipBlock     b) -> [SendRX $ RxBlock     b]
      (GossipTx tx      ) -> [Push2Mempool tx]
      (GossipPex pexmsg ) -> [SendPEX pexmsg]
      _                   -> []
