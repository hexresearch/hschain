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

import HSChain.Control.Util (atomicallyIO)
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

-- | Handler for timeout messages which means we need to generate new
--   gossip messages to send to peers.
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

-- Generate gossip messages to be sent to peer.
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

-- In order to avoid deadlocks we periodically send current state to
-- our peers. We also send out that we have proposal whenever we have
-- commit already and waiting for block.
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

-- | Handler for messages coming from consensus engine
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

-- Handler for issued gossip. It works same as handler for incoming
-- gossip except for AnnStep since we need to change /our/ state
-- instead of peer's state
handleIssuedGossip
  :: (BlockData a, HandlerCtx a m)
  => Config a
  -> State a
  -> GossipMsg a
  -> m (State a)
handleIssuedGossip cfg st msg
  = dispatch st
  $ \dct -> case msg of
              GossipProposal {}     -> handlerGossipMsg dct cfg msg
              GossipPreVote {}      -> handlerGossipMsg dct cfg msg
              GossipPreCommit {}    -> handlerGossipMsg dct cfg msg
              GossipBlock {}        -> handlerGossipMsg dct cfg msg
              GossipAnn (AnnStep s) -> advanceOurHeight dct s
              GossipAnn _           -> return ()
              GossipTx{}            -> return ()
              GossipPex{}           -> return ()


----------------------------------------------------------------
-- Handler for incoming gossip
----------------------------------------------------------------

-- | Handler for gossip coming from peer.
handlerGossip
  :: (BlockData a, HandlerCtx a m)
  => Config a
  -> State a
  -> GossipMsg a
  -> m (State a, [Command a])
handlerGossip cfg st m = do
  st' <- dispatch st $ \dct -> handlerGossipMsg dct cfg m
  return (st', msgRx)
  where
    msgRx = case m of
      GossipPreVote   v -> [SendRX $ RxPreVote   v]
      GossipPreCommit v -> [SendRX $ RxPreCommit v]
      GossipProposal  p -> [SendRX $ RxProposal  p]
      GossipBlock     b -> [SendRX $ RxBlock     b]
      GossipTx tx       -> [Push2Mempool tx]
      GossipPex pexmsg  -> [SendPEX pexmsg]
      GossipAnn{}       -> []


dispatch
  :: (BlockData a, HandlerCtx a m)
  => State a
  -> (forall s. HandlerDict s a m -> TransitionT s a m ())
  -> m (State a)
dispatch st fun =
  case st of
    Lagging s -> runTransitionT (fun Lagging.handler) s
    Current s -> runTransitionT (fun Current.handler) s
    Ahead   s -> runTransitionT (fun Ahead.handler  ) s
    Unknown s -> runTransitionT (fun Unknown.handler) s
