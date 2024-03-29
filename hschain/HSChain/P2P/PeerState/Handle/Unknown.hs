{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs            #-}
{-# LANGUAGE LambdaCase       #-}
module HSChain.P2P.PeerState.Handle.Unknown
  ( handler
  ) where

import HSChain.Internal.Types.Consensus
import HSChain.Internal.Types.Messages
import HSChain.P2P.Internal.Types
import HSChain.P2P.PeerState.Monad
import HSChain.P2P.PeerState.Types
import HSChain.P2P.PeerState.Handle.Utils

handler :: (HandlerCtx (BlockType view) m) => HandlerDict UnknownState view m
handler = HandlerDict
  { handlerGossipMsg        = const handlerGossip
  , advanceOurHeight        = \_   -> return ()
  , handlerProposalTimeout  = \_ _ -> return []
  , handlerPrevoteTimeout   = \_ _ -> return []
  , handlerPrecommitTimeout = \_ _ -> return []
  , handlerBlocksTimeout    = \_ _ -> return []
  }

handlerGossip
  :: (HandlerCtx a m)
  => GossipMsg a -> TransitionT UnknownState a m ()
handlerGossip = \case
  GossipAnn (AnnStep step) -> advancePeer step
  _                        -> return ()
