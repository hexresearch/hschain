{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs            #-}
{-# LANGUAGE LambdaCase       #-}
module HSChain.P2P.PeerState.Handle.Unknown
  ( handler
  ) where

import HSChain.Blockchain.Internal.Types
import HSChain.P2P.Internal.Types
import HSChain.P2P.PeerState.Monad
import HSChain.P2P.PeerState.Types
import HSChain.P2P.PeerState.Handle.Utils

handler :: (HandlerCtx a m) => HandlerDict UnknownState a m
handler = HandlerDict
  { handlerGossipMsg        = \_   -> handlerGossip
  , advanceOurHeight        = \_   -> return ()
  , handlerProposalTimeout  = \_ _ -> return []
  , handlerPrevoteTimeout   = \_ _ -> return []
  , handlerPrecommitTimeout = \_ _ -> return []
  , handlerBlocksTimeout    = \_ _ -> return []
  }

handlerGossip :: MessageHandler UnknownState a m
handlerGossip = \case
  GossipAnn (AnnStep step) -> advancePeer step
  _                        -> return ()
