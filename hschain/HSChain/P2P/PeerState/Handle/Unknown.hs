{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs            #-}
{-# LANGUAGE LambdaCase       #-}
module HSChain.P2P.PeerState.Handle.Unknown
  ( handler
  , issuedGossipHandler
  ) where

import HSChain.Blockchain.Internal.Types
import HSChain.P2P.Internal.Types
import HSChain.P2P.PeerState.Monad
import HSChain.P2P.PeerState.Types
import HSChain.P2P.PeerState.Handle.Utils

handler :: (HandlerCtx a m) => HandlerDict UnknownState a m
handler = HandlerDict
  { handlerGossipMsg      = handlerGossip
  , handlerVotesTimeout   = return ()
  , handlerMempoolTimeout = return ()
  , handlerBlocksTimeout  = return ()
  }

issuedGossipHandler :: (HandlerCtx a m) => IssuedDict UnknownState a m
issuedGossipHandler = IssuedDict
  { handlerIssuedGossip = handlerGossip
  , advanceOurHeight    = \_ -> return ()
  }

handlerGossip :: MessageHandler UnknownState a m
handlerGossip = \case
  GossipAnn (AnnStep step) -> advancePeer step
  _                        -> return ()
