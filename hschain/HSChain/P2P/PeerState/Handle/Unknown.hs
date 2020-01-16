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
  { handlerGossipMsg      = handlerGossip
  , advanceOurHeight      = \_ -> return ()
  , handlerVotesTimeout   = return ()
  , handlerMempoolTimeout = return ()
  , handlerBlocksTimeout  = return ()
  }

handlerGossip :: MessageHandler UnknownState a m
handlerGossip = \case
  GossipAnn (AnnStep step) -> advancePeer step
  _                        -> return ()
