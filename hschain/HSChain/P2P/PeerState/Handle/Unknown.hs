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

handler :: Handler UnknownState Event a m
handler =
  handlerGeneric
   handlerGossipMsg
   handlerVotesTimeout
   handlerMempoolTimeout
   handlerBlocksTimeout

issuedGossipHandler :: Handler UnknownState GossipMsg a m
issuedGossipHandler =
  issuedGossipHandlerGeneric
    handlerGossipMsg
    advanceOurHeight

handlerGossipMsg :: MessageHandler UnknownState a m
handlerGossipMsg = \case
  GossipAnn (AnnStep step) -> advancePeer step
  _                        -> currentState

----------------------------------------------------------------

advanceOurHeight :: AdvanceOurHeight UnknownState a m
advanceOurHeight = const currentState
----------------------------------------------------------------

handlerVotesTimeout :: TimeoutHandler UnknownState a m
handlerVotesTimeout = currentState

----------------------------------------------------------------

handlerMempoolTimeout :: TimeoutHandler UnknownState a m
handlerMempoolTimeout = currentState
----------------------------------------------------------------

handlerBlocksTimeout :: TimeoutHandler UnknownState a m
handlerBlocksTimeout = currentState

