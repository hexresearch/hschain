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

handler :: Handler UnknownState Event alg a m
handler =
  handlerGeneric
   handlerGossipMsg
   handlerVotesTimeout
   handlerMempoolTimeout
   handlerBlocksTimeout

issuedGossipHandler :: Handler UnknownState GossipMsg alg a m
issuedGossipHandler =
  issuedGossipHandlerGeneric
    handlerGossipMsg
    advanceOurHeight

handlerGossipMsg :: MessageHandler UnknownState alg a m
handlerGossipMsg = \case
  GossipAnn (AnnStep step) -> advancePeer step
  _                        -> currentState

----------------------------------------------------------------

advanceOurHeight :: AdvanceOurHeight UnknownState alg a m
advanceOurHeight = const currentState
----------------------------------------------------------------

handlerVotesTimeout :: TimeoutHandler UnknownState alg a m
handlerVotesTimeout = currentState

----------------------------------------------------------------

handlerMempoolTimeout :: TimeoutHandler UnknownState alg a m
handlerMempoolTimeout = currentState
----------------------------------------------------------------

handlerBlocksTimeout :: TimeoutHandler UnknownState alg a m
handlerBlocksTimeout = currentState

