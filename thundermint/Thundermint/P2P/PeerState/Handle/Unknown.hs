{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs            #-}

module Thundermint.P2P.PeerState.Handle.Unknown
  ( handler
  , issuedGossipHandler
  ) where

import Control.Monad.RWS.Strict

import Thundermint.Blockchain.Internal.Types
import Thundermint.Debug.Trace
import Thundermint.P2P.Internal.Types
import Thundermint.P2P.PeerState.Monad
import Thundermint.P2P.PeerState.Types

import Thundermint.P2P.PeerState.Handle.Utils

handler :: Handler Unknown Event alg a m
handler =
  handlerGeneric
   handlerGossipMsg
   handlerVotesTimeout
   handlerMempoolTimeout
   handlerBlocksTimeout

issuedGossipHandler :: Handler Unknown GossipMsg alg a m
issuedGossipHandler =
  issuedGossipHandlerGeneric
    handlerGossipMsg
    advanceOurHeight

handlerGossipMsg :: MessageHandler UnknownState alg a m
handlerGossipMsg gossipMsg = do
  case gossipMsg of
    GossipAnn (AnnStep step) -> lift $ advancePeer step
    _                        -> currentState

----------------------------------------------------------------

advanceOurHeight :: AdvanceOurHeight UnknownState alg a m
advanceOurHeight = const currentState
----------------------------------------------------------------

handlerVotesTimeout :: TimeoutHandler UnknownState alg a m
handlerVotesTimeout = do trace (TePeerGossipVotes TepgvUnknown)
                         currentState
----------------------------------------------------------------

handlerMempoolTimeout :: TimeoutHandler UnknownState alg a m
handlerMempoolTimeout = currentState
----------------------------------------------------------------

handlerBlocksTimeout :: TimeoutHandler UnknownState alg a m
handlerBlocksTimeout = currentState

