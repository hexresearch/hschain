{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs            #-}

module Thundermint.P2P.PeerState.Handle.Unknown ( handler ) where

import Control.Monad.RWS.Strict

import Thundermint.Blockchain.Internal.Types
import Thundermint.Debug.Trace
import Thundermint.P2P.Internal.Types
import Thundermint.P2P.PeerState.Monad
import Thundermint.P2P.PeerState.Types

import Thundermint.P2P.PeerState.Handle.Utils

handler :: Handler Unknown alg a m
handler =
  handlerGeneric
   handlerGossipMsg
   handlerAnnouncement
   handlerVotesTimeout
   handlerMempoolTimeout
   handlerBlocksTimeout

handlerGossipMsg :: MessageHandler UnknownState alg a m
handlerGossipMsg gossipMsg = do
  resendGossip gossipMsg
  case gossipMsg of
    GossipAnn (AnnStep step) -> lift $ advancePeer step
    _                        -> currentState

----------------------------------------------------------------

handlerAnnouncement :: AnnouncementHandler UnknownState alg a m
handlerAnnouncement = const currentState
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

