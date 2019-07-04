{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs            #-}

module Thundermint.P2P.PeerState.Handle.Unknown ( handler ) where

import Control.Monad.RWS.Strict

import Thundermint.Blockchain.Internal.Types
import Thundermint.P2P.Internal.Types
import Thundermint.P2P.PeerState.Monad
import Thundermint.P2P.PeerState.Types

import Thundermint.P2P.PeerState.Handle.Utils

handler :: Handler Unknown alg a m
handler (EGossip gossipMsg) = do
  resendGossip gossipMsg
  case gossipMsg of
    GossipAnn (AnnStep step) -> lift $ advancePeer step
    _                        -> currentState


