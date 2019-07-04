{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs            #-}

module Thundermint.P2P.PeerState.Handle.Ahead ( handler ) where

import Control.Monad.RWS.Strict

import Lens.Micro.Mtl

import Thundermint.Blockchain.Internal.Types
import Thundermint.Types.Blockchain

import Thundermint.P2P.Internal.Types
import Thundermint.P2P.PeerState.Monad
import Thundermint.P2P.PeerState.Types
import Thundermint.P2P.PeerState.Handle.Utils


handler :: Handler Ahead alg a m
handler (EGossip gossipMsg) = do
  resendGossip gossipMsg
  case gossipMsg of
    GossipAnn (AnnStep step@(FullStep h _ _)) -> do
      -- Don't go back.
      s@(FullStep h0 _ _) <- use aheadPeerStep
      if step > s
        -- If update don't change height only advance step of peer
        then if h0 == h
          then aheadPeerStep .= step >> currentState
          else lift $ advancePeer step
        else currentState
    _ -> currentState

