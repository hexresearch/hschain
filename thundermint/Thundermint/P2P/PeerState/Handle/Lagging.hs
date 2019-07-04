{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs            #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE ViewPatterns     #-}

module Thundermint.P2P.PeerState.Handle.Lagging ( handler ) where

import Control.Monad
import Control.Monad.RWS.Strict

import Lens.Micro.Mtl

import Thundermint.Blockchain.Internal.Types
import Thundermint.Crypto
import Thundermint.Types.Blockchain
import Thundermint.Types.Validators

import Thundermint.P2P.Internal.Types
import Thundermint.P2P.PeerState.Monad
import Thundermint.P2P.PeerState.Types
import Thundermint.P2P.PeerState.Handle.Utils

handler :: Handler Lagging alg a m
handler (EGossip gossipMsg) = do
  resendGossip gossipMsg
  case gossipMsg of
    GossipPreCommit v@(signedValue -> Vote{..}) -> do
      addPrecommit voteHeight voteRound $ signedKeyInfo v
      currentState
    GossipProposal (signedValue -> Proposal{..}) -> do
      addProposal propHeight propRound
      currentState
    GossipBlock b -> do
      peerBid <- use lagPeerBlockID
      when (blockHash b == peerBid) $ lagPeerHasBlock .= True
      currentState
    GossipAnn ann -> case ann of
      AnnStep step@(FullStep h _ _) -> do
        -- Don't go back.
        s@(FullStep h0 _ _) <- use lagPeerStep
        if step > s
          -- If update don't change height only advance step of peer
          then if h0 == h
             then lagPeerStep .= step >> currentState
             else lift $ advancePeer step -- TODO: IO read from DB
          else currentState
      AnnHasProposal  h r   -> do
        addProposal h r
        currentState
      AnnHasPreVote   {} ->
        currentState
      AnnHasPreCommit h r i -> do
        addPrecommit h r i
        currentState
      AnnHasBlock     h r   -> do
        (FullStep hP _ _) <- use lagPeerStep
        peerCommitRound <- use lagPeerCommitR
        when ( h == hP && r == peerCommitRound) $
          lagPeerHasBlock .= True
        currentState
    _ -> currentState

addProposal :: MonadState (LaggingState alg a) m
                   => Height -> Round -> m ()
addProposal h r = do
      (FullStep peerHeihgt _ _) <- use lagPeerStep
      peerRound                 <- use lagPeerCommitR
      when (h == peerHeihgt && r == peerRound) $
         lagPeerHasProposal .= True

addPrecommit :: MonadState (LaggingState alg a) m
                   => Height -> Round -> ValidatorIdx alg -> m ()
addPrecommit h r idx = do
      (FullStep hPeer _ _) <- use lagPeerStep
      peerRound            <- use lagPeerCommitR
      when (h == hPeer && r == peerRound) $
        lagPeerPrecommits %= insertValidatorIdx idx
