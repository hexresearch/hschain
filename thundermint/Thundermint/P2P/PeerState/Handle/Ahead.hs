{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs            #-}
{-# LANGUAGE RecordWildCards  #-}

module Thundermint.P2P.PeerState.Handle.Ahead ( handler ) where

import Control.Monad.RWS.Strict

import Lens.Micro.Mtl

import Thundermint.Blockchain.Internal.Types
import Thundermint.Control                   (throwNothing)
import Thundermint.Debug.Trace
import Thundermint.Exceptions
import Thundermint.Store
import Thundermint.Types.Blockchain

import Thundermint.P2P.Internal.Types
import Thundermint.P2P.PeerState.Handle.Utils
import Thundermint.P2P.PeerState.Monad
import Thundermint.P2P.PeerState.Types

import qualified Data.Map.Strict as Map
import qualified Data.Set        as Set


handler :: Handler Ahead alg a m
handler =
  handlerGeneric
   handlerGossipMsg
   handlerAnnouncement
   handlerVotesTimeout
   handlerMempoolTimeout
   handlerBlocksTimeout

handlerGossipMsg :: MessageHandler AheadState alg a m
handlerGossipMsg gossipMsg = do
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

----------------------------------------------------------------

handlerAnnouncement :: AnnouncementHandler AheadState alg a m
handlerAnnouncement (TxAnn (AnnStep (FullStep ourH _ _))) = do
  step@(FullStep h _ _) <- use aheadPeerStep
  if h == ourH then
        do vals <- throwNothing (DBMissingValSet h) <=< lift $ queryRO
                 $ retrieveValidatorSet h
           return $ wrap $ CurrentState
             { _peerStep       = step
             , _peerValidators = vals
             , _peerPrevotes   = Map.empty
             , _peerPrecommits = Map.empty
             , _peerProposals  = Set.empty
             , _peerBlocks     = Set.empty
             }
     else currentState
handlerAnnouncement _ = currentState
----------------------------------------------------------------

handlerVotesTimeout :: TimeoutHandler AheadState alg a m
handlerVotesTimeout = do trace (TePeerGossipVotes TepgvAhead)
                         currentState
----------------------------------------------------------------

handlerMempoolTimeout :: TimeoutHandler AheadState alg a m
handlerMempoolTimeout = do
    MempoolCursor{..} <- view mempCursor
    lift advanceCursor >>= maybe (return ()) (push2Gossip . GossipTx)
    currentState
----------------------------------------------------------------

handlerBlocksTimeout :: TimeoutHandler AheadState alg a m
handlerBlocksTimeout = currentState

