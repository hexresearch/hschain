{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs            #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE RecordWildCards  #-}

module Thundermint.P2P.PeerState.Handle.Ahead
  ( handler
  , issuedGossipHandler
  ) where

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


handler :: Handler Ahead Event alg a m
handler =
  handlerGeneric
   handlerGossipMsg
   handlerVotesTimeout
   handlerMempoolTimeout
   handlerBlocksTimeout

issuedGossipHandler :: Handler Ahead GossipMsg alg a m
issuedGossipHandler =
  issuedGossipHandlerGeneric
    handlerGossipMsg
    advanceOurHeignt

handlerGossipMsg :: MessageHandler AheadState alg a m
handlerGossipMsg = \case
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
advanceOurHeignt :: AdvanceOurHeight AheadState alg a m
advanceOurHeignt (FullStep ourH _ _) = do
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
----------------------------------------------------------------

handlerVotesTimeout :: TimeoutHandler AheadState alg a m
handlerVotesTimeout = do trace (TePeerGossipVotes TepgvAhead)
                         currentState
----------------------------------------------------------------

handlerMempoolTimeout :: TimeoutHandler AheadState alg a m
handlerMempoolTimeout = do
  advanceMempoolCursor
  currentState
----------------------------------------------------------------

handlerBlocksTimeout :: TimeoutHandler AheadState alg a m
handlerBlocksTimeout = currentState

