{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs            #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE RecordWildCards  #-}

module HSChain.P2P.PeerState.Handle.Ahead
  ( handler
  , issuedGossipHandler
  ) where

import Control.Monad.RWS.Strict
import Lens.Micro.Mtl

import HSChain.Blockchain.Internal.Types
import HSChain.Store
import HSChain.Types.Blockchain

import HSChain.P2P.Internal.Types
import HSChain.P2P.PeerState.Handle.Utils
import HSChain.P2P.PeerState.Monad
import HSChain.P2P.PeerState.Types

import qualified Data.Map.Strict as Map
import qualified Data.Set        as Set


handler :: Handler AheadState Event a m
handler =
  handlerGeneric
   handlerGossipMsg
   handlerVotesTimeout
   handlerMempoolTimeout
   handlerBlocksTimeout

issuedGossipHandler :: Handler AheadState GossipMsg a m
issuedGossipHandler =
  issuedGossipHandlerGeneric
    handlerGossipMsg
    advanceOurHeignt

handlerGossipMsg :: MessageHandler AheadState a m
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
advanceOurHeignt :: AdvanceOurHeight AheadState a m
advanceOurHeignt (FullStep ourH _ _) = do
  step@(FullStep h _ _) <- use aheadPeerStep
  if h == ourH then
        do vals <- lift $ queryRO $ mustRetrieveValidatorSet h
           return $ wrap $ CurrentState
             { _peerStep       = step
             , _peerValidators = vals
             , _peerPrevotes   = Map.empty
             , _peerPrecommits = Map.empty
             , _peerProposals  = Set.empty
             , _peerBlocks     = Set.empty
             , _peerLock       = Nothing
             }
     else currentState
----------------------------------------------------------------

handlerVotesTimeout :: TimeoutHandler AheadState a m
handlerVotesTimeout = currentState

----------------------------------------------------------------

handlerMempoolTimeout :: TimeoutHandler AheadState a m
handlerMempoolTimeout = do
  advanceMempoolCursor
  currentState
----------------------------------------------------------------

handlerBlocksTimeout :: TimeoutHandler AheadState a m
handlerBlocksTimeout = currentState

