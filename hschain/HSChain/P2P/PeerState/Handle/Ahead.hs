{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs            #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE RecordWildCards  #-}

module HSChain.P2P.PeerState.Handle.Ahead
  ( handler
  , issuedGossipHandler
  ) where

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


handler :: HandlerCtx a m => HandlerDict AheadState a m
handler = HandlerDict
  { handlerGossipMsg      = handlerGossip
  , handlerVotesTimeout   = handlerVotesTimeoutMsg
  , handlerMempoolTimeout = handlerMempoolTimeoutMsg
  , handlerBlocksTimeout  = handlerBlocksTimeoutMsg
  }

issuedGossipHandler :: Handler AheadState GossipMsg a m
issuedGossipHandler =
  issuedGossipHandlerGeneric
    handlerGossip
    advanceOurHeignt

handlerGossip :: MessageHandler AheadState a m
handlerGossip = \case
    GossipAnn (AnnStep step@(FullStep h _ _)) -> do
      -- Don't go back.
      s@(FullStep h0 _ _) <- use aheadPeerStep
      if step > s
        -- If update don't change height only advance step of peer
        then if h0 == h
          then aheadPeerStep .= step
          else advancePeer step
        else return ()
    _ -> return ()

----------------------------------------------------------------
advanceOurHeignt :: AdvanceOurHeight AheadState a m
advanceOurHeignt (FullStep ourH _ _) = setFinalState advance
  where
    advance p
      | h == ourH = do
          vals <- queryRO $ mustRetrieveValidatorSet h
          return $ wrap $ CurrentState
            { _peerStep       = step
            , _peerValidators = vals
            , _peerPrevotes   = Map.empty
            , _peerPrecommits = Map.empty
            , _peerProposals  = Set.empty
            , _peerBlocks     = Set.empty
            , _peerLock       = Nothing
            }
      | otherwise = return $ wrap p
      where
        step@(FullStep h _ _) = _aheadPeerStep p


----------------------------------------------------------------

handlerVotesTimeoutMsg :: TimeoutHandler AheadState a m
handlerVotesTimeoutMsg = return ()

----------------------------------------------------------------

handlerMempoolTimeoutMsg :: TimeoutHandler AheadState a m
handlerMempoolTimeoutMsg = advanceMempoolCursor

----------------------------------------------------------------

handlerBlocksTimeoutMsg :: TimeoutHandler AheadState a m
handlerBlocksTimeoutMsg = return ()

