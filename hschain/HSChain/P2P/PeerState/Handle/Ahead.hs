{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs            #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE RecordWildCards  #-}

module HSChain.P2P.PeerState.Handle.Ahead
  ( handler
  , issuedGossipHandler
  ) where

import Control.Monad
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

issuedGossipHandler :: HandlerCtx a m => IssuedDict AheadState a m
issuedGossipHandler = IssuedDict
  { handlerIssuedGossip = handlerGossip
  , advanceOurHeight    = advanceOurHeightWrk
  }

handlerGossip :: MessageHandler AheadState a m
handlerGossip = \case
    GossipAnn (AnnStep step) -> do
      s <- use aheadPeerStep
      -- If peer is ahead of us and advances it could only remain ahed
      when (step > s) $ aheadPeerStep .= step
    _ -> return ()

----------------------------------------------------------------
advanceOurHeightWrk :: AdvanceOurHeight AheadState a m
advanceOurHeightWrk (FullStep ourH _ _) = setFinalState advance
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

