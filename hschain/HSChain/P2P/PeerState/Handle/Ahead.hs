{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs            #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE RecordWildCards  #-}

module HSChain.P2P.PeerState.Handle.Ahead
  ( handler
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
  { handlerGossipMsg        = const handlerGossip
  , advanceOurHeight        = advanceOurHeightWrk
  , handlerProposalTimeout  = \_ _ -> return []
  , handlerPrevoteTimeout   = \_ _ -> return []
  , handlerPrecommitTimeout = \_ _ -> return []
  , handlerBlocksTimeout    = \_ _ -> return []
  }

handlerGossip
  :: (Monad m)
  => GossipMsg a -> TransitionT AheadState a m ()
handlerGossip = \case
  GossipAnn (AnnStep step) -> do
    s <- use aheadPeerStep
    when (step > s) $ aheadPeerStep .= step
  _ -> return ()


----------------------------------------------------------------
advanceOurHeightWrk :: (HandlerCtx a m) => FullStep -> TransitionT AheadState a m ()
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

