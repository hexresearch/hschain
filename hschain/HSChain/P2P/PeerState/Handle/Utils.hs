{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module HSChain.P2P.PeerState.Handle.Utils where

import qualified Data.Map as Map
import qualified Data.Set as Set

import Control.Monad
import Control.Monad.RWS.Strict
import Lens.Micro.Mtl

import HSChain.Blockchain.Internal.Types
import HSChain.Control (atomicallyIO)
import HSChain.Crypto
import HSChain.Store
import HSChain.Store.Internal.BlockDB
import HSChain.Types.Blockchain
import HSChain.Types.Validators

import HSChain.Store.Internal.Query (MonadReadDB)

import HSChain.P2P.Internal.Types
import HSChain.P2P.PeerState.Monad
import HSChain.P2P.PeerState.Types

advancePeer :: (Crypto (Alg a), Monad m, MonadIO m, MonadReadDB m a)
            => FullStep -> TransitionT s a m ()
advancePeer step@(FullStep h _ _) = setFinalState $ \_ -> do
  ourH <- succ <$> queryRO blockchainHeight
  case compare h ourH of
    LT -> do vals <- queryRO $ mustRetrieveValidatorSet h
             cmtR <- queryRO $ mustRetrieveCommitRound  h
             bid  <- queryRO $ mustRetrieveBlockID      h
             return $ wrap $ LaggingState
               { _lagPeerStep        = step
               , _lagPeerCommitR     = cmtR
               , _lagPeerValidators  = vals
               , _lagPeerPrecommits  = emptyValidatorISet vals
               , _lagPeerHasProposal = False
               , _lagPeerHasBlock    = False
               , _lagPeerBlockID     = bid
               }
    EQ -> do vals <- queryRO $ mustRetrieveValidatorSet h
             return $ wrap $ CurrentState
               { _peerStep       = step
               , _peerValidators = vals
               , _peerPrevotes   = Map.empty
               , _peerPrecommits = Map.empty
               , _peerProposals  = Set.empty
               , _peerBlocks     = Set.empty
               , _peerLock       = Nothing
               }
    GT -> return $ wrap $ AheadState step


type MessageHandler s a m =  HandlerCtx a m
                          => GossipMsg a
                          -> TransitionT s a m ()

type AnnouncementHandler s a m =  HandlerCtx a m
                               => MessageTx a
                               -> TransitionT s a m ()

type AdvanceOurHeight s a m =  HandlerCtx a m
                            => FullStep
                            -> TransitionT s a m ()

type TimeoutHandler s a m = HandlerCtx a m
                         => TransitionT s a m ()

data HandlerDict s a m = HandlerDict
  { handlerGossipMsg        :: Config a -> GossipMsg a -> TransitionT s a m ()
    -- ^ Handler for incoming /and/ outgoing gossip. We handle both
    --   identically except for 'AnnStep'. This function handles
    --   incoming gossip.
  , advanceOurHeight        :: FullStep -> TransitionT s a m ()
    -- ^ Handler for outgoing 'AnnStep'
  , handlerProposalTimeout  :: Config a -> s a -> m [GossipMsg a]
  , handlerPrevoteTimeout   :: Config a -> s a -> m [GossipMsg a]
  , handlerPrecommitTimeout :: Config a -> s a -> m [GossipMsg a]
  , handlerBlocksTimeout    :: Config a -> s a -> m [GossipMsg a]
  }
