{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module HSChain.P2P.PeerState.Handle.Utils where

import qualified Data.Map as Map
import qualified Data.Set as Set

import Control.Monad.State.Strict

import HSChain.Crypto
import HSChain.Store
import HSChain.Store.Internal.BlockDB
import HSChain.Types.Blockchain
import HSChain.Types.Validators
import HSChain.Internal.Types.Consensus
import HSChain.P2P.Internal.Types
import HSChain.P2P.PeerState.Monad
import HSChain.P2P.PeerState.Types

-- | Unconditionally generate fresh state for peer. Shoudl only be called when we 
advancePeer :: (Crypto (Alg a), Monad m, MonadIO m, MonadReadDB m, MonadCached a m)
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


-- | Dictionary of handlers for messages for each state of peer.
data HandlerDict s view m = HandlerDict
  { handlerGossipMsg        :: Config view
                            -> GossipMsg (BlockType view)
                            -> TransitionT s (BlockType view) m ()
    -- ^ Handler for incoming gossip. It's used for outgoing gossip as
    -- well with minor modifications.
  , advanceOurHeight        :: FullStep -> TransitionT s (BlockType view) m ()
    -- ^ Handler for outgoing 'AnnStep'
  , handlerProposalTimeout  :: Config view -> s (BlockType view) -> m [GossipMsg (BlockType view)]
  , handlerPrevoteTimeout   :: Config view -> s (BlockType view) -> m [GossipMsg (BlockType view)]
  , handlerPrecommitTimeout :: Config view -> s (BlockType view) -> m [GossipMsg (BlockType view)]
  , handlerBlocksTimeout    :: Config view -> s (BlockType view) -> m [GossipMsg (BlockType view)]
  }
