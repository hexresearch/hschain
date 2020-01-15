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

import HSChain.P2P.Internal.Logging (GossipCounters(..))
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

advanceMempoolCursor :: (HandlerCtx a m)
                     => TransitionT s a m ()
advanceMempoolCursor = do
    MempoolCursor{..} <- view mempCursor
    mTx <- lift advanceCursor
    forM_ mTx $ \ tx' -> do
      push2Gossip $ GossipTx tx'
      tickSend tx

type MessageHandler s a m =  HandlerCtx a m
                          => GossipMsg a
                          -> TransitionT s a m ()

type AnnouncementHandler s a m =  HandlerCtx a m
                               => MessageTx a
                               -> TransitionT s a m ()

type AdvanceOurHeight s a m =  HandlerCtx a m
                            => FullStep
                            -> TransitionT s a m ()

type TimeoutHandler s a m = (Wrapable s, HandlerCtx a m)
                         => TransitionT s a m ()

data HandlerDict s a m = HandlerDict
  { handlerGossipMsg      :: GossipMsg a -> TransitionT s a m ()
  , handlerVotesTimeout   :: TransitionT s a m ()
  , handlerMempoolTimeout :: TransitionT s a m ()
  , handlerBlocksTimeout  :: TransitionT s a m ()
  }

handlerGeneric :: (Wrapable s, HandlerCtx a m)
               => HandlerDict s a m
               -> Event a
               -> TransitionT s a m ()
handlerGeneric HandlerDict{..} = \ case
    EGossip m        -> do resendGossip m
                           handlerGossipMsg m
    EAnnouncement a  -> handlerAnnounncement a
    EVotesTimeout    -> handlerVotesTimeout
    EMempoolTimeout  -> handlerMempoolTimeout
    EBlocksTimeout   -> handlerBlocksTimeout
    EAnnounceTimeout -> handlerAnnounceTimeout
  where
    handlerAnnounceTimeout :: TimeoutHandler s a m
    handlerAnnounceTimeout = do
      st <- atomicallyIO =<< view consensusSt
      forM_ st $ \(h,TMState{smRound,smStep}) -> do
        push2Gossip $ GossipAnn $ AnnStep $ FullStep h smRound smStep
        case smStep of
          StepAwaitCommit r -> push2Gossip $ GossipAnn $ AnnHasProposal h r
          _                 -> return ()
    --
    handlerAnnounncement e =
      case e of
        TxAnn       a -> push2Gossip $ GossipAnn a
        TxProposal  p -> push2Gossip $ GossipProposal  p
        TxPreVote   v -> push2Gossip $ GossipPreVote   v
        TxPreCommit v -> push2Gossip $ GossipPreCommit v

issuedGossipHandlerGeneric :: (Wrapable s, HandlerCtx a m)
         => MessageHandler s a m
         -> AdvanceOurHeight s a m
         -> GossipMsg a
         -> TransitionT s a m ()
issuedGossipHandlerGeneric
  handlerGossipMsg
  advanceOurHeight
  m = case m of
    GossipProposal {}     -> handlerGossipMsg m
    GossipPreVote {}      -> handlerGossipMsg m
    GossipPreCommit {}    -> handlerGossipMsg m
    GossipBlock {}        -> handlerGossipMsg m
    GossipAnn (AnnStep s) -> advanceOurHeight s
    _                     -> return ()

