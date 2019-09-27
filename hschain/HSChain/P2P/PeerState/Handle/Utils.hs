{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module HSChain.P2P.PeerState.Handle.Utils where

import qualified Data.Map as Map
import qualified Data.Set as Set

import Control.Concurrent.STM   (atomically)
import Control.Monad
import Control.Monad.RWS.Strict
import Lens.Micro.Mtl

import HSChain.Blockchain.Internal.Types
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

advancePeer :: (CryptoSign alg, CryptoHash alg, Monad m, MonadIO m, MonadReadDB m alg a)
            => FullStep -> m (State alg a)
advancePeer step@(FullStep h _ _) = do
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
                        }
             GT -> return $ wrap $ AheadState step

advanceMempoolCursor :: (HandlerCtx alg a m)
                     => TransitionT s alg a m ()
advanceMempoolCursor = do
    MempoolCursor{..} <- view mempCursor
    mTx <- lift advanceCursor
    forM_ mTx $ \ tx' -> do
      push2Gossip $ GossipTx tx'
      tickSend tx

type MessageHandler s alg a m =  HandlerCtx alg a m
                              => GossipMsg alg a
                              -> TransitionT s alg a m (State alg a)

type AnnouncementHandler s alg a m =  HandlerCtx alg a m
                                   => MessageTx alg a
                                   -> TransitionT s alg a m (State alg a)

type AdvanceOurHeight s alg a m =  HandlerCtx alg a m
                                => FullStep
                                -> TransitionT s alg a m (State alg a)

type TimeoutHandler s alg a m =  (Wrapable s, HandlerCtx alg a m)
                              => TransitionT s alg a m (State alg a)

handlerGeneric :: (Wrapable s, HandlerCtx alg a m)
               => MessageHandler s alg a m
               -> TimeoutHandler s alg a m
               -> TimeoutHandler s alg a m
               -> TimeoutHandler s alg a m
               -> Event alg a
               -> TransitionT s alg a m (State alg a)
handlerGeneric
  hanldlerGossipMsg
  handlerVotesTimeout
  handlerMempoolTimeout
  handlerBlocksTimeout = \ case
    EGossip m        -> do resendGossip m
                           hanldlerGossipMsg m
    EAnnouncement a  -> handlerAnnounncement a
    EVotesTimeout    -> handlerVotesTimeout
    EMempoolTimeout  -> handlerMempoolTimeout
    EBlocksTimeout   -> handlerBlocksTimeout
    EAnnounceTimeout -> handlerAnnounceTimeout
  where
    handlerAnnounceTimeout :: TimeoutHandler s alg a m
    handlerAnnounceTimeout = do
      st <- view consensusSt >>= lift . liftIO . atomically
      forM_ st $ \(h,TMState{smRound,smStep}) -> do
        push2Gossip $ GossipAnn $ AnnStep $ FullStep h smRound smStep
        case smStep of
          StepAwaitCommit r -> push2Gossip $ GossipAnn $ AnnHasProposal h r
          _                 -> return ()
      currentState
    --
    handlerAnnounncement e = do
      case e of
        TxAnn       a -> push2Gossip $ GossipAnn a
        TxProposal  p -> push2Gossip $ GossipProposal  p
        TxPreVote   v -> push2Gossip $ GossipPreVote   v
        TxPreCommit v -> push2Gossip $ GossipPreCommit v
      currentState

issuedGossipHandlerGeneric :: (Wrapable s, HandlerCtx alg a m)
         => MessageHandler s alg a m
         -> AdvanceOurHeight s alg a m
         -> GossipMsg alg a
         -> TransitionT s alg a m (State alg a)
issuedGossipHandlerGeneric
  handlerGossipMsg
  advanceOurHeight
  m = case m of
    GossipProposal {}     -> handlerGossipMsg m
    GossipPreVote {}      -> handlerGossipMsg m
    GossipPreCommit {}    -> handlerGossipMsg m
    GossipBlock {}        -> handlerGossipMsg m
    GossipAnn (AnnStep s) -> advanceOurHeight s
    _                     -> currentState

