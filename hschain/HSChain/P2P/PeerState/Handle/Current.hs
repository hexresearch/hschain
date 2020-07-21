{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE ViewPatterns      #-}

module HSChain.P2P.PeerState.Handle.Current
  ( handler
  ) where

import Control.Monad
import Control.Monad.RWS.Strict
import Data.Foldable            (toList)
import Data.Maybe
import System.Random            (randomRIO)

import Lens.Micro
import Lens.Micro.Mtl

import HSChain.Blockchain.Internal.Types
import HSChain.Control.Util              (atomicallyIO)
import HSChain.Crypto.Containers         (toPlainMap)
import HSChain.Internal.Types.Messages
import HSChain.Store
import HSChain.Store.Internal.Proposals
import HSChain.Store.Internal.BlockDB
import HSChain.Types.Blockchain
import HSChain.Types.Validators

import HSChain.P2P.Internal.Types
import HSChain.P2P.PeerState.Monad
import HSChain.P2P.PeerState.Types

import HSChain.P2P.PeerState.Handle.Utils

import qualified HSChain.Data.CIntMap as CIMap
import qualified Data.Map.Strict    as Map
import qualified Data.Set           as Set


handler :: (BlockData a, HandlerCtx a m) => HandlerDict CurrentState a m
handler = HandlerDict
  { handlerGossipMsg        = handlerGossip
  , advanceOurHeight        = advanceOurHeightWrk
  , handlerProposalTimeout  = handlerProposalTimeoutMsg
  , handlerPrevoteTimeout   = handlerPrevoteTimeoutMsg
  , handlerPrecommitTimeout = handlerPrecommitsTimeoutMsg
  , handlerBlocksTimeout    = handlerBlocksTimeoutMsg
  }


handlerGossip
  :: (MonadIO m, MonadReadDB m, MonadCached a m, BlockData a)
  => Config n a -> GossipMsg a -> TransitionT CurrentState a m ()
handlerGossip cfg = \case
  GossipPreVote v@(signedValue -> Vote{..}) -> do
    addPrevote voteHeight voteRound $ signedKeyInfo v
  GossipPreCommit v@(signedValue -> Vote{..}) -> do
    addPrecommit voteHeight voteRound $ signedKeyInfo v
  GossipProposal (signedValue -> Proposal{..}) -> do
    addProposal propHeight propRound
  GossipBlock b -> addBlock (blockHash b)
  GossipTx{}    -> return ()
  GossipPex{}   -> return ()
  GossipAnn ann -> case ann of
    AnnStep step@(FullStep h _ _) -> do
      -- Don't go back.
      s@(FullStep h0 _ _) <- use peerStep
      if | h    > h0 -> advancePeer step
         -- If update don't change height only advance step of peer
         | step > s  -> peerStep .= step
         | otherwise -> return ()
    AnnHasProposal  h r   -> addProposal h r
    AnnHasPreVote   h r i -> addPrevote h r i
    AnnHasPreCommit h r i -> addPrecommit h r i
    AnnHasBlock     h r   -> do
      FullStep hPeer _ _ <- use peerStep
      when ( h == hPeer ) $ do
        mstate <- atomicallyIO $ view consensusSt cfg
        forM_ mstate $ \(hSt, st) ->
          when (hPeer == hSt) $ do
            forM_ (proposalByR (smProposedBlocks st) r) $ \(bid,_) ->
              peerBlocks %= Set.insert bid
    AnnLock mr -> peerLock .= mr


addProposal :: MonadState (CurrentState a) m
            => Height -> Round -> m ()
addProposal h r = do
  FullStep hPeer _ _ <- use peerStep
  when (h == hPeer) $
    peerProposals %= Set.insert r

addPrevote :: MonadState (CurrentState a) m
           => Height -> Round -> ValidatorIdx alg -> m ()
addPrevote h r idx = do
  FullStep hPeer _ _ <- use peerStep
  when (h == hPeer) $ do
    vals <- use peerValidators
    peerPrevotes %= Map.alter ( Just
                              . insertValidatorIdx idx
                              . fromMaybe (emptyValidatorISet vals)
                              ) r


addPrecommit :: MonadState (CurrentState a) m
             => Height -> Round -> ValidatorIdx alg -> m ()
addPrecommit h r idx = do
  FullStep hPeer _ _ <- use peerStep
  when (h == hPeer) $ do
    vals <- use peerValidators
    peerPrecommits %= Map.alter ( Just
                                . insertValidatorIdx idx
                                . fromMaybe (emptyValidatorISet vals)
                                ) r

addBlock :: (MonadState (CurrentState a) m)
         => BlockID a -> m ()
addBlock bid = peerBlocks %= Set.insert bid

----------------------------------------------------------------

advanceOurHeightWrk :: (HandlerCtx a m) => FullStep -> TransitionT CurrentState a m ()
advanceOurHeightWrk (FullStep ourH _ _) = setFinalState advance
  where
    advance p
      -- When our height advances peer couldn't become Ahed
      | h >= ourH = return $ wrap p
      -- Otherwise it becomes lagging
      | otherwise = do
          vals <- queryRO $ mustRetrieveValidatorSet h
          r    <- queryRO $ mustRetrieveCommitRound  h
          bid  <- queryRO $ mustRetrieveBlockID      h
          return $ wrap $ LaggingState
            { _lagPeerStep        = _peerStep p
            , _lagPeerCommitR     = r
            , _lagPeerValidators  = vals
            , _lagPeerPrecommits  = emptyValidatorISet vals
            , _lagPeerHasProposal = r   `Set.member` _peerProposals p
            , _lagPeerHasBlock    = bid `Set.member` _peerBlocks p
            , _lagPeerBlockID     = bid
            }
      where
        FullStep h _ _ = _peerStep p

----------------------------------------------------------------

handlerProposalTimeoutMsg
  :: (MonadIO m, MonadReadDB m, MonadCached a m)
  => Config n a -> CurrentState a -> m [GossipMsg a]
handlerProposalTimeoutMsg cfg st = do
  bchH <- queryRO blockchainHeight
  atomicallyIO (view consensusSt cfg) >>= \case
    Nothing                       -> return []
    Just (h',_) | h' /= succ bchH -> return []
    Just (_,tm)
      | noRoundInProposals
      , Just prop <- r `Map.lookup` smProposals tm
        -> return [GossipProposal $ unverifySignature prop]
      | otherwise
        -> return []
  where
    FullStep _ r _     = st ^. peerStep
    noRoundInProposals = Set.notMember r $ st ^. peerProposals

handlerPrevoteTimeoutMsg
  :: (MonadIO m, MonadReadDB m, MonadCached a m)
  => Config n a -> CurrentState a -> m [GossipMsg a]
handlerPrevoteTimeoutMsg cfg st
  | Just polR <- st^.peerLock = gossipPrevotes cfg st polR
  | otherwise                 = gossipPrevotes cfg st r
  where
    FullStep _ r _ = st ^. peerStep

gossipPrevotes
  :: (MonadIO m, MonadReadDB m, MonadCached a m)
  => Config n a -> CurrentState a -> Round -> m [GossipMsg a]
gossipPrevotes cfg st r = do
  bchH <- queryRO blockchainHeight
  atomicallyIO (view consensusSt cfg) >>= \case
    Nothing                       -> return []
    Just (h',_) | h' /= succ bchH -> return []
    Just (_,tm)
      | Just localPV <- Map.lookup r $ toPlainMap $ smPrevotesSet tm
      , unknown      <- CIMap.difference localPV peerPV
      , n            <- CIMap.size unknown
      , n > 0
        -> do i <- liftIO $ randomRIO (0,n-1)
              let vote = unverifySignature $ toList unknown !! i
              return [GossipPreVote vote]
      | otherwise ->
          return []
  where
    peerPV = maybe CIMap.empty (CIMap.fromSet (const ()) . getValidatorIntSet)
           $ Map.lookup r
           $ st^.peerPrevotes

  

handlerPrecommitsTimeoutMsg
  :: (MonadIO m, MonadReadDB m, MonadCached a m)
  => Config n a -> CurrentState a -> m [GossipMsg a]
handlerPrecommitsTimeoutMsg cfg st = do
  bchH <- queryRO blockchainHeight
  atomicallyIO (view consensusSt cfg) >>= \case
    Nothing                       -> return []
    Just (h',_) | h' /= succ bchH -> return []
    Just (_,tm)
      | Just localPC <- Map.lookup r $ toPlainMap $ smPrecommitsSet tm
      , unknown      <- CIMap.difference localPC peerPC
      , n            <- CIMap.size unknown
      , n > 0
        -> do i <- liftIO $ randomRIO (0,n-1)
              let vote = unverifySignature $ toList unknown !! i
              return [GossipPreCommit vote]
      | otherwise ->
          return []
  where
    FullStep _ r _ = st ^. peerStep
    peerPC = maybe CIMap.empty (CIMap.fromSet (const ()) . getValidatorIntSet)
           $ Map.lookup r
           $ st^.peerPrecommits


handlerBlocksTimeoutMsg
  :: (MonadIO m, MonadReadDB m, MonadCached a m)
  => Config n a -> CurrentState a -> m [GossipMsg a]
handlerBlocksTimeoutMsg cfg st = do
  bchH <- queryRO blockchainHeight
  atomicallyIO (view consensusSt cfg) >>= \case
    Nothing                       -> return []
    Just (h',_) | h' /= succ bchH -> return []
    Just (_,tm) -> return $ do
      let mBlk = do (bid, bVal) <- proposalByR (smProposedBlocks tm) r
                    blk         <- blockFromBlockValidation bVal
                    return (bid,blk)
      case mBlk of
        Nothing      -> []
        Just (bid,b)
          -- Peer has proposal but not block
          | Set.member r $ st^.peerProposals
          , Set.notMember bid $ st^.peerBlocks
            -> [GossipBlock b]
          | otherwise
            -> []
  where
    FullStep _ r _   = st^.peerStep

