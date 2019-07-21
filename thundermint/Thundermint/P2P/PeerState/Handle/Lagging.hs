{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs            #-}
{-# LANGUAGE MultiWayIf       #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE ViewPatterns     #-}

module Thundermint.P2P.PeerState.Handle.Lagging ( handler ) where

import Control.Monad
import Control.Monad.RWS.Strict
import Data.Foldable            (toList)
import System.Random            (randomRIO)

import Lens.Micro.Mtl

import Thundermint.Blockchain.Internal.Types
import Thundermint.Control
import Thundermint.Crypto
import Thundermint.Debug.Trace
import Thundermint.Exceptions
import Thundermint.Store
import Thundermint.Types.Blockchain
import Thundermint.Types.Validators

import Thundermint.P2P.Internal.Types
import Thundermint.P2P.PeerState.Handle.Utils
import Thundermint.P2P.PeerState.Monad
import Thundermint.P2P.PeerState.Types

import qualified Data.IntSet        as ISet
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict    as Map

handler :: Handler Lagging alg a m
handler =
  handlerGeneric
   handlerGossipMsg
   handlerAnnouncement
   handlerVotesTimeout
   handlerMempoolTimeout
   handlerBlocksTimeout

handlerGossipMsg :: MessageHandler LaggingState alg a m
handlerGossipMsg gossipMsg = do
  resendGossip gossipMsg
  case gossipMsg of
    GossipPreCommit v@(signedValue -> Vote{..}) -> do
      addPrecommit voteHeight voteRound $ signedKeyInfo v
      currentState
    GossipProposal (signedValue -> Proposal{..}) -> do
      addProposal propHeight propRound
      currentState
    GossipBlock b -> do
      addBlock b
      currentState
    GossipAnn ann -> case ann of
      AnnStep step@(FullStep h _ _) -> do
        -- Don't go back.
        s@(FullStep h0 _ _) <- use lagPeerStep
        if step > s
          -- If update don't change height only advance step of peer
          then if h0 == h
             then lagPeerStep .= step >> currentState
             else lift $ advancePeer step -- TODO: IO read from DB
          else currentState
      AnnHasProposal  h r   -> do
        addProposal h r
        currentState
      AnnHasPreVote   {} ->
        currentState
      AnnHasPreCommit h r i -> do
        addPrecommit h r i
        currentState
      AnnHasBlock     h r   -> do
        (FullStep hP _ _) <- use lagPeerStep
        peerCommitRound <- use lagPeerCommitR
        when ( h == hP && r == peerCommitRound) $
          lagPeerHasBlock .= True
        currentState
    _ -> currentState

addProposal :: MonadState (LaggingState alg a) m
                   => Height -> Round -> m ()
addProposal h r = do
      (FullStep peerHeihgt _ _) <- use lagPeerStep
      peerRound                 <- use lagPeerCommitR
      when (h == peerHeihgt && r == peerRound) $
         lagPeerHasProposal .= True

addPrecommit :: MonadState (LaggingState alg a) m
                   => Height -> Round -> ValidatorIdx alg -> m ()
addPrecommit h r idx = do
      (FullStep hPeer _ _) <- use lagPeerStep
      peerRound            <- use lagPeerCommitR
      when (h == hPeer && r == peerRound) $
        lagPeerPrecommits %= insertValidatorIdx idx

addBlock :: (MonadState (LaggingState alg a) m, CryptoHash alg, CryptoSign alg)
         => Block alg a -> m ()
addBlock b = do
      peerBid <- use lagPeerBlockID
      when (blockHash b == peerBid) $ lagPeerHasBlock .= True
----------------------------------------------------------------

handlerAnnouncement :: AnnouncementHandler LaggingState alg a m
handlerAnnouncement = const currentState
----------------------------------------------------------------

handlerVotesTimeout :: TimeoutHandler LaggingState alg a m
handlerVotesTimeout = do
  bchH      <- lift $ queryRO blockchainHeight
  trace (TePeerGossipVotes TepgvNewIter)
  trace (TePeerGossipVotes TepgvLagging)
  (FullStep peerH _ _) <- use lagPeerStep
  mcmt <- lift $ queryRO $
    if peerH == bchH
       then retrieveLocalCommit peerH
       else retrieveCommit      peerH
  --
  forM_ mcmt $ \cmt -> do
     let cmtVotes  = Map.fromList [ (signedKeyInfo v, unverifySignature v)
                                  | v <- NE.toList (commitPrecommits cmt) ]
     peerVotes <- Map.fromList
                . map (\x -> (ValidatorIdx x,()))
                . ISet.toList
                . getValidatorIntSet
              <$> use lagPeerPrecommits
     let unknown   = Map.difference cmtVotes peerVotes
         n         = Map.size unknown
     when (n>0) $
       do i <- lift $ liftIO $ randomRIO (0,n-1) -- TODO: move RndGen to Config or state
          let vote@(signedValue -> Vote{..}) = unverifySignature $ toList unknown !! i
          addPrecommit voteHeight voteRound $ signedKeyInfo vote
          push2Gossip $ GossipPreCommit vote
          --tickSend $ precommit gossipCnts
  currentState
      --
----------------------------------------------------------------

handlerMempoolTimeout :: TimeoutHandler LaggingState alg a m
handlerMempoolTimeout = currentState
----------------------------------------------------------------

handlerBlocksTimeout :: TimeoutHandler LaggingState alg a m
handlerBlocksTimeout = do
  hasProp    <- use lagPeerHasProposal
  hasNoBlock <- not <$> use lagPeerHasBlock
  when (hasProp && hasNoBlock) $ do
    (FullStep h _ _) <- use lagPeerStep
    b <- lift $ throwNothing (DBMissingBlock h) <=< queryRO
              $ retrieveBlock h
    addBlock b
    push2Gossip $ GossipBlock b
    --tickSend $ blocks gossipCnts
  currentState

