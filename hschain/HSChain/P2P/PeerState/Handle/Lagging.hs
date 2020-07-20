{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs            #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE MultiWayIf       #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE ViewPatterns     #-}

module HSChain.P2P.PeerState.Handle.Lagging
  ( handler
  ) where

import Control.Monad
import Control.Monad.RWS.Strict
import Data.Foldable            (toList)
import System.Random            (randomRIO)

import Lens.Micro
import Lens.Micro.Mtl

import HSChain.Crypto
import HSChain.Internal.Types.Messages
import HSChain.P2P.Internal.Types
import HSChain.P2P.PeerState.Handle.Utils
import HSChain.P2P.PeerState.Monad
import HSChain.P2P.PeerState.Types
import HSChain.Store
import HSChain.Types.Blockchain
import HSChain.Types.Validators

import qualified Data.IntSet        as ISet
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict    as Map

handler :: (BlockData a, HandlerCtx a m) => HandlerDict LaggingState a m
handler = HandlerDict
  { handlerGossipMsg        = const handlerGossip
  , advanceOurHeight        = \_ -> return ()
  , handlerProposalTimeout  = \_ _ -> return []
  , handlerPrevoteTimeout   = \_ _ -> return []
  , handlerPrecommitTimeout = const handlerVotesTimeoutMsg
  , handlerBlocksTimeout    = const handlerBlocksTimeoutMsg
  }

handlerGossip
  :: (MonadIO m, MonadReadDB a m, BlockData a)
  => GossipMsg a -> TransitionT LaggingState a m ()
handlerGossip = \case
  GossipPreVote   _ ->
    return ()
  GossipPreCommit v@(signedValue -> Vote{..}) -> do
    addPrecommit voteHeight voteRound $ signedKeyInfo v
  GossipProposal (signedValue -> Proposal{..}) -> do
    addProposal propHeight propRound
  GossipBlock b -> addBlock b
  GossipAnn ann -> case ann of
    AnnStep step@(FullStep h _ _) -> do
      -- Don't go back.
      s@(FullStep h0 _ _) <- use lagPeerStep
      if | h    > h0 -> advancePeer step
         -- If update don't change height only advance step of peer
         | step > s  -> lagPeerStep .= step
         | otherwise -> return ()
    AnnLock{}             -> return ()
    AnnHasProposal  h r   -> addProposal h r
    AnnHasPreVote   {}    -> return ()
    AnnHasPreCommit h r i -> addPrecommit h r i
    AnnHasBlock     h r   -> do
      FullStep hP _ _ <- use lagPeerStep
      peerCommitRound <- use lagPeerCommitR
      when ( h == hP && r == peerCommitRound) $
        lagPeerHasBlock .= True
  GossipTx{}  -> return ()
  GossipPex{} -> return ()

addProposal :: MonadState (LaggingState a) m
            => Height -> Round -> m ()
addProposal h r = do
      FullStep peerHeihgt _ _ <- use lagPeerStep
      peerRound               <- use lagPeerCommitR
      when (h == peerHeihgt && r == peerRound) $
         lagPeerHasProposal .= True

addPrecommit :: MonadState (LaggingState a) m
             => Height -> Round -> ValidatorIdx alg -> m ()
addPrecommit h r idx = do
      FullStep hPeer _ _ <- use lagPeerStep
      peerRound           <- use lagPeerCommitR
      when (h == hPeer && r == peerRound) $
        lagPeerPrecommits %= insertValidatorIdx idx

addBlock :: (MonadState (LaggingState a) m, Crypto (Alg a))
         => Block a -> m ()
addBlock b = do
      peerBid <- use lagPeerBlockID
      when (blockHash b == peerBid) $ lagPeerHasBlock .= True

----------------------------------------------------------------

handlerVotesTimeoutMsg
  :: (MonadIO m, MonadReadDB a m)
  => LaggingState a -> m [GossipMsg a]
handlerVotesTimeoutMsg st = do
  queryRO (retrieveLocalCommit peerH) >>= \case
    Nothing  -> return []
    Just cmt -> do
      let cmtVotes  = Map.fromList [ (signedKeyInfo v, unverifySignature v)
                                   | v <- NE.toList (commitPrecommits cmt) ]
          peerVotes = Map.fromList
                    $ map (\x -> (ValidatorIdx x,()))
                    $ ISet.toList
                    $ getValidatorIntSet
                    $ st ^. lagPeerPrecommits
          unknown   = Map.difference cmtVotes peerVotes
          n         = Map.size unknown
      if | n > 0     -> do i <- liftIO $ randomRIO (0,n-1) -- TODO: move RndGen to Config or state
                           let vote@(signedValue -> Vote{..}) = unverifySignature $ toList unknown !! i
                           return [GossipPreCommit vote]
         | otherwise -> return []
  where
    FullStep peerH _ _ = st ^. lagPeerStep


----------------------------------------------------------------

handlerBlocksTimeoutMsg
  :: (MonadIO m, MonadReadDB a m, BlockData a)
  => LaggingState a -> m [GossipMsg a]
handlerBlocksTimeoutMsg st
  | st ^. lagPeerHasProposal
  , not $ st ^. lagPeerHasBlock
    = do b <- queryRO $ mustRetrieveBlock h
         return [GossipBlock b]
  | otherwise
    = return []
  where
    FullStep h _ _ = st ^. lagPeerStep
