{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs            #-}
{-# LANGUAGE MultiWayIf       #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE ViewPatterns     #-}

module HSChain.P2P.PeerState.Handle.Lagging
  ( handler
  , issuedGossipHandler
  ) where

import Control.Monad
import Control.Monad.RWS.Strict
import Data.Foldable            (toList)
import System.Random            (randomRIO)

import Lens.Micro.Mtl

import HSChain.Blockchain.Internal.Types
import HSChain.Crypto
import HSChain.Store
import HSChain.Types.Blockchain
import HSChain.Types.Validators

import HSChain.P2P.Internal.Logging       (GossipCounters(..))
import HSChain.P2P.Internal.Types
import HSChain.P2P.PeerState.Handle.Utils
import HSChain.P2P.PeerState.Monad
import HSChain.P2P.PeerState.Types

import qualified Data.IntSet        as ISet
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict    as Map

handler :: Handler LaggingState Event alg a m
handler =
  handlerGeneric
   handlerGossipMsg
   handlerVotesTimeout
   handlerMempoolTimeout
   handlerBlocksTimeout

issuedGossipHandler :: Handler LaggingState GossipMsg alg a m
issuedGossipHandler =
  issuedGossipHandlerGeneric
    handlerGossipMsg
    advanceOurHeight

handlerGossipMsg :: MessageHandler LaggingState alg a m
handlerGossipMsg = \case
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
             else advancePeer step -- TODO: IO read from DB
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
      AnnLock _ -> currentState
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

advanceOurHeight :: AdvanceOurHeight LaggingState alg a m
advanceOurHeight = const currentState
----------------------------------------------------------------

handlerVotesTimeout :: TimeoutHandler LaggingState alg a m
handlerVotesTimeout = do
  bchH      <- queryRO blockchainHeight
  (FullStep peerH _ _) <- use lagPeerStep
  mcmt <- queryRO $
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
       do i <- liftIO $ randomRIO (0,n-1) -- TODO: move RndGen to Config or state
          let vote@(signedValue -> Vote{..}) = unverifySignature $ toList unknown !! i
          addPrecommit voteHeight voteRound $ signedKeyInfo vote
          push2Gossip $ GossipPreCommit vote
          tickSend precommit
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
    FullStep h _ _ <- use lagPeerStep
    b <- queryRO $ mustRetrieveBlock h
    addBlock b
    push2Gossip $ GossipBlock b
    tickSend blocks
  currentState

