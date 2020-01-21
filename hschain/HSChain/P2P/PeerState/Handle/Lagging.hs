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

handler :: (CryptoHashable a, HandlerCtx a m) => HandlerDict LaggingState a m
handler = HandlerDict
  { handlerGossipMsg      = handlerGossip
  , advanceOurHeight      = \_ -> return ()
  , handlerVotesTimeout   = handlerVotesTimeoutMsg
  , handlerMempoolTimeout = handlerMempoolTimeoutMsg
  , handlerBlocksTimeout  = handlerBlocksTimeoutMsg
  }

handlerGossip :: MessageHandler LaggingState a m
handlerGossip = \case
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
    _ -> return ()

addProposal :: MonadState (LaggingState a) m
                   => Height -> Round -> m ()
addProposal h r = do
      (FullStep peerHeihgt _ _) <- use lagPeerStep
      peerRound                 <- use lagPeerCommitR
      when (h == peerHeihgt && r == peerRound) $
         lagPeerHasProposal .= True

addPrecommit :: MonadState (LaggingState a) m
                   => Height -> Round -> ValidatorIdx alg -> m ()
addPrecommit h r idx = do
      (FullStep hPeer _ _) <- use lagPeerStep
      peerRound            <- use lagPeerCommitR
      when (h == hPeer && r == peerRound) $
        lagPeerPrecommits %= insertValidatorIdx idx

addBlock :: (MonadState (LaggingState a) m, Crypto (Alg a))
         => Block a -> m ()
addBlock b = do
      peerBid <- use lagPeerBlockID
      when (blockHash b == peerBid) $ lagPeerHasBlock .= True

----------------------------------------------------------------

handlerVotesTimeoutMsg :: CryptoHashable a => TimeoutHandler LaggingState a m
handlerVotesTimeoutMsg = do
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

----------------------------------------------------------------

handlerMempoolTimeoutMsg :: TimeoutHandler LaggingState a m
handlerMempoolTimeoutMsg = return ()
----------------------------------------------------------------

handlerBlocksTimeoutMsg :: CryptoHashable a => TimeoutHandler LaggingState a m
handlerBlocksTimeoutMsg = do
  hasProp    <- use lagPeerHasProposal
  hasNoBlock <- not <$> use lagPeerHasBlock
  when (hasProp && hasNoBlock) $ do
    FullStep h _ _ <- use lagPeerStep
    b <- queryRO $ mustRetrieveBlock h
    addBlock b
    push2Gossip $ GossipBlock b
