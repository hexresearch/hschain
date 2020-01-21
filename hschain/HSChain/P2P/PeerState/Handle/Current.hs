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
import Katip                    (showLS)
import System.Random            (randomRIO)

import Lens.Micro.Mtl

import HSChain.Blockchain.Internal.Types
import HSChain.Control                   (atomicallyIO)
import HSChain.Crypto
import HSChain.Crypto.Containers         (toPlainMap)
import HSChain.Logger
import HSChain.Store
import HSChain.Store.Internal.Proposals
import HSChain.Store.Internal.BlockDB
import HSChain.Types.Blockchain
import HSChain.Types.Validators

import HSChain.P2P.Internal.Logging (GossipCounters(..))
import HSChain.P2P.Internal.Types
import HSChain.P2P.PeerState.Monad
import HSChain.P2P.PeerState.Types

import HSChain.P2P.PeerState.Handle.Utils

import qualified HSChain.Data.CIntMap as CIMap
import qualified Data.Map.Strict    as Map
import qualified Data.Set           as Set


handler :: HandlerCtx a m => HandlerDict CurrentState a m
handler = HandlerDict
  { handlerGossipMsg      = handlerGossip
  , advanceOurHeight      = advanceOurHeightWrk
  , handlerVotesTimeout   = handlerVotesTimeoutMsg
  , handlerMempoolTimeout = handlerMempoolTimeoutMsg
  , handlerBlocksTimeout  = handlerBlocksTimeoutMsg
  }

handlerGossip :: MessageHandler CurrentState a m
handlerGossip = \case
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
          mstate <- atomicallyIO =<< view consensusSt
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

addBlock :: (MonadState (CurrentState a) m, Crypto (Alg a))
         => BlockID a -> m ()
addBlock bid = peerBlocks %= Set.insert bid

----------------------------------------------------------------

advanceOurHeightWrk :: AdvanceOurHeight CurrentState a m
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

handlerVotesTimeoutMsg :: TimeoutHandler CurrentState a m
handlerVotesTimeoutMsg = do
  bchH <- queryRO blockchainHeight
  st <- atomicallyIO =<< view consensusSt
  case st of
    Nothing                       -> return ()
    Just (h',_) | h' /= succ bchH -> return ()
    Just (_,tm)                   -> do
      FullStep _ r _ <- use peerStep
      noRoundInProposals <- Set.notMember r <$> use peerProposals
      -- Send proposals
      when noRoundInProposals $
        forM_ (r `Map.lookup` smProposals tm) $ \pr -> do
          let prop = signedValue pr
          addProposal (propHeight prop) (propRound prop)
          push2Gossip $ GossipProposal $ unverifySignature pr
      -- Send prevotes for lock round
      use peerLock >>= mapM_ (gossipPrevotes tm)
      -- Send prevotes
      gossipPrevotes tm r
      -- Send precommits
      peerPC <- maybe CIMap.empty (CIMap.fromSet (const ()) . getValidatorIntSet)
              . Map.lookup r
            <$> use peerPrecommits
      case () of
        _| Just localPC <- Map.lookup r $ toPlainMap $ smPrecommitsSet tm
         , unknown      <- CIMap.difference localPC peerPC
         , not (CIMap.null unknown)
           -> do let n = CIMap.size unknown
                 i <- liftIO $ randomRIO (0,n-1)
                 let vote@(signedValue -> Vote{..}) = unverifySignature $ toList unknown !! i
                 addPrecommit voteHeight voteRound $ signedKeyInfo vote
                 push2Gossip $ GossipPreCommit vote
         | otherwise -> return ()

gossipPrevotes
  :: ( MonadState  (CurrentState a) m
     , MonadWriter [Command a] m
     , MonadReader (Config n a) m
     , MonadIO m
     )
  => TMState a -> Round -> m ()
gossipPrevotes tm r = do
  peerPV <- maybe CIMap.empty (CIMap.fromSet (const ()) . getValidatorIntSet)
          . Map.lookup r
         <$> use peerPrevotes
  forM_ (Map.lookup r $ toPlainMap $ smPrevotesSet tm) $ \localPV -> do
    let unknown = CIMap.difference localPV peerPV
    unless (CIMap.null unknown) $ do
      let n = CIMap.size unknown
      i <- liftIO $ randomRIO (0,n-1)
      let vote@(signedValue -> Vote{..}) = unverifySignature $ toList unknown !! i
      addPrevote voteHeight voteRound $ signedKeyInfo vote
      push2Gossip $ GossipPreVote vote

----------------------------------------------------------------

handlerMempoolTimeoutMsg :: TimeoutHandler CurrentState a m
handlerMempoolTimeoutMsg = advanceMempoolCursor

----------------------------------------------------------------

handlerBlocksTimeoutMsg :: TimeoutHandler CurrentState a m
handlerBlocksTimeoutMsg = do
  mstate <- atomicallyIO =<< view consensusSt
  forM_ mstate $ \(hSt,st) -> do
    FullStep h r _ <- use peerStep
    when (h == hSt) $ do
      roundInProposals <- Set.member r <$> use peerProposals
      let mBlk = do (bid, bVal) <- proposalByR (smProposedBlocks st) r
                    blk         <- blockFromBlockValidation bVal
                    return (bid,blk)
      forM_ mBlk $ \(bid,b) -> do
        -- Peer has proposal but not block
        noBlockInState <- Set.notMember bid <$> use peerBlocks
        when (roundInProposals && noBlockInState) $ do
          lift $ logger DebugS ("Gossip: " <> showLS bid) ()
          addBlock bid
          push2Gossip $ GossipBlock b
