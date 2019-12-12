{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE ViewPatterns      #-}

module HSChain.P2P.PeerState.Handle.Current
  ( handler
  , issuedGossipHandler
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


handler :: Handler CurrentState Event a m
handler =
  handlerGeneric
   handlerGossipMsg
   handlerVotesTimeout
   handlerMempoolTimeout
   handlerBlocksTimeout

issuedGossipHandler :: Handler CurrentState GossipMsg a m
issuedGossipHandler =
  issuedGossipHandlerGeneric
    handlerGossipMsg
    advanceOurHeight

handlerGossipMsg :: MessageHandler CurrentState a m
handlerGossipMsg = \case
    GossipPreVote v@(signedValue -> Vote{..}) -> do
      addPrevote voteHeight voteRound $ signedKeyInfo v
      currentState
    GossipPreCommit v@(signedValue -> Vote{..}) -> do
      addPrecommit voteHeight voteRound $ signedKeyInfo v
      currentState
    GossipProposal (signedValue -> Proposal{..}) -> do
      addProposal propHeight propRound
      currentState
    GossipBlock b -> do
      addBlock b
      currentState
    GossipTx {} -> currentState
    GossipPex {} -> currentState
    GossipAnn ann -> case ann of
      AnnStep step@(FullStep h _ _) -> do
        -- Don't go back.
        s@(FullStep h0 _ _) <- use peerStep
        if step > s
          -- If update don't change height only advance step of peer
          then if h0 == h
             then peerStep .= step >> currentState
             else advancePeer step
          else currentState
      AnnHasProposal  h r   -> do
        addProposal h r
        currentState
      AnnHasPreVote   h r i -> do
        addPrevote h r i
        currentState
      AnnHasPreCommit h r i -> do
        addPrecommit h r i
        currentState
      AnnHasBlock     h r   -> do
        FullStep hPeer _ _ <- use peerStep
        when ( h == hPeer ) $ do
          mstate <- atomicallyIO =<< view consensusSt
          forM_ mstate $ \(hSt, st) ->
            when (hPeer == hSt) $ do
              forM_ (proposalByR (smProposedBlocks st) r) $ \(bid,_) ->
                peerBlocks %= Set.insert bid
        currentState
      AnnLock mr -> do
        peerLock .= mr
        currentState

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
         => Block a -> m ()
addBlock b = peerBlocks %= Set.insert (blockHash b)

----------------------------------------------------------------
  --
advanceOurHeight :: AdvanceOurHeight CurrentState a m
advanceOurHeight (FullStep ourH _ _) = do
      -- Current peer may become lagging if we increase our height
  (FullStep h _ _) <- use peerStep
  if h < ourH then
        do vals <- queryRO $ mustRetrieveValidatorSet h
           r    <- queryRO $ mustRetrieveCommitRound  h
           bid  <- queryRO $ mustRetrieveBlockID      h
           p <- get
           return $ wrap $ LaggingState
             { _lagPeerStep        = _peerStep p
             , _lagPeerCommitR     = r
             , _lagPeerValidators  = vals
             , _lagPeerPrecommits  = emptyValidatorISet vals
             , _lagPeerHasProposal = r   `Set.member` _peerProposals p
             , _lagPeerHasBlock    = bid `Set.member` _peerBlocks p
             , _lagPeerBlockID     = bid
             }
    else currentState

----------------------------------------------------------------

handlerVotesTimeout :: TimeoutHandler CurrentState a m
handlerVotesTimeout = do
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
          tickSend proposals
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
                 tickSend precommit
         | otherwise -> return ()
  currentState

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
      tickSend prevote

----------------------------------------------------------------

handlerMempoolTimeout :: TimeoutHandler CurrentState a m
handlerMempoolTimeout = do
  advanceMempoolCursor
  currentState
----------------------------------------------------------------

handlerBlocksTimeout :: TimeoutHandler CurrentState a m
handlerBlocksTimeout = do
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
          addBlock b
          push2Gossip $ GossipBlock b
          tickSend blocks
  currentState
