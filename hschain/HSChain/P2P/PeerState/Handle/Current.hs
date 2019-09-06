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

import Control.Concurrent.STM   (atomically)
import Control.Monad
import Control.Monad.RWS.Strict
import Data.Foldable            (toList)
import Katip                    (showLS)
import System.Random            (randomRIO)

import Lens.Micro.Mtl

import HSChain.Blockchain.Internal.Types
import HSChain.Control                   (throwNothing)
import HSChain.Crypto
import HSChain.Crypto.Containers         (toPlainMap)
import HSChain.Debug.Trace
import HSChain.Exceptions
import HSChain.Logger
import HSChain.Store
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

handler :: Handler Current Event alg a m
handler =
  handlerGeneric
   handlerGossipMsg
   handlerVotesTimeout
   handlerMempoolTimeout
   handlerBlocksTimeout

issuedGossipHandler :: Handler Current GossipMsg alg a m
issuedGossipHandler =
  issuedGossipHandlerGeneric
    handlerGossipMsg
    advanceOurHeight

handlerGossipMsg :: MessageHandler CurrentState alg a m
handlerGossipMsg  gossipMsg = do
  case gossipMsg of
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
             else lift $ advancePeer step
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
        (FullStep hP _ _) <- use peerStep
        p <- view propStorage
        when ( h == hP ) $ do
          mbid <- lift $ retrievePropByR p h r -- TODO: IO here!
          forM_ mbid $ \(bid,_) ->
            peerBlocks %= Set.insert bid
        currentState


addProposal :: MonadState (CurrentState alg a) m
                   => Height -> Round -> m ()
addProposal h r = do
  (FullStep peerHeihgt _ _) <- use peerStep
  when (h == peerHeihgt) $
    peerProposals %= Set.insert r

addPrevote :: MonadState (CurrentState alg a) m
                   => Height -> Round -> ValidatorIdx alg -> m ()
addPrevote h r idx = do
      (FullStep hPeer _ _) <- use peerStep
      when (h == hPeer) $
        modify ( \ p ->
            p { _peerPrevotes = Map.alter
                         (\case
                             Nothing   -> Just
                                        $ insertValidatorIdx idx
                                        $ emptyValidatorISet
                                        $ validatorSetSize
                                        $ _peerValidators p
                             Just iset -> Just
                                        $ insertValidatorIdx idx iset
                         ) r (_peerPrevotes p)
               })

addPrecommit :: MonadState (CurrentState alg a) m
                   => Height -> Round -> ValidatorIdx alg -> m ()
addPrecommit h r idx = do
      (FullStep hPeer _ _) <- use peerStep
      when (h == hPeer) $
        modify $ \ p -> p { _peerPrecommits = Map.alter
                                   (\case
                                       Nothing   -> Just
                                                  $ insertValidatorIdx idx
                                                  $ emptyValidatorISet
                                                  $ validatorSetSize
                                                  $ _peerValidators p
                                       Just iset -> Just
                                                  $ insertValidatorIdx idx iset
                                   ) r (_peerPrecommits p)
                          }

addBlock :: (MonadState (CurrentState alg a) m, CryptoSign alg, CryptoHash alg)
         => Block alg a -> m ()
addBlock b = peerBlocks %= Set.insert (blockHash b)

----------------------------------------------------------------
  --
advanceOurHeight :: AdvanceOurHeight CurrentState alg a m
advanceOurHeight (FullStep ourH _ _) = do
      -- Current peer may become lagging if we increase our height
  (FullStep h _ _) <- use peerStep
  if h < ourH then
        do vals <- throwNothing (DBMissingValSet  h) <=< lift $ queryRO
                 $ retrieveValidatorSet h
           r    <- throwNothing (DBMissingRound   h) <=< lift $ queryRO
                 $ retrieveCommitRound  h
           bid  <- throwNothing (DBMissingBlockID h) <=< lift $ queryRO
                 $ retrieveBlockID      h
           p <- get
           return $ wrap $ LaggingState
             { _lagPeerStep        = _peerStep p
             , _lagPeerCommitR     = r
             , _lagPeerValidators  = vals
             , _lagPeerPrecommits  = emptyValidatorISet $ validatorSetSize vals
             , _lagPeerHasProposal = r   `Set.member` _peerProposals p
             , _lagPeerHasBlock    = bid `Set.member` _peerBlocks p
             , _lagPeerBlockID     = bid
             }
    else currentState

----------------------------------------------------------------

handlerVotesTimeout :: TimeoutHandler CurrentState alg a m
handlerVotesTimeout = do
  bchH      <- lift $ queryRO blockchainHeight
  trace (TePeerGossipVotes TepgvCurrent)
  st <- view consensusSt >>= lift . liftIO . atomically
  case st of
    Nothing                       -> return ()
    Just (h',_) | h' /= succ bchH -> return ()
    Just (_,tm)                   -> do
      (FullStep _ r _) <- use peerStep
      let doGosip            = push2Gossip
      let toSet = getValidatorIntSet
      noRoundInProposals <- Set.notMember r <$> use peerProposals

      -- Send proposals
      when noRoundInProposals $
        forM_ (r `Map.lookup` smProposals tm) $ \pr -> do
          let prop = signedValue pr
          addProposal (propHeight prop) (propRound prop)
          doGosip $ GossipProposal $ unverifySignature pr
          tickSend proposals
      -- Send prevotes
      peerPV <- maybe CIMap.empty (CIMap.fromSet (const ()) . toSet)
              . Map.lookup r
             <$> use peerPrevotes
      forM_ (Map.lookup r $ toPlainMap $ smPrevotesSet tm) $ \localPV -> do
        let unknown = CIMap.difference localPV peerPV
        unless (CIMap.null unknown) $ do
          let n = CIMap.size unknown
          i <- lift $ liftIO $ randomRIO (0,n-1)
          let vote@(signedValue -> Vote{..}) = unverifySignature $ toList unknown !! i
          addPrevote voteHeight voteRound $ signedKeyInfo vote
          doGosip $ GossipPreVote vote
          tickSend prevote
      -- Send precommits
      peerPC <- maybe CIMap.empty (CIMap.fromSet (const ()) . toSet)
              . Map.lookup r
            <$> use peerPrecommits
      case () of
        _| Just localPC <- Map.lookup r $ toPlainMap $ smPrecommitsSet tm
         , unknown      <- CIMap.difference localPC peerPC
         , not (CIMap.null unknown)
           -> do let n = CIMap.size unknown
                 i <- lift $ liftIO $ randomRIO (0,n-1)
                 let vote@(signedValue -> Vote{..}) = unverifySignature $ toList unknown !! i
                 addPrecommit voteHeight voteRound $ signedKeyInfo vote
                 doGosip $ GossipPreCommit vote
                 tickSend precommit
         | otherwise -> return ()
  currentState
----------------------------------------------------------------

handlerMempoolTimeout :: TimeoutHandler CurrentState alg a m
handlerMempoolTimeout = do
  advanceMempoolCursor
  currentState
----------------------------------------------------------------

handlerBlocksTimeout :: TimeoutHandler CurrentState alg a m
handlerBlocksTimeout = do
  (FullStep h r _) <- use peerStep
  roundInProposals <- Set.member r <$> use peerProposals
  p <- view propStorage
  mbid <- lift $ retrievePropByR p h r
  let mBlk = do (bid, bVal) <- mbid
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