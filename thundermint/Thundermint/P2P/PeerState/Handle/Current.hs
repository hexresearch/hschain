{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs            #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE ViewPatterns     #-}

module Thundermint.P2P.PeerState.Handle.Current ( handler ) where

import qualified Data.Map as Map
import qualified Data.Set as Set

import Control.Monad
import Control.Monad.RWS.Strict

import Lens.Micro.Mtl

import Thundermint.Blockchain.Internal.Types
import Thundermint.Crypto
import Thundermint.Store
import Thundermint.Types.Blockchain
import Thundermint.Types.Validators

import Thundermint.P2P.Internal.Types
import Thundermint.P2P.PeerState.Monad
import Thundermint.P2P.PeerState.Types

import Thundermint.P2P.PeerState.Handle.Utils

handler :: Handler Current alg a m
handler (EGossip gossipMsg) = do
  resendGossip gossipMsg
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
      peerBlocks %= Set.insert (blockHash b)
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
        (Config p) <- ask
        when ( h == hP ) $ do
          res <- lift $ retrievePropByR p h r -- TODO: IO here!
          forM_ (blockFromBlockValidation res) $ \(bid,_) ->
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
