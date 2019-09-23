{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE TypeFamilies               #-}

module HSChain.P2P.PeerState.Monad where

import Codec.Serialise          (Serialise)
import Control.Monad.Catch      (MonadThrow)
import Control.Monad.RWS.Strict
import Lens.Micro.Mtl

import HSChain.Blockchain.Internal.Types
import HSChain.Crypto
import HSChain.Logger
import HSChain.Store.Internal.Query      (MonadReadDB)

import HSChain.P2P.Internal.Types
import HSChain.P2P.Internal.Logging hiding (tx, tickRecv)
import HSChain.P2P.PeerState.Types

import qualified HSChain.P2P.Internal.Logging as Logging

newtype TransitionT s alg a m r = TransitionT
  { unTransition :: RWST (Config m alg a) [Command alg a] (s alg a) m r }
  deriving ( Functor
           , Applicative
           , Monad
           , MonadIO
           , MonadReader (Config m alg a)
           , MonadWriter [Command alg a]
           , MonadState (s alg a)
           , MonadRWS (Config m alg a) [Command alg a] (s alg a)
           , MonadThrow
           )
instance MonadTrans (TransitionT s alg a) where
  lift = TransitionT . lift

--instance MonadLogger m => MonadLogger (TransitionT s alg a m) where
--  logger s l a = lift $ logger s l a
--  localNamespace f (TransitionT action) = TransitionT $ localNamespace f $ lift $ action

-- | Runs `TransitionT'.
runTransitionT
  :: Monad m
  => TransitionT s alg a m (State alg a)
  -> Config m alg a
  -> s alg a
  -> m (State alg a, [Command alg a])
runTransitionT action cfg st = do
  (r,_,acc) <- runRWST (unTransition action) cfg st
  return (r,acc)

type HandlerCtx alg a m = ( Serialise a
                          , Crypto alg
                          , MonadIO m
                          , MonadReadDB m alg a
                          , MonadLogger m
                          )

-- | Handler of events.
type Handler s t alg a m =  HandlerCtx alg a m
                         => t alg a -- ^ `Event' to handle
                         -> TransitionT s alg a m (State alg a) -- ^ new `TransitionT'

currentState :: (Monad m, Wrapable t) => TransitionT t alg a m (State alg a)
currentState = wrap <$> get

resendGossip :: ( MonadReader (Config n alg a) m
                , MonadWriter [Command alg a]  m
                , MonadIO m
                )
             => GossipMsg alg a -> m ()
resendGossip (GossipPreVote v  ) = tell [SendRX $ RxPreVote v] >> tickRecv prevote
resendGossip (GossipPreCommit v) = tell [SendRX $ RxPreCommit v] >> tickRecv precommit
resendGossip (GossipProposal  p) = tell [SendRX $ RxProposal p] >> tickRecv proposals
resendGossip (GossipBlock     b) = tell [SendRX $ RxBlock b] >> tickRecv blocks
resendGossip (GossipTx tx      ) = tell [Push2Mempool tx] >> tickRecv Logging.tx
resendGossip (GossipPex pexmsg ) = tell [SendPEX pexmsg] >> tickRecv pex
resendGossip _                   = return ()

tickRecv :: (MonadReader (Config n alg a) m, MonadIO m)
         => (GossipCounters -> Counter) -> m ()
tickRecv counter =
  Logging.tickRecv . counter =<< view gossipCounters

tickSend :: (MonadReader (Config n alg a) m, MonadIO m)
         => (GossipCounters -> Counter) -> m ()
tickSend counter =
  Logging.tickSend . counter =<< view gossipCounters

push2Gossip :: MonadWriter [Command alg a] m
            => GossipMsg alg a -> m ()
push2Gossip msg = tell [Push2Gossip msg]
