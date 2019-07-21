{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE TypeFamilies               #-}

module Thundermint.P2P.PeerState.Monad where

import Codec.Serialise          (Serialise)
import Control.Monad.Catch      (MonadThrow)
import Control.Monad.RWS.Strict

import Thundermint.Blockchain.Internal.Types
import Thundermint.Crypto
import Thundermint.Logger
import Thundermint.Debug.Trace
import Thundermint.Store.Internal.Query      (MonadReadDB)

import Thundermint.P2P.Internal.Types
import Thundermint.P2P.PeerState.Types

newtype TransitionT s alg a m r = TransitionT { unTransition :: RWST (Config m alg a) [Command alg a] (s alg a) m r }
  deriving ( Functor
           , Applicative
           , Monad
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

instance MonadTrace m => MonadTrace (TransitionT s alg a m) where
    trace = lift . trace

-- | Runs `TransitionT'.
runTransitionT :: TransitionT s alg a m (SomeState alg a) -> Config m alg a -> s alg a -> m (SomeState alg a, s alg a, [Command alg a])
runTransitionT = runRWST . unTransition

type HandlerCtx alg a m = (Serialise a, CryptoHash alg, CryptoSign alg, Monad m, MonadIO m, MonadReadDB m alg a, MonadThrow m, MonadLogger m, MonadTrace m)

-- | Handler of events.
type Handler s alg a m =  HandlerCtx alg a m
                       => Event alg a -- ^ `Event' to handle
                       -> TransitionT (InternalState s) alg a m (SomeState alg a) -- ^ new `TransitionT'

currentState :: (Functor m, Monad m, Wrapable t) => TransitionT t alg a m (SomeState alg a)
currentState = wrap <$> get

resendGossip :: MonadWriter [Command alg a] m => GossipMsg alg a -> m ()
resendGossip (GossipPreVote v  ) = tell [SendRX $ RxPreVote v]
resendGossip (GossipPreCommit v) = tell [SendRX $ RxPreCommit v]
resendGossip (GossipProposal  p) = tell [SendRX $ RxProposal p]
resendGossip (GossipBlock     b) = tell [SendRX $ RxBlock b]
resendGossip (GossipTx tx      ) = tell [Push2Mempool tx]
resendGossip (GossipPex pexmsg ) = tell [SendPEX pexmsg]
resendGossip _                   = return ()

push2Gossip :: MonadWriter [Command alg a] m
            => GossipMsg alg a -> m ()
push2Gossip msg = tell [Push2Gossip msg]
