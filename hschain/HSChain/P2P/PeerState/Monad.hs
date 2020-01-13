{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE TypeFamilies               #-}

module HSChain.P2P.PeerState.Monad where

import Codec.Serialise          (Serialise)
import Control.Monad.Catch      (MonadThrow)
import Control.Monad.RWS.Strict
import Lens.Micro.Mtl

import HSChain.Blockchain.Internal.Types
import HSChain.Crypto
import HSChain.Types
import HSChain.Logger
import HSChain.Store.Internal.Query      (MonadReadDB(..))

import HSChain.P2P.Internal.Types
import HSChain.P2P.Internal.Logging hiding (tx, tickRecv)
import HSChain.P2P.PeerState.Types

import qualified HSChain.P2P.Internal.Logging as Logging


-- | Underlying monad for transitions of state for gossip
newtype TransitionT s a m r = TransitionT
  { unTransition :: RWST (Config m a) [Command a] (s a) m r }
  deriving ( Functor
           , Applicative
           , Monad
           , MonadIO
           , MonadReader (Config m a)
           , MonadWriter [Command a]
           , MonadState (s a)
           , MonadThrow
           )
instance MonadReadDB m a => MonadReadDB (TransitionT s a m) a where
  askConnectionRO = TransitionT $ lift askConnectionRO

instance MonadTrans (TransitionT s a) where
  lift = TransitionT . lift

-- | Runs `TransitionT'.
runTransitionT
  :: Monad m
  => TransitionT s a m (State a)
  -> Config m a
  -> s a
  -> m (State a, [Command a])
runTransitionT action cfg st = do
  (r,_,acc) <- runRWST (unTransition action) cfg st
  return (r,acc)

type HandlerCtx a m = ( Serialise a
                      , Crypto (Alg a)
                      , MonadIO m
                      , MonadReadDB m a
                      , MonadLogger m
                      )

-- | Handler of events.
type Handler s t a m = HandlerCtx a m
                    => t a -- ^ `Event' to handle
                    -> TransitionT s a m (State a) -- ^ new `TransitionT'

-- | Obtain current state wrapped as 'State'
currentState :: (MonadState (s a) m, Wrapable s) => m (State a)
currentState = wrap <$> get

resendGossip :: ( MonadReader (Config n a) m
                , MonadWriter [Command a]  m
                , MonadIO m
                )
             => GossipMsg a -> m ()
resendGossip (GossipPreVote v  ) = tell [SendRX $ RxPreVote v] >> tickRecv prevote
resendGossip (GossipPreCommit v) = tell [SendRX $ RxPreCommit v] >> tickRecv precommit
resendGossip (GossipProposal  p) = tell [SendRX $ RxProposal p] >> tickRecv proposals
resendGossip (GossipBlock     b) = tell [SendRX $ RxBlock b] >> tickRecv blocks
resendGossip (GossipTx tx      ) = tell [Push2Mempool tx] >> tickRecv Logging.tx
resendGossip (GossipPex pexmsg ) = tell [SendPEX pexmsg] >> tickRecv pex
resendGossip _                   = return ()


-- | Increment receive counter
tickRecv :: (MonadReader (Config n a) m, MonadIO m)
         => (GossipCounters -> Counter) -> m ()
tickRecv counter =
  Logging.tickRecv . counter =<< view gossipCounters

-- | Increment send counter
tickSend :: (MonadReader (Config n a) m, MonadIO m)
         => (GossipCounters -> Counter) -> m ()
tickSend counter =
  Logging.tickSend . counter =<< view gossipCounters

push2Gossip :: MonadWriter [Command a] m
            => GossipMsg a -> m ()
push2Gossip msg = tell [Push2Gossip msg]
