{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeFamilies               #-}

module HSChain.P2P.PeerState.Monad where

import Codec.Serialise            (Serialise)
import Control.Monad.Catch        (MonadThrow)
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.State.Strict (StateT(..),execStateT,MonadState(..),modify')

import HSChain.Blockchain.Internal.Types
import HSChain.Crypto
import HSChain.Types
import HSChain.Logger
import HSChain.Store.Internal.Query      (MonadReadDB(..))

import HSChain.P2P.Internal.Types
import HSChain.P2P.PeerState.Types



-- | Underlying monad for transitions of state for gossip
newtype TransitionT s a m r = TransitionT
  { unTransition :: StateT (s a, s a -> m (State a)) m r }
  deriving ( Functor
           , Applicative
           , Monad
           , MonadIO
           , MonadThrow
           )
instance MonadReadDB m a => MonadReadDB (TransitionT s a m) a where
  askConnectionRO = TransitionT $ lift askConnectionRO

instance Monad m => MonadState (s a) (TransitionT s a m) where
  state f = TransitionT $ state $ \(s,fini) -> (,fini) <$> f s

instance MonadTrans (TransitionT s a) where
  lift = TransitionT . lift

setFinalState :: Monad m => (s a -> m (State a)) -> TransitionT s a m ()
setFinalState st = TransitionT $
  modify' $ \(s,_) -> (s, st)


-- | Runs `TransitionT'.
runTransitionT
  :: (Wrapable s, Monad m)
  => TransitionT s a m ()
  -> s a
  -> m (State a)
runTransitionT action st = do
  (s,fini) <- execStateT (unTransition action) (st, return . wrap)
  fini s



type HandlerCtx a m = ( Serialise a
                      , Crypto (Alg a)
                      , MonadIO m
                      , MonadReadDB m a
                      , MonadLogger m
                      )

-- | Handler of events.
type Handler s t a m = HandlerCtx a m
                    => t a -- ^ `Event' to handle
                    -> TransitionT s a m () -- ^ new `TransitionT'
