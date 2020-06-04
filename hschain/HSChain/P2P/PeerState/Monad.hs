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

import HSChain.Crypto
import HSChain.Types
import HSChain.Logger
import HSChain.Store.Internal.Query      (MonadReadDB(..))

import HSChain.P2P.PeerState.Types



-- | Transitions of peer's state during the gossip. It's state monad
--   for lagging\/current\/ahesd state together with final
--   callback which describes how to convert that state to
--   'State'. It's required since we may change type of state in
--   reaction to gossip.
newtype TransitionT s a m r = TransitionT
  { unTransition :: StateT (s a, s a -> m (State a)) m r }
  deriving ( Functor
           , Applicative
           , Monad
           , MonadIO
           , MonadThrow
           )
instance MonadReadDB a m => MonadReadDB a (TransitionT s a m) where
  askConnectionRO = TransitionT $ lift askConnectionRO

instance Monad m => MonadState (s a) (TransitionT s a m) where
  state f = TransitionT $ state $ \(s,fini) -> (,fini) <$> f s

instance MonadTrans (TransitionT s a) where
  lift = TransitionT . lift

-- | Set finaliser callback.
setFinalState :: Monad m => (s a -> m (State a)) -> TransitionT s a m ()
setFinalState st = TransitionT $
  modify' $ \(s,_) -> (s, st)


-- | Runs 'TransitionT'.
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
                      , MonadReadDB a m
                      , MonadLogger m
                      )
