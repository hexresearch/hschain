-- | Monad MonadTrace for tracing execution
--
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables        #-}
module Thundermint.Debug.Trace where


import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Data.Set (Set)

import Thundermint.Control

-- TODO : Нужно обобщить MonadTrace; сейчас `trace` принимает
--        жёстко закодированный TraceEvents; надо сделать,
--        чтобы можно было передавать объект класса IsTraceEvents,
--        которые могут быть любыми. Посмотреть в сторону
--        Data types a la carte

data TraceEvents
    =  TeNodeStarted
    -- ^ Node is started
    | TeNodeConnectingTo String
    -- ^ Node try to connect to other address
    | TeNodeOtherTryConnect String
    -- ^ Other node try to connect; if it does not
    --   connected previously then `TeNodeConnected`
    --   event fired
    | TeNodeOtherConnected String
    -- ^ Other node connected successfully
    | TePeerRegistryChanged (Set String)
    deriving (Show, Ord, Eq)

class Monad m => MonadTrace m where
    trace :: TraceEvents -> m ()

instance MonadTrace IO where
    trace _ = return ()

instance (MonadTrace m) => MonadTrace (ReaderT r m) where
    trace = lift . trace

type Callback m = TraceEvents -> m ()

newtype TracerT m a = TracerT ( ReaderT (Callback m) m a )
  deriving ( Functor, Applicative, Monad
           , MonadIO, MonadThrow, MonadCatch, MonadMask, MonadFork )

instance MonadTrans TracerT where
  lift m = TracerT (ReaderT (const m))

runTracerT :: Callback m -> TracerT m a -> m a
runTracerT callback (TracerT m) = runReaderT m callback

instance MonadIO m => MonadTrace (TracerT m) where
  trace event = TracerT ask >>= \callback -> lift $ callback event

