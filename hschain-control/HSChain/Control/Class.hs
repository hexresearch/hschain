{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- |
module HSChain.Control.Class
  ( MonadFork(..)
  , forkFinally
  , forkLinked
  , forkLinkedIO
  ) where

import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
import qualified Control.Monad.Trans.State.Strict as SS
import qualified Control.Monad.Trans.State.Lazy   as SL
import Control.Concurrent  (ThreadId, killThread, myThreadId, throwTo)
import Control.Exception   (AsyncException, Exception(..), SomeException(..))
import Control.Monad.Catch (MonadMask, bracket, mask, try)
import qualified Control.Concurrent as Conc



-- | Type class for monads which could be forked
class MonadIO m => MonadFork m where
  fork           :: m () -> m ThreadId
  forkWithUnmask :: ((forall a. m a -> m a) -> m ()) -> m ThreadId

forkFinally
  :: (MonadFork m, MonadMask m)
  => m a -> (Either SomeException a -> m ()) -> m ThreadId
forkFinally action fini =
  mask $ \restore ->
    fork $ try (restore action) >>= fini

instance MonadFork IO where
  fork           = Conc.forkIO
  forkWithUnmask = Conc.forkIOWithUnmask

instance MonadFork m => MonadFork (ReaderT r m) where
  fork (ReaderT action) = ReaderT $ fork . action
  forkWithUnmask cont = ReaderT $ \r -> forkWithUnmask $ \restore ->
    runReaderT (cont (liftF restore)) r
    where
      liftF f m = ReaderT $ f . runReaderT m

instance MonadFork m => MonadFork (SS.StateT s m) where
  fork action = SS.StateT $ \s -> do
    tid <- fork $ SS.evalStateT action s
    return (tid,s)
  forkWithUnmask cont = SS.StateT $ \s -> do
    tid <- forkWithUnmask $ \restore ->
      SS.evalStateT (cont (liftF restore)) s
    return (tid,s)
    where
      liftF f m = SS.StateT $ f . SS.runStateT m

instance MonadFork m => MonadFork (SL.StateT s m) where
  fork action = SL.StateT $ \s -> do
    tid <- fork $ SL.evalStateT action s
    return (tid,s)
  forkWithUnmask cont = SL.StateT $ \s -> do
    tid <- forkWithUnmask $ \restore ->
      SL.evalStateT (cont (liftF restore)) s
    return (tid,s)
    where
      liftF f m = SL.StateT $ f . SL.runStateT m


-- | Fork thread. Any exception except `AsyncException` in forked
--   thread is forwarded to original thread. When parent thread dies
--   child thread is killed too
forkLinked :: (MonadIO m, MonadMask m, MonadFork m)
           => m a              -- ^ Action to execute in forked thread
           -> m b              -- ^ What to do while thread executes
           -> m b
forkLinked action io = do
  tid <- liftIO myThreadId
  -- NOTE: Resource in bracket is acquired with asynchronous
  --       exceptions masked so child thread inherits masked state and
  --       needs to be explicitly unmasked
  bracket
    (forkWithUnmask $ \restore ->
        try (restore action) >>= \case
          Right _ -> return ()
          Left  e -> case fromException e of
            Just (_ :: AsyncException) -> return ()
            _                          -> liftIO $ throwTo tid e
    )
    (liftIO . killThread)
    (const io)

-- | Fork thread. Any exception except `AsyncException` in forked
--   thread is forwarded to original thread. When parent thread dies
--   child thread is killed too
forkLinkedIO :: (MonadIO m, MonadMask m)
             => IO a             -- ^ Action to execute in forked thread
             -> m b              -- ^ What to do while thread executes
             -> m b
forkLinkedIO action io = do
  tid <- liftIO myThreadId
  -- NOTE: Resource in bracket is acquired with asynchronous
  --       exceptions masked so child thread inherits masked state and
  --       needs to be explicitly unmasked
  bracket
    (liftIO $ forkWithUnmask $ \restore ->
        try (restore action) >>= \case
          Right _ -> return ()
          Left  e -> case fromException e of
            Just (_ :: AsyncException) -> return ()
            _                          -> liftIO $ throwTo tid e
    )
    (liftIO . killThread)
    (const io)
