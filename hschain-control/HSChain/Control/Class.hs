{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- |
module HSChain.Control.Class
  ( MonadFork(..)
  , forkFinally
  , forkLinked
  , forkLinkedIO
    -- * Helper
  , runConcurrently
  ) where

import Control.Monad
import Control.Monad.Catch (MonadThrow(..))
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



-- | Run computations concurrently. As soon as one thread finishes
--   execution normally or abnormally all other threads are killed.
--   Function blocks until all child threads finish execution. If
--   thread is killed by exceptions it's rethrown. Being killed by
--   'AsyncException' is considered normal termination.
runConcurrently
  :: (MonadIO m, MonadMask m, MonadFork m)
  => [m ()]              -- ^ Functions to run
  -> m ()
runConcurrently []      = return ()
runConcurrently [act]   = act
runConcurrently actions = do
  -- We communicate return status of thread via channel since we don't
  -- know a priory which will terminated first
  ch <- liftIO Conc.newChan
  -- Run child threads. We wait until one of threads terminate and
  -- then kill all others.
  (r, tids) <- bracket
    -- Acquisution is done under mask and fork doesn't block so we're
    -- couldn't be interrupted here
    (forM actions $ \f -> forkWithUnmask $ \restore -> do
        try (restore f) >>= liftIO . \case
          Right _ -> Conc.writeChan ch (Right ())
          Left  e -> case fromException e of
            Just (_ :: AsyncException) -> Conc.writeChan ch (Right ())
            _                          -> Conc.writeChan ch (Left  e)
    )
    -- throwTo and therefore killThread block and as such could be
    -- interrupted and leave threads behind. In order to avoid this
    -- and to parallelise killing we create threads per killThread
    (liftIO . mapM (Conc.forkIO . killThread))
    -- Wait until one of threads completes execution.
    (\tids -> do r <- liftIO $ Conc.readChan ch
                 return (r,tids)
    )
  -- Wait until all other threads terminate. We can be killed by async
  -- exceptions here. But it doesn't matter at this point since we
  -- sent black mark to every child thread
  case tids of
    []   -> return ()
    _:ts -> liftIO $ forM_ ts $ const $ Conc.readChan ch
  case r of
    Right () -> return ()
    Left  e  -> throwM e
