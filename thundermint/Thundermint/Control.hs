{-# LANGUAGE ScopedTypeVariables #-}
-- |
module Thundermint.Control (
    MonadFork(..)
  , forkLinked
  , runConcurrently
    -- * Generalized MVar-code
  , withMVarM
  , modifyMVarM
  , modifyMVarM_
    -- * Mutex
  , Mutex
  , newMutex
  , withMutex
    -- * throwing on Maybe and Either
  , throwNothing
  , throwNothingM
  , throwLeft
  , throwLeftM
    -- * Generalized bracket
  , withMany
  ) where

import Control.Concurrent.MVar
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader

import Control.Concurrent  (ThreadId, killThread, myThreadId, throwTo)
import Control.Exception   (AsyncException, Exception(..), SomeException)
import Control.Monad.Catch (MonadMask, MonadThrow, bracket, mask, onException, throwM)

import qualified Control.Concurrent as Conc

----------------------------------------------------------------
--
----------------------------------------------------------------

-- | Type class for monads which could be forked
class MonadIO m => MonadFork m where
  fork :: m () -> m ThreadId
  forkFinally :: m a -> (Either SomeException a -> m ()) -> m ThreadId

instance MonadFork IO where
  fork        = Conc.forkIO
  forkFinally = Conc.forkFinally

instance MonadFork m => MonadFork (ReaderT r m) where
  fork (ReaderT action) = ReaderT $ fork . action
  forkFinally (ReaderT action) fini = ReaderT $ \r ->
    forkFinally (action r) (\e -> runReaderT (fini e) r)



-- | Fork thread. Any exception except `AsyncException` in forked
--   thread is forwarded to original thread. When parent thread dies
--   child thread is killed too
forkLinked :: (MonadIO m, MonadMask m, MonadFork m)
           => m a              -- ^ Action to execute in forked thread
           -> m b              -- ^ What to do while thread executes
           -> m b
forkLinked action io = do
  tid <- liftIO myThreadId
  let fini (Right _) = return ()
      fini (Left  e) = case fromException e of
        Just (_ :: AsyncException) -> return ()
        _                          -> liftIO $ throwTo tid e
  bracket (forkFinally action fini)
          (liftIO . killThread)
          (const io)


-- | Run computations concurrently. Any exception will propagate to
--   the top level and if any of them terminates normally function
--   will return and all other threads will be killed
runConcurrently
  :: (MonadIO m, MonadMask m, MonadFork m)
  => [m ()]              -- ^ Functions to run
  -> m ()
runConcurrently []      = return ()
runConcurrently actions = do
  lock <- liftIO Conc.newEmptyMVar
  foldr (\f -> forkLinked $ f >> void (liftIO (Conc.tryPutMVar lock ())))
        (liftIO $ Conc.takeMVar lock)
        actions


----------------------------------------------------------------
-- MVars
----------------------------------------------------------------

withMVarM :: (MonadMask m, MonadIO m) => MVar a -> (a -> m b) -> m b
withMVarM m action =
  mask $ \restore -> do
    a <- liftIO $ takeMVar m
    b <- restore (action a) `onException` liftIO (putMVar m a)
    liftIO $ putMVar m a
    return b

modifyMVarM_ :: (MonadMask m, MonadIO m) => MVar a -> (a -> m a) -> m ()
modifyMVarM_ m action =
  mask $ \restore -> do
    a  <- liftIO $ takeMVar m
    a' <- restore (action a) `onException` liftIO (putMVar m a)
    liftIO $ putMVar m a'

modifyMVarM :: (MonadMask m, MonadIO m) => MVar a -> (a -> m (a,b)) -> m b
modifyMVarM m action =
  mask $ \restore -> do
    a      <- liftIO $ takeMVar m
    (a',b) <- restore (action a) `onException` liftIO (putMVar m a)
    liftIO $ putMVar m a'
    return b


----------------------------------------------------------------
-- Mutex
----------------------------------------------------------------

newtype Mutex = Mutex (MVar ())

newMutex :: IO Mutex
newMutex = Mutex <$> newMVar ()

withMutex :: Mutex -> IO a -> IO a
withMutex (Mutex mvar) = withMVar mvar . const

----------------------------------------------------------------
--
----------------------------------------------------------------

-- | Convert some @withResource@ function to work with multiple
--   resources at once
withMany
  :: (r -> (h -> m a) -> m a)
  -> [r]
  -> ([h] -> m a)
  -> m a
withMany with rs0 action = go [] rs0
  where
    go hs []     = action (reverse hs)
    go hs (r:rs) = with r $ \h -> go (h:hs) rs


throwNothing :: (Exception e, MonadThrow m) => e -> Maybe a -> m a
throwNothing e = maybe (throwM e) pure

throwNothingM :: (Exception e, MonadThrow m) => e -> m (Maybe a) -> m a
throwNothingM e m = m >>= throwNothing e

throwLeft :: (Exception e, MonadThrow m) => Either e a -> m a
throwLeft = either throwM pure

throwLeftM :: (Exception e, MonadThrow m) => m (Either e a) -> m a
throwLeftM m = m >>= throwLeft
