{-# LANGUAGE ScopedTypeVariables #-}
-- |
module Thundermint.Control (
    MonadFork(..)
  , forkLinked
  , runConcurrently
  ) where

import Control.Monad
import Control.Monad.Trans.Reader
import Control.Monad.Catch            (bracket,MonadMask)
import Control.Monad.IO.Class
import           Control.Exception    (Exception(..), SomeException, AsyncException)
import           Control.Concurrent   (ThreadId, killThread, throwTo, myThreadId)
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
