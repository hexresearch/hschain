{-# LANGUAGE ScopedTypeVariables #-}
-- |
module Thundermint.Control (
    MonadFork(..)
  , forkLinked
  ) where

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
class MonadFork m where
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
           => IO a              -- ^ Action to execute in forked thread
           -> m b              -- ^ What to do while thread executes
           -> m b
forkLinked action io = do
  tid <- liftIO myThreadId
  let fini (Right _) = return ()
      fini (Left  e) = case fromException e of
        Just (_ :: AsyncException) -> return ()
        _                          -> throwTo tid e
  bracket (liftIO $ forkFinally action fini)
          (liftIO . killThread)
          (const io)
