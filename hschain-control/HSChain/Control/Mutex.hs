-- |
module HSChain.Control.Mutex
  ( Mutex
  , newMutex
  , withMutex
  ) where

import Control.Concurrent.MVar
import Control.Monad.Catch
import Control.Monad.IO.Class

-- | Simple 'MVar' based mutex
newtype Mutex = Mutex (MVar ())

newMutex :: MonadIO m => m Mutex
newMutex = Mutex <$> liftIO (newMVar ())

withMutex :: (MonadMask m, MonadIO m) => Mutex -> m a -> m a
withMutex (Mutex m) action = do
  mask $ \restore -> do
    () <- liftIO (takeMVar m)
    a  <- restore action `onException` liftIO (putMVar m ())
    liftIO (putMVar m ())
    return a
