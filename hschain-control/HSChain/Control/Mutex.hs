-- |
module HSChain.Control.Mutex
  ( Mutex
  , newMutex
  , withMutex
    -- * MVars
  , withMVarM
  , modifyMVarM
  , modifyMVarM_
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
