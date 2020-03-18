-- |
module HSChain.Control.Shepherd
  ( Shepherd
  , withShepherd
  , newSheep
  , newSheepFinally
  ) where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad
import Control.Monad.Catch
import Control.Monad.IO.Class
import qualified Data.Map.Strict as Map

import HSChain.Control.Class


-- | Object which shepherds herd of threads, keeps track of them, and
--   most importantly ensures that they are properly terminated when
--   @Shepherd@ goes out of scope (see 'withShepherd' for details).
newtype Shepherd = Shepherd (TVar (Map.Map ThreadId (MVar ())))


-- | Create new shepherd. All threads that are created using
--   'newSheep' or 'newSheepFinally' will be terminated after this
--   function returns. This function only ensures that no thread will
--   leak. It performs no action if thread terminates normally or
--   abnormally during function execution.
withShepherd :: (MonadMask m, MonadIO m) => (Shepherd -> m a) -> m a
withShepherd
  = bracket (liftIO $ Shepherd <$> newTVarIO Map.empty)
            (liftIO . cleanup)
  where
    -- Cleanup routine which:
    --  1. Ensures that no thread left running
    --  2. We continue execution only after all threads terminated
    cleanup (Shepherd tv) = do
      children <- readTVarIO tv
      -- Kill children
      sacrifice $ Map.keys children
      -- Wait until all threads terminate
      forM_ children takeMVar
    -- Sacrifice all threads from herd to gods of program
    -- stability. Weq need to take to special care to ensure that
    -- _all_ threads are terminated. Despite being run under mask we
    -- can receive asynchronous exception when calling killThread. If
    -- we do we receive exception we continue to kill remaining
    -- children in separate thread.
    sacrifice []          = return ()
    sacrifice tAll@(t:ts) = do
      killThread t `onException` forkIO (mapM_ killThread tAll)
      sacrifice ts

-- | Create new thread which will be terminated when we exit
--   corresponding 'withShepherd' block
newSheep
  :: (MonadFork m, MonadIO m, MonadMask m)
  => Shepherd -> m () -> m ()
newSheep shepherd action = do
  lock <- liftIO newEmptyMVar
  mask $ \restore -> do
    tid <- fork $ restore action `finally` liftIO (putMVar lock ())
    liftIO $ addSheep shepherd tid lock

-- | Create new thread which will be terminated when we exit
--   corresponding 'withShepherd' block. Finalizer action will be
--   called in any case on thread termination
newSheepFinally
  :: (MonadFork m, MonadIO m, MonadMask m)
  => Shepherd -> m () -> m () -> m ()
newSheepFinally shepherd action fini = do
  lock <- liftIO newEmptyMVar
  mask $ \restore -> do
    tid <- fork $ (restore action `finally` fini) `finally` liftIO (putMVar lock ())
    liftIO $ addSheep shepherd tid lock

addSheep :: Shepherd -> ThreadId -> MVar () -> IO ()
addSheep (Shepherd tv) tid lock = do
  atomically $ modifyTVar' tv $ Map.insert tid lock
