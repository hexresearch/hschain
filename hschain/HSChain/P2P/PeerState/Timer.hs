{-# LANGUAGE RecordWildCards #-}

module HSChain.P2P.PeerState.Timer where

import Control.Monad
import Control.Monad.Catch
import Control.Monad.Trans.Cont
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Concurrent
import Control.Concurrent.STM

import HSChain.Control.Class

-- NOTE This implementation isn't very safe

data Timer = Timer { timerThread :: !(TMVar ThreadId)
                   , timerTMVar  :: !(TMVar ())
                   }

newTimer :: STM Timer
newTimer = Timer <$> newEmptyTMVar <*> newEmptyTMVar

newTimerIO :: MonadIO m => m Timer
newTimerIO = liftIO $
    Timer <$> newEmptyTMVarIO <*> newEmptyTMVarIO

reset :: MonadIO m => Timer -> Int -> m ()
reset t@Timer{..} delay = liftIO $ do
    cancel t

    n <- forkIO $ do
            threadDelay (1000*delay)
            void $ atomically $ tryPutTMVar timerTMVar ()

    atomically $ putTMVar timerThread n

cancel :: MonadIO m => Timer -> m ()
cancel Timer{..} = liftIO $ do
    tid <- atomically $ tryTakeTMVar timerThread
    mapM_ killThread tid

await :: Timer -> STM ()
await Timer{..} = takeTMVar timerTMVar
-- | Perform action every N ms (plus some delay introduced
--   by RTS)
timerForever :: Int -> IO a -> IO b
timerForever delay action = forever $ action >> threadDelay (1000*delay)

-- | Create thread which will be killed when main thread exits
linkedTimedIO :: (MonadIO m, MonadMask m) => Int -> IO a -> ContT r m ()
linkedTimedIO delay action = ContT $ \fun ->
  forkLinkedIO (timerForever delay action) (fun ())


linkedTimer :: (MonadIO m, MonadMask m) => Int -> TQueue a -> a -> ContT r m ()
linkedTimer delay ch a = linkedTimedIO delay (atomically $ writeTQueue ch a)
