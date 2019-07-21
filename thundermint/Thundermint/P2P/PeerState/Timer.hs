{-# LANGUAGE RecordWildCards #-}

module Thundermint.P2P.PeerState.Timer where

import Control.Monad(void, mapM_)

import Control.Monad.IO.Class (MonadIO, liftIO)

import Control.Concurrent
import Control.Concurrent.STM

-- NOTE This implementation isn't very safe

data Timer = Timer { timerThread :: !(TMVar ThreadId)
                   , timerTMVar  :: !(TMVar ())
                   }

new :: STM Timer
new = Timer <$> newEmptyTMVar <*> newEmptyTMVar

newIO :: MonadIO m => m Timer
newIO = liftIO $
    Timer <$> newEmptyTMVarIO <*> newEmptyTMVarIO

reset :: MonadIO m => Timer -> Int -> m ()
reset t@Timer{..} i = liftIO $ do
    cancel t

    n <- forkIO $ do
            threadDelay i
            void $ atomically $ tryPutTMVar timerTMVar ()

    atomically $ putTMVar timerThread n

cancel :: MonadIO m => Timer -> m ()
cancel Timer{..} = liftIO $ do
    tid <- atomically $ tryTakeTMVar timerThread
    mapM_ killThread tid

await :: Timer -> STM ()
await Timer{..} = takeTMVar timerTMVar
