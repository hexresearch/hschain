{-# LANGUAGE RecordWildCards #-}

module HSChain.P2P.PeerState.Timer where

import Control.Monad
import Control.Monad.Catch
import Control.Monad.Trans.Cont
import Control.Monad.IO.Class (MonadIO)
import Control.Concurrent
import Control.Concurrent.STM

import HSChain.Control

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
