-- |HSChain.PoW.Mining
--
-- A mining process for HSChain.
--
-- Copyright (C) ...

module HSChain.PoW.Mining
  ( miningProcess
  ) where

import Control.Concurrent
import Control.Concurrent.STM

-- |Start mining process.
--
-- The process returns a channel to put requests to, a channel to
-- get mining results and shutdown call.
miningProcess :: (MonadIO m, BlockData b)
              => m (TChan (Header b, [Tx b], [Tx b]), TChan (Block b), m ())
miningProcess = do
  stopped <- atomically $ newTVar (-1)
  requests <- atomically newBroadcastTChan
  results <- atomically newTChan
  n <- return 4
  miners <- forM [1..n] $ \n -> do
    ch <- atomically newTChan
    liftIO $ forkIO $ mine stopped ch
    return ch
  let stop = do
        atomically $ writeTVar stopped n
        atomically $ do
          n' <- readTVar stopped
          if n' < n then retry else return ()
  return (requests, results, stop)

mine :: BlockData b => TVar Int -> TChan (Header b, [Tx b]) -> IO ()
mine stopped requests = do
  stop <- atomically $ do
    x <- readTVar stopped
    writeTVar stopped (if x <= 0 then x else (x-1))
    return $ x > 0
  if stop
    then return ()
    else do
  request <- atomically $ readTChan requests
  case request of
    Nothing -
