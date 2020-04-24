-- |
module HSChain.PoW.P2P.Handler.CatchupLock
  ( CatchupThrottle(..)
  , ReleaseCatchupThrottle(..)
  , newCatchupThrottle
  ) where

import Control.Concurrent (forkIO)
import Control.Concurrent.STM
import Control.Monad.Catch
import Control.Monad.Cont

import HSChain.Control.Class
import HSChain.Control.Delay
import HSChain.PoW.P2P.Types


----------------------------------------------------------------
-- Public API
----------------------------------------------------------------

newCatchupThrottle
  :: (MonadMask m, MonadFork m)
  => ContT r m CatchupThrottle
newCatchupThrottle = do
  counter <- liftIO $ newTVarIO (0 :: Int)
  free    <- liftIO $ newTVarIO True
  ch      <- liftIO   newTQueueIO
  --
  cforkLinkedIO $ timerThread ch
  --
  return $ CatchupThrottle $ do
    -- Take lock that is not taken and increase count.
    check =<< readTVar free
    writeTVar free False
    nLock <- modifyTVarRet counter (+1)
    -- We release timer if counter wasn't incremented which means new
    -- locks wasn't taken
    let release = do n <- readTVar counter
                     when (n == nLock) $ writeTVar free True
    writeTQueue ch release
    return $ ReleaseCatchupThrottle release




timerThread
  :: TQueue (STM ())
  -> IO x
timerThread ch = forever $ do
  release <- atomically $ readTQueue ch
  forkIO $ waitSec 1 >> atomically release

modifyTVarRet :: TVar a -> (a -> a) -> STM a
modifyTVarRet v f = modifyTVar' v f >> readTVar v
