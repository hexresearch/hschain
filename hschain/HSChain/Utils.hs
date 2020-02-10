-- | Some small but useful functions
module HSChain.Utils
  ( waitSec
  , waitMSec
  ) where


import Control.Concurrent (threadDelay)
import Control.Monad.IO.Class


-- | Thread delay in parameter expressed in seconds
waitSec :: (MonadIO m) => Double -> m ()
waitSec sec = liftIO $ threadDelay $ round $ 1e6 * sec
{-# INLINE waitSec #-}

-- | Thread delay in parameter expressed in milliseconds
waitMSec :: (MonadIO m, Integral a) => a -> m ()
waitMSec ms = liftIO $ threadDelay $ fromIntegral $ 1000 * ms
{-# INLINE waitMSec #-}
