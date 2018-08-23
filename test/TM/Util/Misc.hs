-- | Some small useful functions
--
{-# LANGUAGE NumDecimals #-}
module TM.Util.Misc where


import Control.Concurrent (threadDelay)
import Control.Monad.IO.Class


-- | Wait for some seconds
--
waitSec :: (MonadIO m) => Double -> m ()
waitSec sec = liftIO $ threadDelay $ round $ 1e6 * sec

