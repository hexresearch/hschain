-- | Some small but useful functions
--
{-# LANGUAGE CPP #-}
module Thundermint.Utils
   ( -- * PeerId encode/decode
     -- * Timings
     waitSec
   ) where


import Control.Concurrent (threadDelay)
import Control.Monad.IO.Class


-- | Wait a few seconds
--
waitSec :: (MonadIO m) => Double -> m ()
waitSec sec = liftIO $ threadDelay $ round $ 1e6 * sec
