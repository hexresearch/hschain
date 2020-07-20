-- |
module HSChain.Control.Util
  ( -- * Lifted functions
    atomicallyIO
    -- * throwing on Maybe and Either
  , throwNothing
  , throwNothingM
  , throwLeft
  , throwLeftM
    -- * Utils
  , iterateM
  ) where

import Control.Concurrent.STM
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Catch


atomicallyIO :: MonadIO m => STM a -> m a
atomicallyIO = liftIO . atomically

iterateM :: (Monad m) => a -> (a -> m a) -> m b
iterateM x0 f = let loop = f >=> loop in loop x0

----------------------------------------------------------------
--
----------------------------------------------------------------

throwNothing :: (Exception e, MonadThrow m) => e -> Maybe a -> m a
throwNothing e = maybe (throwM e) pure

throwNothingM :: (Exception e, MonadThrow m) => e -> m (Maybe a) -> m a
throwNothingM e m = m >>= throwNothing e

throwLeft :: (Exception e, MonadThrow m) => Either e a -> m a
throwLeft = either throwM pure

throwLeftM :: (Exception e, MonadThrow m) => m (Either e a) -> m a
throwLeftM m = m >>= throwLeft
