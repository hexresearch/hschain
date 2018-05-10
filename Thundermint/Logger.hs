{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- |
-- Logger 
module Thundermint.Logger (
    MonadLogger(..)
  , LoggerT(..)
  , runLoggerT
  ) where

import Control.Arrow (first,second)
import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Katip

----------------------------------------------------------------
--
----------------------------------------------------------------

-- | Monad which supports logging. Unlike 'Katip' it ties namespace to
--   the monad.
class MonadIO m => MonadLogger m where
  logger :: LogItem a => Severity -> LogStr -> a -> m ()
  localNamespace :: (Namespace -> Namespace) -> m a -> m a

-- | Concrete implementation of logger monad
newtype LoggerT m a = LoggerT (ReaderT (Namespace, LogEnv) m a)
  deriving ( Functor, Applicative, Monad
           , MonadIO, MonadThrow, MonadCatch, MonadMask, MonadTrans)

runLoggerT :: Namespace -> LogEnv -> LoggerT m a -> m a
runLoggerT nm le (LoggerT m) = runReaderT m (nm,le)

instance MonadIO m => Katip (LoggerT m) where
  getLogEnv = LoggerT $ fmap snd ask
  localLogEnv f (LoggerT m) = LoggerT $ local (second f) m
  
instance MonadIO m => MonadLogger (LoggerT m) where
  logger sev s a = do
    (nm,_) <- LoggerT ask
    logF a nm sev s
  localNamespace f (LoggerT m) = LoggerT $ local (first f) m
