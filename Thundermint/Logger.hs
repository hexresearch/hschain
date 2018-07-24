{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
-- |
-- Logger
module Thundermint.Logger (
    MonadLogger(..)
  , LoggerT(..)
  , runLoggerT
  , logOnException
    -- * JSON scribe
  , makeJsonHandleScribe
  , makeJsonFileScribe
    -- * Reexports
  , Severity(..)
    -- * Structured logging
  , LogBlockInfo(..)
  , LogBlock(..)
  ) where

import Control.Arrow (first,second)
import Control.Monad
import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Control.Exception          (SomeException(..),AsyncException(..))
import Data.Aeson
import Data.Int
import Data.Typeable
import Data.Monoid     ((<>))
import qualified Data.HashMap.Strict        as HM
import qualified Data.ByteString.Lazy.Char8 as BL
import Katip
import System.IO

import Thundermint.Control
import Thundermint.Consensus.Types

----------------------------------------------------------------
--
----------------------------------------------------------------

-- | Monad which supports logging. Unlike 'Katip' it ties namespace to
--   the monad.
class Monad m => MonadLogger m where
  -- | Log value
  logger :: LogItem a => Severity -> LogStr -> a -> m ()
  -- | Change current namespace
  localNamespace :: (Namespace -> Namespace) -> m a -> m a

-- | Concrete implementation of logger monad
newtype LoggerT m a = LoggerT (ReaderT (Namespace, LogEnv) m a)
  deriving ( Functor, Applicative, Monad
           , MonadIO, MonadThrow, MonadCatch, MonadMask, MonadTrans, MonadFork)

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

-- | Log exceptions at Error severity
logOnException :: (MonadLogger m, MonadCatch m) => m a -> m a
logOnException = handle logE
  where
    logE e
      | Just ThreadKilled <- fromException e = do
          logger InfoS "Killed normally by ThreadKilled" ()
          throwM e
      | SomeException eTy <- e               = do
          logger ErrorS ("Killed by " <> showLS e <> " :: " <> showLS (typeOf eTy)) ()
          throwM e


----------------------------------------------------------------
-- JSON scribe
----------------------------------------------------------------

makeJsonHandleScribe :: Handle -> Severity -> Verbosity -> IO Scribe
makeJsonHandleScribe  h sev verb = do
  hSetBuffering h LineBuffering
  lock <- newMutex
  let loggerFun i@Item{..} = when (permitItem sev i) $ withMutex lock $
        BL.hPutStrLn h $ encode Item
          { _itemPayload = Object (payloadObject verb _itemPayload)
          , ..
          }
  return $ Scribe loggerFun (hFlush h)

makeJsonFileScribe :: FilePath -> Severity -> Verbosity -> IO Scribe
makeJsonFileScribe nm sev verb = do
  h <- openFile nm AppendMode
  Scribe loggerFun finalizer <- makeJsonHandleScribe h sev verb
  return $ Scribe loggerFun (finalizer `finally` hClose h)


----------------------------------------------------------------
-- JSON scribe
----------------------------------------------------------------

-- | Type class for providing logging information about block
class LogBlock a where
  logBlockData :: a -> Object
  logBlockData _ = HM.empty

instance LogBlock Int64
instance LogBlock [a] where
  logBlockData txs = HM.singleton "Ntx" (toJSON (length txs))

-- | Wrapper for log data for logging purposes
data LogBlockInfo a = LogBlockInfo Height a

instance LogBlock a => ToObject (LogBlockInfo a) where
  toObject (LogBlockInfo (Height h) a)
    = HM.insert "H" (toJSON h)
    $ logBlockData a

instance LogBlock a => LogItem (LogBlockInfo a) where
  payloadKeys Katip.V0 _ = Katip.SomeKeys ["H"]
  payloadKeys _        _ = Katip.AllKeys
