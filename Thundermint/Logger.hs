{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TemplateHaskell            #-}
-- |
-- Logger
module Thundermint.Logger (
    MonadLogger(..)
  , setNamespace
  , LoggerT(..)
  , NoLogsT(..)
  , runLoggerT
  , logOnException
  , withLogEnv
    -- ** Scribe construction helpers
  , ScribeType(..)
  , ScribeSpec(..)
  , makeScribe
    -- * JSON scribe
  , makeJsonHandleScribe
  , makeJsonFileScribe
    -- * Reexports
  , Severity(..)
  , Verbosity(..)
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
import Data.Aeson.TH
import Data.Int
import Data.IORef
import Data.Typeable
import Data.Monoid     ((<>))
import qualified Data.HashMap.Strict        as HM
import qualified Data.ByteString.Lazy.Char8 as BL
import Katip
import System.Directory (createDirectoryIfMissing)
import System.FilePath  (splitFileName)
import System.IO
import GHC.Generics (Generic)

import Thundermint.Control
import Thundermint.Consensus.Types
import Thundermint.Debug.Trace

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

setNamespace :: MonadLogger m => Namespace -> m a -> m a
setNamespace nm = localNamespace (const nm)

-- | Concrete implementation of logger monad
newtype LoggerT m a = LoggerT (ReaderT (Namespace, LogEnv) m a)
  deriving ( Functor, Applicative, Monad
           , MonadIO, MonadThrow, MonadCatch, MonadMask, MonadTrans, MonadFork, MonadTrace )

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

-- | Mock logging. Useful for cases where constraints require logging
--   but we don't need any
newtype NoLogsT m a = NoLogsT { runNoLogsT :: m a }
  deriving ( Functor, Applicative, Monad
           , MonadIO, MonadThrow, MonadCatch, MonadMask, MonadFork, MonadTrace )
instance MonadTrans NoLogsT where
  lift = NoLogsT

instance MonadIO m => MonadLogger (NoLogsT m) where
  logger _ _ _ = return ()
  localNamespace _ a = a

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


-- | Initialize logging environment and add scribes. All scribes will
--   be closed when function returns.
withLogEnv
  :: (MonadIO m, MonadMask m)
  => Katip.Namespace
  -> Katip.Environment
  -> [IO Katip.Scribe]          -- ^ List of scribes to add to environment
  -> (Katip.LogEnv -> m a)
  -> m a
withLogEnv namespace env scribes action
  = bracket initLE fini $ \leRef -> loop leRef scribes
  where
    initLE = liftIO (newIORef =<< Katip.initLogEnv namespace env)
    -- N.B. We need IORef to pass LogEnv with all scribes to fini
    fini   = liftIO . (Katip.closeScribes <=< readIORef)
    --
    loop leRef []           = action =<< liftIO (readIORef leRef)
    loop leRef (ios : rest) = mask $ \unmask -> do
      scribe <- liftIO ios
      le  <- liftIO (readIORef leRef)
      le' <- liftIO (Katip.registerScribe "log" scribe Katip.defaultScribeSettings le)
        `onException` liftIO (Katip.scribeFinalizer scribe)
      liftIO (writeIORef leRef le')
      unmask $ loop leRef rest

-- | Type of scribe to use
data ScribeType
  = ScribeJSON
  | ScribeTXT
  deriving (Show,Eq,Ord,Generic)
instance ToJSON   ScribeType
instance FromJSON ScribeType

-- | Description of scribe
data ScribeSpec = ScribeSpec
  { scribe'type      :: ScribeType
    -- ^ Type of scribe to use
  , scribe'path      :: Maybe FilePath
    -- ^ Log file if not specified will log to stdout
  , scribe'severity  :: Severity
  , scribe'verbosity :: Verbosity
  }
  deriving (Show,Eq,Ord,Generic)
deriveJSON defaultOptions
  { fieldLabelModifier = drop 7 } ''ScribeSpec

makeScribe :: ScribeSpec -> IO Scribe
makeScribe ScribeSpec{..} = do
  forM_ scribe'path $ \path -> do
    let (dir,_) = splitFileName path
    createDirectoryIfMissing True dir
  case (scribe'type, scribe'path) of
    (ScribeTXT,  Nothing) -> mkHandleScribe ColorIfTerminal stdout  sev verb
    (ScribeTXT,  Just nm) -> mkFileScribe nm sev verb
    (ScribeJSON, Nothing) -> makeJsonHandleScribe stdout sev verb
    (ScribeJSON, Just nm) -> makeJsonFileScribe nm sev verb
  where
    sev  = scribe'severity
    verb = scribe'verbosity

instance ToJSON   Verbosity
instance FromJSON Verbosity

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
