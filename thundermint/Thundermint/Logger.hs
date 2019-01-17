{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE UndecidableInstances       #-}
-- |
-- Logger
module Thundermint.Logger (
    MonadLogger(..)
  , setNamespace
  , descendNamespace
  , LoggerT(..)
  , NoLogsT(..)
  , runLoggerT
  , logOnException
  , withLogEnv
  , newLogEnv
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
  ) where

import Control.Arrow (first,second)
import Control.Monad
import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Maybe  (MaybeT(..))
import Control.Exception          (SomeException(..),AsyncException(..))
import Data.Aeson
import Data.Aeson.TH
import Data.IORef
import Data.Text       (Text)
import Data.Typeable
import Data.Monoid     ((<>))
import qualified Data.HashMap.Strict        as HM
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Text as T
import Katip
import System.Directory (createDirectoryIfMissing)
import System.FilePath  (splitFileName)
import System.IO
import GHC.Generics (Generic)
import Network.HTTP.Client.TLS
import Database.V5.Bloodhound
import Katip.Scribes.ElasticSearch

import Thundermint.Control
import Thundermint.Types.Blockchain
import Thundermint.Debug.Trace
import Thundermint.Store.Internal.Query (MonadReadDB(..), MonadDB(..))


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

instance MonadLogger m => MonadLogger (MaybeT m) where
  logger sev str a = lift $ logger sev str a
  localNamespace fun (MaybeT m) = MaybeT (localNamespace fun m)

instance MonadLogger m => MonadLogger (ReaderT r m) where
  logger sev str a = lift $ logger sev str a
  localNamespace fun (ReaderT m) = ReaderT $ localNamespace fun . m

-- | Change logger's namespace
setNamespace :: MonadLogger m => Namespace -> m a -> m a
setNamespace nm = localNamespace (const nm)

-- | Append string to namespace
descendNamespace :: MonadLogger m => Text -> m a -> m a
descendNamespace nm = localNamespace $ \(Namespace x) -> Namespace (x ++ [nm])

-- | Concrete implementation of logger monad
newtype LoggerT m a = LoggerT (ReaderT (Namespace, LogEnv) m a)
  deriving ( Functor, Applicative, Monad
           , MonadIO, MonadThrow, MonadCatch, MonadMask, MonadTrans
           , MonadFork, MonadTrace)
instance (MonadReadDB m alg a) => MonadReadDB (LoggerT m) alg a where
  askConnectionRO = lift askConnectionRO
instance (MonadDB m alg a) => MonadDB (LoggerT m) alg a where
  askConnectionRW = lift askConnectionRW

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
           , MonadIO, MonadThrow, MonadCatch, MonadMask
           , MonadFork, MonadTrace)
instance (MonadReadDB m alg a) => MonadReadDB (NoLogsT m) alg a where
  askConnectionRO = lift askConnectionRO
instance (MonadDB m alg a) => MonadDB (NoLogsT m) alg a where
  askConnectionRW = lift askConnectionRW

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
          logger ErrorS "Killed by" (  sl "type" (show (typeOf eTy))
                                    <> sl "err"  (show eTy))
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
  = bracket initLE fini $ \leRef -> loop leRef (names `zip` scribes)
  where
    initLE = liftIO (newIORef =<< Katip.initLogEnv namespace env)
    -- N.B. We need IORef to pass LogEnv with all scribes to fini
    fini   = liftIO . (Katip.closeScribes <=< readIORef)
    --
    loop leRef []                = action =<< liftIO (readIORef leRef)
    loop leRef ((nm,ios) : rest) = mask $ \unmask -> do
      scribe <- liftIO ios
      le  <- liftIO (readIORef leRef)
      le' <- liftIO (Katip.registerScribe nm scribe Katip.defaultScribeSettings le)
        `onException` liftIO (Katip.scribeFinalizer scribe)
      liftIO (writeIORef leRef le')
      unmask $ loop leRef rest
    --
    names = [T.pack ("log_" ++ show i) | i <- [0::Int ..]]

-- | Initialize logging environment and add scribes.
newLogEnv
  :: (MonadIO m)
  => Katip.Namespace
  -> Katip.Environment
  -> [IO Katip.Scribe]          -- ^ List of scribes to add to environment
  -> m Katip.LogEnv
newLogEnv namespace env scribes = liftIO $ do
  le <- Katip.initLogEnv namespace env
  foldM addScribe le (names `zip` scribes)
  where
    addScribe le (nm,io) = do
      scribe <- io
      Katip.registerScribe nm scribe Katip.defaultScribeSettings le
    names = [T.pack ("log_" ++ show i) | i <- [0::Int ..]]


-- | Type of scribe to use
data ScribeType
  = ScribeJSON
  | ScribeES { elasticIndex :: Text }
  | ScribeTXT
  deriving (Show,Eq,Ord,Generic)
instance ToJSON   ScribeType where
  toJSON = \case
    ScribeJSON   -> String "ScribeJSON"
    ScribeTXT    -> String "ScribeTXT"
    ScribeES{..} -> object [ "tag"   .= ("ScribeES" :: Text)
                           , "index" .= elasticIndex
                           ]
instance FromJSON ScribeType where
  parseJSON (String "ScribeJSON") = return ScribeJSON
  parseJSON (String "ScribeTXT" ) = return ScribeTXT
  parseJSON (String _           ) = fail "Unknown string while decoding ScribeType"
  parseJSON (Object o) = do
    tag <- o .: "tag"
    unless (tag == ("ScribeES" :: Text)) $ fail "Unexpected tag for ScribeType"
    elasticIndex <- o .: "index"
    return ScribeES{..}
  parseJSON _                     = fail "Expecting string or object while decoding ScribeType"

-- | Description of scribe
data ScribeSpec = ScribeSpec
  { scribe'type      :: !ScribeType
    -- ^ Type of scribe to use
  , scribe'path      :: !(Maybe FilePath)
    -- ^ Log file if not specified will log to stdout
  , scribe'severity  :: !Severity
  , scribe'verbosity :: !Verbosity
  }
  deriving (Show,Eq,Ord,Generic)
deriveJSON defaultOptions
  { fieldLabelModifier = drop 7 } ''ScribeSpec

makeScribe :: ScribeSpec -> IO Scribe
makeScribe ScribeSpec{..} = do
  when needToPrepare $
    forM_ scribe'path $ \path -> do
      let (dir,_) = splitFileName path
      createDirectoryIfMissing True dir
  case (scribe'type, scribe'path) of
    (ScribeTXT,    Nothing) -> mkHandleScribe ColorIfTerminal stdout  sev verb
    (ScribeTXT,    Just nm) -> mkFileScribe nm sev verb
    (ScribeES{},   Nothing) -> error "Empty ES address"
    (ScribeES{..}, Just nm) -> makeEsUrlScribe nm elasticIndex sev verb
    (ScribeJSON,   Nothing) -> makeJsonHandleScribe stdout sev verb
    (ScribeJSON,   Just nm) -> makeJsonFileScribe nm sev verb
  where
    sev  = scribe'severity
    verb = scribe'verbosity
    needToPrepare = case scribe'type of
        ScribeES{} -> False
        _          -> True


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
-- ES (ElasticSearch) scribe
----------------------------------------------------------------

makeEsUrlScribe :: FilePath -> Text -> Severity -> Verbosity -> IO Scribe
makeEsUrlScribe serverPath index sev verb = do
  mgr <- newTlsManager
  let bhe = mkBHEnv (Server (T.pack serverPath)) mgr
      Just queSize = mkEsQueueSize 20
  mkEsScribe
    -- Reasonable for production
    defaultEsScribeCfgV5 { essQueueSize = queSize }
    -- Reasonable for single-node in development
    -- defaultEsScribeCfgV5 { essIndexSettings = IndexSettings (ShardCound 1) (ReplicaCount 0)}
    bhe
    (IndexName index)
    (MappingName "application-logs")
    sev
    verb

----------------------------------------------------------------
-- LogBlock
----------------------------------------------------------------

-- | Wrapper for log data for logging purposes
data LogBlockInfo a = LogBlockInfo !Height !a

instance BlockData a => ToObject (LogBlockInfo a) where
  toObject (LogBlockInfo (Height h) a)
    = HM.insert "H" (toJSON h)
    $ logBlockData a

instance BlockData a => LogItem (LogBlockInfo a) where
  payloadKeys Katip.V0 _ = Katip.SomeKeys ["H"]
  payloadKeys _        _ = Katip.AllKeys
