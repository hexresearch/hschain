{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
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
module HSChain.Logger (
    MonadLogger(..)
  , setNamespace
  , descendNamespace
    -- ** Monad transformers
  , LoggerT(..)
  , runLoggerT
  , withLogEnv
  , newLogEnv
  , NoLogsT(..)
  , StdoutLogT(..)
  , runStdoutLogT
  , logOnException
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
import Control.Monad.Morph        (MFunctor(..))
import Control.Monad.Fail         (MonadFail)
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader

import Control.Exception          (SomeException(..),AsyncException(..))
import Data.Aeson
import Data.Aeson.TH
import Data.IORef
import qualified Data.Text.Lazy as TL
import           Data.Text.Lazy.Builder (toLazyText)
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

import HSChain.Control.Mutex
import HSChain.Control.Class
import HSChain.Types.Blockchain
import HSChain.Logger.Class
import HSChain.Store.Internal.Query (MonadReadDB(..), MonadDB(..))


----------------------------------------------------------------
--
----------------------------------------------------------------

-- | Concrete implementation of logger monad
newtype LoggerT m a = LoggerT (ReaderT (Namespace, LogEnv) m a)
  deriving ( Functor, Applicative, Monad
           , MonadIO, MonadThrow, MonadCatch, MonadMask, MonadTrans
           , MonadFork, MonadFail)

instance MFunctor LoggerT where
  hoist f (LoggerT m) = LoggerT (hoist f m)
instance (MonadReadDB m a) => MonadReadDB (LoggerT m) a where
instance (MonadDB m a) => MonadDB (LoggerT m) a where

runLoggerT :: LogEnv -> LoggerT m a -> m a
runLoggerT le (LoggerT m) = runReaderT m (mempty, le)

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
  deriving newtype ( Functor, Applicative, Monad, MonadFail
                   , MonadIO, MonadThrow, MonadCatch, MonadMask
                   , MonadFork)

instance MFunctor NoLogsT where
  hoist f (NoLogsT m) = NoLogsT (f m)
instance (MonadReadDB m a) => MonadReadDB (NoLogsT m) a where
instance (MonadDB m a) => MonadDB (NoLogsT m) a where

instance MonadTrans NoLogsT where
  lift = NoLogsT

instance MonadIO m => MonadLogger (NoLogsT m) where
  logger _ _ _ = return ()
  localNamespace _ a = a


-- | Simple monad transformer for writing logs to stdout. Motly useful
--   for debugging when one need to add remove some logging capability
--   quickly.
newtype StdoutLogT m a = StdoutLogT { unStdoutLogT :: ReaderT Namespace m a }
  deriving newtype ( Functor, Applicative, Monad, MonadFail
                   , MonadIO, MonadThrow, MonadCatch, MonadMask
                   , MonadFork
                   )

runStdoutLogT :: StdoutLogT m a -> m a
runStdoutLogT = flip runReaderT mempty . unStdoutLogT

instance MonadTrans StdoutLogT where
  lift = StdoutLogT . lift

instance MonadIO m => MonadLogger (StdoutLogT m) where
  logger _ msg a = do
    Namespace chunks <- StdoutLogT ask
    liftIO $ putStr $ T.unpack $ case chunks of
      [] -> ""
      _  -> T.intercalate "." chunks
    liftIO $ putStrLn $ TL.unpack $ toLazyText $ unLogStr msg
    liftIO $ forM_ (HM.toList $ toObject a) $ \(k,v) -> do
      putStr $ "  " ++ T.unpack k ++ " = "
      print v
  localNamespace f = StdoutLogT . local f . unStdoutLogT


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
  | ScribeTXT
  deriving (Show,Eq,Ord,Generic)
instance ToJSON   ScribeType where
  toJSON = \case
    ScribeJSON   -> String "ScribeJSON"
    ScribeTXT    -> String "ScribeTXT"
instance FromJSON ScribeType where
  parseJSON (String "ScribeJSON") = return ScribeJSON
  parseJSON (String "ScribeTXT" ) = return ScribeTXT
  parseJSON (String _           ) = fail "Unknown string while decoding ScribeType"
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
  forM_ scribe'path $ \path -> do
    let (dir,_) = splitFileName path
    createDirectoryIfMissing True dir
  case (scribe'type, scribe'path) of
    (ScribeTXT,    Nothing) -> mkHandleScribe ColorIfTerminal stdout  sev verb
    (ScribeTXT,    Just nm) -> mkFileScribe nm sev verb
    (ScribeJSON,   Nothing) -> makeJsonHandleScribe stdout sev verb
    (ScribeJSON,   Just nm) -> makeJsonFileScribe nm sev verb
  where
    sev  = scribe'severity
    verb = scribe'verbosity


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
-- LogBlock
----------------------------------------------------------------

-- | Wrapper for log data for logging purposes
data LogBlockInfo a = LogBlockInfo !Height !a !Int

instance BlockData a => ToObject (LogBlockInfo a) where
  toObject (LogBlockInfo (Height h) a ns)
    = HM.insert "H"     (toJSON h)
    $ HM.insert "nsign" (toJSON ns)
    $ logBlockData a

instance BlockData a => LogItem (LogBlockInfo a) where
  payloadKeys Katip.V0 _ = Katip.SomeKeys ["H"]
  payloadKeys _        _ = Katip.AllKeys
