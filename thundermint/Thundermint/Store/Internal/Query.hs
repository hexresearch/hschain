{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE ScopedTypeVariables        #-}
-- |
module Thundermint.Store.Internal.Query (
    -- * Connection
    Connection(..)
  , openConnection
  , closeConnection
  , withConnection
    -- * Query wrappers
  , Access(..)
  , Query(..)
  , QueryRO(..)
  , toQueryRO
  , rollback
  , RunQueryRO(..)
  , RunQueryRW(..)
    -- * Primitives
  , query
  , query1
  , execute
  ) where

import Codec.Serialise (Serialise,serialise,deserialiseOrFail)
import Control.Applicative
import Control.Monad
import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.State
import Data.Typeable
import Data.Text (Text)
import Data.IORef
import Data.Int
import           Data.Map.Strict   (Map)
import qualified Data.Map.Strict as Map
import qualified Database.SQLite.Simple as SQL
import qualified Database.SQLite.Simple.ToField as SQL
import qualified Database.SQLite.Simple.FromField as SQL
import           Database.SQLite.Simple   (Only(..))
import Lens.Micro
import Lens.Micro.Mtl

import Thundermint.Blockchain.Types
import Thundermint.Control
import Thundermint.Store.Internal.Types



----------------------------------------------------------------
-- Connection to SQL database
----------------------------------------------------------------

-- | Connection to database. It's just thin wrapper around underlying
--   connection and mutex to serialize access to DB
data Connection = Connection !Mutex !SQL.Connection

openConnection :: MonadIO m => FilePath -> m Connection
openConnection db
  = liftIO $ Connection <$> newMutex <*> SQL.open db

closeConnection :: MonadIO m => Connection -> m ()
closeConnection (Connection _ c) = liftIO $ SQL.close c

withConnection
  :: (MonadMask m, MonadIO m)
  => FilePath -> (Connection -> m a) -> m a
withConnection db = bracket (openConnection db) closeConnection


----------------------------------------------------------------
-- SQL wrappers
----------------------------------------------------------------

-- | Access rights for storage or whether database queries alter
--   underlying data
data Access = RO                -- ^ Read-only access
            | RW                -- ^ Read-write access
            deriving (Show)

-- | Monad for executing queries to database. All queries are executed
--   in single transaction and commited upon completion or rolled back
--   on any error. Calling 'fail' is supported and will cause normal
--   transaction rollback.
--
--   Note that while this type wraps 'IO' it doesn't have 'MonadIO'
--   instance since we don't want allow arbitrary IO.
newtype Query (rw :: Access) a = Query
  { unQuery :: MaybeT (ReaderT SQL.Connection IO) a }
  deriving (Functor, Applicative, Monad)

-- | Same as 'Query' but only read-only queries could be executed.
newtype QueryRO (rw :: Access) a = QueryRO
  { unQueryRO :: MaybeT (ReaderT SQL.Connection IO) a }
  deriving (Functor, Applicative, Monad)

-- | Only allow read-only queries
toQueryRO :: Query rw a -> QueryRO rw a
toQueryRO (Query m) = QueryRO m

-- | Cause transaction rollback.
rollback :: Monad m => m a
rollback = fail "ROLLBACK"

-- | Run read-only query. Note that read-only couldn't be rolled back
--  so they always succeed
class RunQueryRO q where
  runQueryRO :: MonadIO m => Connection -> q 'RO a -> m a

-- | Run read-write query
class RunQueryRW q where
  runQueryRW  :: MonadIO m => Connection -> q 'RW a -> m (Maybe a)

instance RunQueryRO QueryRO where
  runQueryRO c q = do r <-  liftIO . runQueryWorker c . unQueryRO $ q
                      case r of
                        Nothing -> error "QueryRO couldn't be rolled back"
                        Just a  -> return a

instance RunQueryRO Query where
  runQueryRO c q = do r <-  liftIO . runQueryWorker c . unQuery $ q
                      case r of
                        Nothing -> error "QueryRO couldn't be rolled back"
                        Just a  -> return a

instance RunQueryRW Query where
  runQueryRW c = liftIO . runQueryWorker c . unQuery

runQueryWorker :: Connection -> MaybeT (ReaderT SQL.Connection IO) a -> IO (Maybe a)
runQueryWorker (Connection mutex conn) query = withMutex mutex $ do
  SQL.execute_ conn "BEGIN TRANSACTION"
  -- FIXME: exception safety. We need to rollback in presence of async
  --        exceptions
  r <- try $ runReaderT (runMaybeT query) conn
  case r of
    Left (e :: SomeException) -> Nothing <$ SQL.execute_ conn "ROLLBACK"
    Right Nothing             -> Nothing <$ SQL.execute_ conn "ROLLBACK"
    Right a                   -> a       <$ SQL.execute_ conn "COMMIT"

----------------------------------------------------------------
-- Internal primitives
----------------------------------------------------------------

query
  :: (SQL.ToRow row, SQL.FromRow a)
  => Text                  -- ^ SQL query
  -> row                   -- ^ Query parameters
  -> Query rw [a]
query sql param = Query $ do
  conn <- lift ask
  liftIO $ SQL.query conn (SQL.Query sql) param

query1
  :: (SQL.ToRow row, SQL.FromRow a)
  => Text                  -- ^ SQL query
  -> row                   -- ^ Query parameters
  -> Query rw (Maybe a)
query1 sql param = Query $ do
  conn <- lift ask
  r    <- liftIO $ SQL.query conn (SQL.Query sql) param
  case r of
    []  -> return Nothing
    [x] -> return $ Just x
    _   -> error "Impossible"

execute
  :: (SQL.ToRow row)
  => Text                  -- ^ SQL query
  -> row                   -- ^ Query parameters
  -> Query rw ()
execute sql param = Query $ do
  conn <- lift ask
  liftIO $ SQL.execute conn (SQL.Query sql) param
