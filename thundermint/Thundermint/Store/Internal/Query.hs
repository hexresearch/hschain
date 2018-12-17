{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
-- |
module Thundermint.Store.Internal.Query (
    -- * Connection
    Connection(..)
  , connectionRO
  , openConnection
  , closeConnection
  , withConnection
  , MonadReadDB(..)
  , MonadDB(..)
    -- * Query wrappers
  , Access(..)
  , Query(..)
  , rollback
  , runQueryRO
  , runQueryRW
    -- * Primitives
  , query
  , query1
  , execute
  , execute_
  ) where

import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Maybe
import Data.Text (Text)
import Data.Coerce
import qualified Database.SQLite.Simple           as SQL

import Thundermint.Control



----------------------------------------------------------------
-- Connection to SQL database
----------------------------------------------------------------

-- | Connection to database for storage of blocks and user's
--   state. It's tagged by level of access, cryptographic algorithm,
--   and type of block. Latter two are needed to facilitate type inference
data Connection (rw :: Access) alg a = Connection !Mutex !SQL.Connection

connectionRO :: Connection rw alg a -> Connection 'RO alg a
connectionRO = coerce

openConnection :: MonadIO m => FilePath -> m (Connection rw alg a)
openConnection db
  = liftIO $ Connection <$> newMutex <*> SQL.open db

closeConnection :: MonadIO m => Connection 'RW alg a -> m ()
closeConnection (Connection _ c) = liftIO $ SQL.close c

withConnection
  :: (MonadMask m, MonadIO m)
  => FilePath -> (Connection rw alg a -> m x) -> m x
withConnection db = bracket (openConnection db) (closeConnection . coerce)

class MonadIO m => MonadReadDB m alg a | m -> alg a where
  askConnectionRO :: m (Connection 'RO alg a)

class MonadReadDB m alg a => MonadDB m alg a | m -> alg a where
  askConnectionRW :: m (Connection 'RW alg a)



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
newtype Query (rw :: Access) alg a x = Query
  { unQuery :: MaybeT (ReaderT SQL.Connection IO) x }
  deriving (Functor, Applicative, Monad)

-- | Cause transaction rollback.
rollback :: Query 'RW alg a x
rollback = fail "ROLLBACK"

-- | Run read-only query. Note that read-only couldn't be rolled back
--  so they always succeed
runQueryRO :: MonadIO m => Connection rw alg a -> Query 'RO alg a x -> m x
runQueryRO c q = do r <- liftIO . runQueryWorker c . unQuery $ q
                    case r of
                      Nothing -> error "Query 'RO couldn't be rolled back"
                      Just a  -> return a

-- | Run read-write query
runQueryRW  :: MonadIO m => Connection 'RW alg a -> Query 'RW alg a x -> m (Maybe x)
runQueryRW c = liftIO . runQueryWorker c . unQuery

runQueryWorker :: Connection rw alg a -> MaybeT (ReaderT SQL.Connection IO) x -> IO (Maybe x)
runQueryWorker (Connection mutex conn) sql = withMutex mutex $ uninterruptibleMask $ \restore -> do
  -- This function is rather tricky to get right. We need to maintain
  -- following invariant: No matter what happens we MUST call either
  -- ROLLBACK or COMMIT after BEGIN TRANSACTION. Otherwise database
  -- will be left inside a transaction and any attempt to start
  -- another will be met with error about nested transactions.
  --
  -- It seems that functions from sqlite-simple are interruptible so
  -- we need to use uninterruptibleMask
  --
  --  * BEGIN TRANSACTION does not acquire lock. It's deferred and
  --    lock is acquired only when database is actually accessed. So
  --    it's fast
  --
  --  * Neither ROLLBACK nor COMMIT should block but may take time for
  --    IO operations but that's inevitable
  --
  -- Therefore usage of uninterruptibleMask is justified
  --
  -- See following links for details
  --  + https://www.sqlite.org/lang_transaction.html
  --  + https://www.sqlite.org/lockingv3.html
  SQL.execute_ conn "BEGIN TRANSACTION"
  -- FIXME: exception safety. We need to rollback in presence of async
  --        exceptions
  r <- restore (runReaderT (runMaybeT sql) conn) `onException` rollbackTx
  case r of Nothing -> rollbackTx
            _       -> commitTx
  return r
  where
    rollbackTx = SQL.execute_ conn "ROLLBACK"
    commitTx   = SQL.execute_ conn "COMMIT"


----------------------------------------------------------------
-- Internal primitives
----------------------------------------------------------------

query
  :: (SQL.ToRow row, SQL.FromRow x)
  => Text                  -- ^ SQL query
  -> row                   -- ^ Query parameters
  -> Query rw alg a [x]
query sql param = Query $ do
  conn <- lift ask
  liftIO $ SQL.query conn (SQL.Query sql) param

query1
  :: (SQL.ToRow row, SQL.FromRow x)
  => Text                  -- ^ SQL query
  -> row                   -- ^ Query parameters
  -> Query rw alg a (Maybe x)
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
  -> Query rw alg a ()
execute sql param = Query $ do
  conn <- lift ask
  liftIO $ SQL.execute conn (SQL.Query sql) param

execute_
  :: Text                  -- ^ SQL query
  -> Query rw alg a ()
execute_ sql = Query $ do
  conn <- lift ask
  liftIO $ SQL.execute_ conn (SQL.Query sql)
