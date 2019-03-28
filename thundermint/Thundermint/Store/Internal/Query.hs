{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE UndecidableInstances       #-}
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
    -- * MonadQuery
  , MonadQueryRO(..)
  , basicQuery1
  , MonadQueryRW(..)
    -- * Database queries
  , Access(..)
    -- ** Monad transformer
  , QueryT(..)
  , runQueryROT
  , runQueryRWT
  , queryROT
  , queryRWT
    -- ** Plain queries
  , Query(..)
  , runQueryRO
  , runQueryRW
  , queryRO
  , queryRW
  ) where

import Control.Monad.Catch
import qualified Control.Monad.Fail as Fail
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.State.Strict as SS(StateT(..))
import Control.Monad.Trans.State.Lazy   as SL(StateT(..))
import Control.Monad.Trans.Except            (ExceptT(..))
import Control.Monad.Trans.Identity          (IdentityT(..))
import Data.Coerce
import qualified Database.SQLite.Simple           as SQL

import Thundermint.Control



----------------------------------------------------------------
-- Connection to SQL database
----------------------------------------------------------------

-- | Access rights for storage or whether database queries alter
--   underlying data
data Access = RO                -- ^ Read-only access
            | RW                -- ^ Read-write access
            deriving (Show)

-- | Connection to database for storage of blocks and user's
--   state. It's tagged by level of access, cryptographic algorithm,
--   and type of block. Latter two are needed to facilitate type
--   inference for functions like @retrieveBlock@ where @alg@ & @a@
--   appear only in return type and frequently couldn't be inferred
--   without tags.
data Connection (rw :: Access) alg a = Connection !Mutex !SQL.Connection

-- | Convert read-only or read-write connection to read-only one.
connectionRO :: Connection rw alg a -> Connection 'RO alg a
connectionRO = coerce

-- | Open connection to database and set necessary pragmas
openConnection :: MonadIO m => FilePath -> m (Connection rw alg a)
openConnection db = liftIO $ do
  m <- newMutex
  c <- SQL.open db
  -- SQLite have support for retrying transactions in case database is
  -- busy. Here we switch ot on
  SQL.execute_ c "PRAGMA busy_timeout = 10"
  return $! Connection m c

-- | Close connection to database. Note that we can only close
--   connection if we have read-write acces to it.
closeConnection :: MonadIO m => Connection 'RW alg a -> m ()
closeConnection (Connection _ c) = liftIO $ SQL.close c

withConnection
  :: (MonadMask m, MonadIO m)
  => FilePath -> (Connection rw alg a -> m x) -> m x
withConnection db = bracket (openConnection db) (closeConnection . coerce)


----------------------------------------------------------------
-- Monadic API
----------------------------------------------------------------

-- | Monad which provides access to read-only connection to database
class Monad m => MonadReadDB m alg a | m -> alg a where
  askConnectionRO :: m (Connection 'RO alg a)

-- | Monad which provides access to read-write connection to database
class MonadReadDB m alg a => MonadDB m alg a | m -> alg a where
  askConnectionRW :: m (Connection 'RW alg a)

-- | Monad which allows to perform read-only queries. API is just thin
--   wrappers about @sqlite-simple@ functions thus they shouldn't be
--   used directly. Also note that it doesn't have 'MonadIO' superclass.
class MonadReadDB m alg a => MonadQueryRO m alg a | m -> alg a where
  basicQuery    :: (SQL.ToRow p, SQL.FromRow q) => SQL.Query -> p -> m [q]
  basicQuery_   :: (SQL.FromRow q)              => SQL.Query -> m [q]

basicQuery1 :: (SQL.ToRow row, SQL.FromRow x, MonadQueryRO m alg a)
  => SQL.Query             -- ^ SQL query
  -> row                   -- ^ Query parameters
  -> m (Maybe x)
basicQuery1 sql param =
  basicQuery sql param >>= \case
    []  -> return Nothing
    [x] -> return $ Just x
    _   -> error "Impossible"


-- | Monad which can perform read-write queries. @basic*RW@ functions
--   are same as @RO@ variants and intended to be used as 
class (MonadDB m alg a, MonadQueryRO m alg a) => MonadQueryRW m alg a | m -> alg a where
  basicExecute  :: (SQL.ToRow p) => SQL.Query -> p -> m ()
  basicExecute_ :: ()            => SQL.Query ->      m ()
  -- | Roll back execution of transaction. It's executed as throwing
  --   of exception which is caught in the 'runQueryT'. Thus other
  --   effects in monadic stack may or may not be rolled back as well.
  rollback      :: m x


instance MonadReadDB m alg a => MonadReadDB (IdentityT m) alg a where
  askConnectionRO = lift askConnectionRO
instance MonadDB m alg a => MonadDB (IdentityT m) alg a where
  askConnectionRW = lift askConnectionRW
instance MonadQueryRO m alg a => MonadQueryRO (IdentityT m) alg a where
  basicQuery sql p = lift $ basicQuery sql p
  basicQuery_      = lift . basicQuery_
instance MonadQueryRW m alg a => MonadQueryRW (IdentityT m) alg a where
  basicExecute sql p = lift $ basicExecute sql p
  basicExecute_      = lift . basicExecute_
  rollback           = lift rollback

instance MonadReadDB m alg a => MonadReadDB (MaybeT m) alg a where
  askConnectionRO = lift askConnectionRO
instance MonadDB m alg a => MonadDB (MaybeT m) alg a where
  askConnectionRW = lift askConnectionRW
instance MonadQueryRO m alg a => MonadQueryRO (MaybeT m) alg a where
  basicQuery sql p = lift $ basicQuery sql p
  basicQuery_      = lift . basicQuery_
instance MonadQueryRW m alg a => MonadQueryRW (MaybeT m) alg a where
  basicExecute sql p = lift $ basicExecute sql p
  basicExecute_      = lift . basicExecute_
  rollback           = lift rollback

instance MonadReadDB m alg a => MonadReadDB (ExceptT e m) alg a where
  askConnectionRO = lift askConnectionRO
instance MonadDB m alg a => MonadDB (ExceptT e m) alg a where
  askConnectionRW = lift askConnectionRW
instance MonadQueryRO m alg a => MonadQueryRO (ExceptT e m) alg a where
  basicQuery sql p = lift $ basicQuery sql p
  basicQuery_      = lift . basicQuery_
instance MonadQueryRW m alg a => MonadQueryRW (ExceptT e m) alg a where
  basicExecute sql p = lift $ basicExecute sql p
  basicExecute_      = lift . basicExecute_
  rollback           = lift rollback

instance MonadReadDB m alg a => MonadReadDB (ReaderT r m) alg a where
  askConnectionRO = lift askConnectionRO
instance MonadDB m alg a => MonadDB (ReaderT r m) alg a where
  askConnectionRW = lift askConnectionRW
instance MonadQueryRO m alg a => MonadQueryRO (ReaderT r m) alg a where
  basicQuery sql p = lift $ basicQuery sql p
  basicQuery_      = lift . basicQuery_
instance MonadQueryRW m alg a => MonadQueryRW (ReaderT r m) alg a where
  basicExecute sql p = lift $ basicExecute sql p
  basicExecute_      = lift . basicExecute_
  rollback           = lift rollback

instance MonadReadDB m alg a => MonadReadDB (SS.StateT s m) alg a where
  askConnectionRO = lift askConnectionRO
instance MonadDB m alg a => MonadDB (SS.StateT s m) alg a where
  askConnectionRW = lift askConnectionRW
instance MonadQueryRO m alg a => MonadQueryRO (SS.StateT s m) alg a where
  basicQuery sql p = lift $ basicQuery sql p
  basicQuery_      = lift . basicQuery_
instance MonadQueryRW m alg a => MonadQueryRW (SS.StateT s m) alg a where
  basicExecute sql p = lift $ basicExecute sql p
  basicExecute_      = lift . basicExecute_
  rollback           = lift rollback

instance MonadReadDB m alg a => MonadReadDB (SL.StateT s m) alg a where
  askConnectionRO = lift askConnectionRO
instance MonadDB m alg a => MonadDB (SL.StateT s m) alg a where
  askConnectionRW = lift askConnectionRW
instance MonadQueryRO m alg a => MonadQueryRO (SL.StateT s m) alg a where
  basicQuery sql p = lift $ basicQuery sql p
  basicQuery_      = lift . basicQuery_
instance MonadQueryRW m alg a => MonadQueryRW (SL.StateT s m) alg a where
  basicExecute sql p = lift $ basicExecute sql p
  basicExecute_      = lift . basicExecute_
  rollback           = lift rollback



----------------------------------------------------------------
-- Monad transformer
----------------------------------------------------------------

-- | Monad transormer for executing queries to the database. By itself
--   it's just thin wrapper which signifies that every action in the
--   monad is performed inside transaction.
--
--   Note about transaction semantics. Rollback of transaction is
--   implemented by throwing and catching exception. So depending on
--   semantics of monadic stack effects may or may not be rolled back
--   if DB transaction does not succeed. For example changes to
--   @StateT@ will while @IO@-effects obviously won't. Proceed with
--   caution.
newtype QueryT (rw :: Access) alg a m x = QueryT { unQueryT :: m x }
  deriving newtype ( Functor, Applicative, MonadIO, MonadThrow, MonadCatch, MonadMask) 

instance MonadThrow m => Monad (QueryT rw alg a m) where
  return         = QueryT . return
  QueryT m >>= f = QueryT $ unQueryT . f =<< m
  fail _         = throwM Rollback

instance MonadThrow m => Fail.MonadFail (QueryT rw alg a m) where
  fail _ = throwM Rollback

-- | Exception thrown in order to roll back transaction
data Rollback = Rollback
  deriving (Show,Eq)
instance Exception Rollback


instance MonadTrans (QueryT rr alg a) where
  lift = QueryT

instance (MonadThrow m, MonadReadDB m alg a) => MonadReadDB (QueryT rw alg a m) alg a where
  askConnectionRO = QueryT askConnectionRO

instance (MonadThrow m, MonadDB m alg a) => MonadDB (QueryT 'RW alg a m) alg a where
  askConnectionRW = QueryT askConnectionRW

instance (MonadIO m, MonadThrow m, MonadReadDB m alg a) => MonadQueryRO (QueryT rw alg a m) alg a where
  basicQuery sql param = do
    Connection _ conn <- askConnectionRO
    liftIO $ SQL.query conn sql param
  basicQuery_ sql = do
    Connection _ conn <- askConnectionRO
    liftIO $ SQL.query_ conn sql

instance (MonadIO m, MonadThrow m, MonadDB m alg a) => MonadQueryRW (QueryT 'RW alg a m) alg a where
  basicExecute  sql param = do
    Connection _ conn <- askConnectionRW
    liftIO $ SQL.execute conn sql param
  basicExecute_ sql = do
    Connection _ conn <- askConnectionRW
    liftIO $ SQL.execute_ conn sql
  rollback      = throwM Rollback

-- | Run read-only query. Note that read-only couldn't be rolled back
--  so they always succeed
runQueryROT
  :: (MonadIO m, MonadMask m)
  => Connection rw alg a
  -> QueryT 'RO alg a m x -> m x
runQueryROT c q = do
  r <- runQueryWorker False c . unQueryT $ q
  case r of
    Nothing -> error "Query 'RO couldn't be rolled back"
    Just a  -> return a

-- | Run read-write query
runQueryRWT
  :: (MonadIO m, MonadMask m)
  => Connection 'RW alg a
  -> QueryT 'RW alg a m x
  -> m (Maybe x)
runQueryRWT c = runQueryWorker True c . unQueryT

queryROT
  :: (MonadReadDB m alg a, MonadIO m, MonadMask m)
  => QueryT 'RO alg a m x
  -> m x
queryROT q = flip runQueryROT q =<< askConnectionRO

queryRWT
  :: (MonadDB m alg a, MonadIO m, MonadMask m)
  => QueryT 'RW alg a m x
  -> m (Maybe x)
queryRWT q = flip runQueryRWT q =<< askConnectionRW


----------------------------------------------------------------
-- Query monad
----------------------------------------------------------------

-- | Query which doesn't allow any other effect except interaction
--   with database.
newtype Query rw alg a x = Query (ReaderT (Connection rw alg a) IO x)
  deriving newtype (Functor, Applicative)

instance Monad (Query rm alg a) where
  return = Query . return
  Query m >>= f = Query $ (\(Query q) -> q) . f =<< m
  fail _ = Query $ throwM Rollback

instance Fail.MonadFail (Query rw alg a) where
  fail _ = Query $ throwM Rollback

instance MonadReadDB (Query rw alg a) alg a where
  askConnectionRO = Query $ asks connectionRO

instance MonadDB (Query 'RW alg a) alg a where
  askConnectionRW = Query ask

instance MonadQueryRO (Query rw alg a) alg a where
  basicQuery sql param = do
    Connection _ conn <- askConnectionRO
    Query $ liftIO $ SQL.query conn sql param
  basicQuery_ sql = do
    Connection _ conn <- askConnectionRO
    Query $ liftIO $ SQL.query_ conn sql

instance MonadQueryRW (Query 'RW alg a) alg a where
  basicExecute  sql param = do
    Connection _ conn <- askConnectionRW
    Query $ liftIO $ SQL.execute conn sql param
  basicExecute_ sql = do
    Connection _ conn <- askConnectionRW
    Query $ liftIO $ SQL.execute_ conn sql
  rollback = Query $ throwM Rollback


-- | Run read-only query. Note that read-only couldn't be rolled back
--  so they always succeed
runQueryRO
  :: (MonadIO m)
  => Connection rw alg a
  -> Query 'RO alg a x -> m x
runQueryRO c (Query q) = liftIO $ do
  r <- runQueryWorker False c $ runReaderT q (connectionRO c)
  case r of
    Nothing -> error "Query 'RO couldn't be rolled back"
    Just a  -> return a

-- | Run read-write query
runQueryRW
  :: (MonadIO m)
  => Connection 'RW alg a
  -> Query 'RW alg a x
  -> m (Maybe x)
runQueryRW c (Query q) = liftIO $ runQueryWorker True c $ runReaderT q c

queryRO
  :: (MonadReadDB m alg a, MonadIO m)
  => Query 'RO alg a x
  -> m x
queryRO q = flip runQueryRO q =<< askConnectionRO

queryRW
  :: (MonadDB m alg a, MonadIO m)
  => Query 'RW alg a x
  -> m (Maybe x)
queryRW q = flip runQueryRW q =<< askConnectionRW


----------------------------------------------------------------
-- Implementation
----------------------------------------------------------------

runQueryWorker
  :: (MonadIO m, MonadMask m)
  => Bool
  -> Connection rw alg a
  -> m x -> m (Maybe x)
runQueryWorker isWrite (Connection mutex conn) sql = withMutex mutex $ uninterruptibleMask $ \restore -> do
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
  liftIO $ SQL.execute_ conn $
    if isWrite then "BEGIN TRANSACTION IMMEDIATE" else "BEGIN TRANSACTION"
  r <- try $ restore sql `onError` rollbackTx
  case r of Left  Rollback -> return Nothing
            Right x        -> do commitTx
                                 return $ Just x
  where
    rollbackTx = liftIO $ SQL.execute_ conn "ROLLBACK"
    commitTx   = liftIO $ SQL.execute_ conn "COMMIT"
