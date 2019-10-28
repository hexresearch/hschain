{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DefaultSignatures          #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}
-- |
-- Building blocks wrapping SQL queries into accessors functions. It's
-- not internal per se but is quite low level and API doesn't provide
-- many guarantees. Use with care.
module HSChain.Store.Internal.Query (
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
  , basicQuery
  , basicQuery1
  , basicQuery_
  , basicExecute
  , basicExecute_
  , basicCacheGenesis
  , basicCacheBlock
  , basicPutCacheBlock
  , rollback
  , MonadQueryRW(..)
    -- * Database queries
  , Access(..)
    -- ** Monad transformer
  , QueryT(..)
  , runQueryROT
  , runQueryRWT
  , queryROT
  , queryRWT
  , mustQueryRWT
    -- ** Plain queries
  , Query(..)
  , runQueryRO
  , runQueryRW
  , queryRO
  , queryRW
  , mustQueryRW
    -- * sqlite-simple helpers
  , CBORed(..)
  ) where

import Codec.Serialise                (Serialise, deserialise, serialise)
import Control.Monad
import Control.Monad.Catch
import qualified Control.Monad.Fail as Fail
import Control.Monad.IO.Class
import Control.Monad.Morph            (MFunctor(..))
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.State.Strict as SS(StateT(..))
import Control.Monad.Trans.State.Lazy   as SL(StateT(..))
import Control.Monad.Trans.Except            (ExceptT(..))
import Control.Monad.Trans.Identity          (IdentityT(..))
import Data.Coerce
import Data.IORef
import qualified Data.Cache.LRU                   as LRU
import qualified Database.SQLite.Simple           as SQL
import qualified Database.SQLite.Simple.ToField   as SQL
import qualified Database.SQLite.Simple.FromField as SQL
import Pipes (Proxy)

import HSChain.Types.Blockchain
import HSChain.Control
import HSChain.Exceptions
import HSChain.Logger.Class


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
--   inference for functions like
--   'HSChain.Store.retrieveBlock' where @alg@ &
--   @a@ appear only in return type and frequently couldn't be
--   inferred without explicit type annotations.
data Connection (rw :: Access) alg a = Connection
  { connMutex    :: !Mutex
  , connConn     :: !SQL.Connection
  , connCacheGen :: !(IORef (Maybe (Block alg a)))
  , connCacheBlk :: !(IORef (LRU.LRU Height (Block alg a)))
  }

-- | Convert read-only or read-write connection to read-only one.
connectionRO :: Connection rw alg a -> Connection 'RO alg a
connectionRO = coerce

-- | Open connection to database and set necessary pragmas
openConnection :: MonadIO m => FilePath -> m (Connection rw alg a)
openConnection db = liftIO $ do
  connMutex    <- newMutex
  connConn     <- SQL.open db
  connCacheGen <- newIORef Nothing
  connCacheBlk <- newIORef $ LRU.newLRU (Just 8)
  -- SQLite have support for retrying transactions in case database is
  -- busy. Here we switch ot on
  SQL.execute_ connConn "PRAGMA busy_timeout = 10"
  return $! Connection{..}

-- | Close connection to database. Note that we can only close
--   connection if we have read-write acces to it.
closeConnection :: MonadIO m => Connection 'RW alg a -> m ()
closeConnection = closeConnection'

closeConnection' :: MonadIO m => Connection rw alg a -> m ()
closeConnection' = liftIO . SQL.close . connConn

withConnection
  :: (MonadMask m, MonadIO m)
  => FilePath -> (Connection rw alg a -> m x) -> m x
withConnection db = bracket (openConnection db) closeConnection'


----------------------------------------------------------------
-- Monadic API
----------------------------------------------------------------

-- | Monad which provides access to read-only connection to
--   database. That is it provides access to read-only handle to
--   database. This type class is expected to be more common than
--   read\/write one since most of the writing is expected to be done
--   by library.
--
--   Default implementation uses lift.
class Monad m => MonadReadDB m alg a | m -> alg a where
  askConnectionRO :: m (Connection 'RO alg a)
  --
  default askConnectionRO :: (m ~ t f, MonadReadDB f alg a, MonadTrans t)
                          => m (Connection 'RO alg a)
  askConnectionRO = lift askConnectionRO

-- | Monad which provides access to read-write connection to
--   database. It's half-internal API since user code shouldn't write
--   to the database and writing should be handled by library.
--
--   Default implementation uses lift.
class MonadReadDB m alg a => MonadDB m alg a | m -> alg a where
  askConnectionRW :: m (Connection 'RW alg a)
  --
  default askConnectionRW :: (m ~ t f, MonadDB f alg a, MonadTrans t)
                          => m (Connection 'RW alg a)
  askConnectionRW = lift askConnectionRW

-- | Monad which allows to perform read-only queries. API is just thin
--   wrappers about @sqlite-simple@ functions thus they shouldn't be
--   used directly. Also note that it doesn't have 'MonadIO' superclass.
class Monad m => MonadQueryRO m alg a | m -> alg a where
  liftQueryRO :: Query 'RO alg a x -> m x
  --
  default liftQueryRO :: (m ~ t f, MonadQueryRO f alg a, MonadTrans t)
                      => Query 'RO alg a x -> m x
  liftQueryRO = lift . liftQueryRO

-- | Monad which can perform read-write queries. @basic*RW@ functions
--   are same as @RO@ variants and intended to be used as
class (MonadQueryRO m alg a) => MonadQueryRW m alg a | m -> alg a where
  liftQueryRW :: Query rw alg a x -> m x
  --
  default liftQueryRW :: (m ~ t f, MonadQueryRW f alg a, MonadTrans t)
                      => Query rw alg a x -> m x
  liftQueryRW = lift . liftQueryRW

basicQuery1 :: (SQL.ToRow row, SQL.FromRow x, MonadQueryRO m alg a)
  => SQL.Query             -- ^ SQL query
  -> row                   -- ^ Query parameters
  -> m (Maybe x)
basicQuery1 sql param =
  basicQuery sql param >>= \case
    []  -> return Nothing
    [x] -> return $ Just x
    _   -> error "Impossible"

basicQuery :: (MonadQueryRO m alg a, SQL.ToRow p, SQL.FromRow q) => SQL.Query -> p -> m [q]
basicQuery sql p = liftQueryRO $ Query $ do
  conn <- asks connConn
  liftIO $ SQL.query conn sql p

basicQuery_ :: forall m alg a q. (MonadQueryRO m alg a, SQL.FromRow q) => SQL.Query -> m [q]
basicQuery_ sql = liftQueryRO $ Query $ do
  conn <- asks connConn
  liftIO $ SQL.query_ conn sql


basicExecute :: (MonadQueryRW m alg a, SQL.ToRow p) => SQL.Query -> p -> m ()
basicExecute sql param = liftQueryRW $ Query $ do
  conn <- asks connConn
  liftIO $ SQL.execute conn sql param

basicExecute_ :: (MonadQueryRW m alg a) => SQL.Query -> m ()
basicExecute_ sql = liftQueryRW $ Query $ do
  conn <- asks connConn
  liftIO $ SQL.execute_ conn sql

-- | Roll back execution of transaction. It's executed as throwing
--   of exception which is caught in the 'runQueryT'. Thus other
--   effects in monadic stack may or may not be rolled back as well.
rollback :: (MonadQueryRW m alg a) => m x
rollback = liftQueryRW $ Query $ throwM Rollback


basicCacheGenesis
  :: Query 'RO alg a (Maybe (Block alg a))
  -> Query 'RO alg a (Maybe (Block alg a))
basicCacheGenesis (Query query) = Query $ do
  ref <- asks connCacheGen
  liftIO (readIORef ref) >>= \case
    Just b  -> return (Just b)
    Nothing -> do b <- query
                  liftIO $ b <$ atomicWriteIORef ref b

basicCacheBlock
  :: (Height -> Query 'RO alg a (Maybe (Block alg a)))
  ->  Height -> Query 'RO alg a (Maybe (Block alg a))
basicCacheBlock query h = Query $ do
  ref <- asks connCacheBlk
  r   <- liftIO (atomicModifyIORef ref (LRU.lookup h))
  case r of
    Just _  -> return r
    Nothing -> do mb <- unQuery $ query h
                  liftIO $ forM_ mb $ \b -> atomicModifyIORef ref $ (,()) . LRU.insert h b
                  return mb

basicPutCacheBlock :: Block alg a -> Query 'RW alg a ()
basicPutCacheBlock b = Query $ do
  let h = blockHeight b
  ref <- asks connCacheBlk
  liftIO $ atomicModifyIORef ref $ (,()) . LRU.insert h b


instance MonadReadDB  m alg a => MonadReadDB  (IdentityT m) alg a
instance MonadDB      m alg a => MonadDB      (IdentityT m) alg a
instance MonadQueryRO m alg a => MonadQueryRO (IdentityT m) alg a
instance MonadQueryRW m alg a => MonadQueryRW (IdentityT m) alg a

instance MonadReadDB  m alg a => MonadReadDB  (MaybeT m) alg a
instance MonadDB      m alg a => MonadDB      (MaybeT m) alg a
instance MonadQueryRO m alg a => MonadQueryRO (MaybeT m) alg a
instance MonadQueryRW m alg a => MonadQueryRW (MaybeT m) alg a

instance MonadReadDB  m alg a => MonadReadDB  (ExceptT e m) alg a
instance MonadDB      m alg a => MonadDB      (ExceptT e m) alg a
instance MonadQueryRO m alg a => MonadQueryRO (ExceptT e m) alg a
instance MonadQueryRW m alg a => MonadQueryRW (ExceptT e m) alg a

instance MonadReadDB  m alg a => MonadReadDB  (ReaderT r m) alg a
instance MonadDB      m alg a => MonadDB      (ReaderT r m) alg a
instance MonadQueryRO m alg a => MonadQueryRO (ReaderT r m) alg a
instance MonadQueryRW m alg a => MonadQueryRW (ReaderT r m) alg a

instance MonadReadDB  m alg a => MonadReadDB  (SS.StateT s m) alg a
instance MonadDB      m alg a => MonadDB      (SS.StateT s m) alg a
instance MonadQueryRO m alg a => MonadQueryRO (SS.StateT s m) alg a
instance MonadQueryRW m alg a => MonadQueryRW (SS.StateT s m) alg a

instance MonadReadDB  m alg a => MonadReadDB  (SL.StateT s m) alg a
instance MonadDB      m alg a => MonadDB      (SL.StateT s m) alg a
instance MonadQueryRO m alg a => MonadQueryRO (SL.StateT s m) alg a
instance MonadQueryRW m alg a => MonadQueryRW (SL.StateT s m) alg a

instance MonadReadDB m alg a => MonadReadDB (Proxy x x' y y' m) alg a
instance MonadDB     m alg a => MonadDB     (Proxy x x' y y' m) alg a

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
  deriving newtype ( Functor, Applicative, MonadIO, MonadThrow, MonadCatch, MonadMask, MonadLogger)

instance MFunctor (QueryT rw alg a) where
  hoist f (QueryT m) = QueryT (f m)

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

instance (MonadIO m, MonadThrow m, MonadReadDB m alg a) => MonadQueryRO (QueryT rw alg a m) alg a where
  liftQueryRO (Query action) = QueryT $ do
    -- NOTE: coercion is needed since we need to implement for both
    --       QueryT 'RO/'RW
    liftIO . runReaderT action . coerce =<< askConnectionRO

instance (MonadIO m, MonadThrow m, MonadDB m alg a) => MonadQueryRW (QueryT 'RW alg a m) alg a where
  liftQueryRW (Query action) = QueryT $ do
    -- NOTE: coercion is needed since we need to implement for both
    --       Query 'RO/'RW
    liftIO . runReaderT action . coerce =<< askConnectionRW

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

-- | Same as 'queryRO' but for 'QueryT'
queryROT
  :: (MonadReadDB m alg a, MonadIO m, MonadMask m)
  => QueryT 'RO alg a m x
  -> m x
queryROT q = flip runQueryROT q =<< askConnectionRO

-- | Same as 'queryRW' but for 'QueryT'
queryRWT
  :: (MonadDB m alg a, MonadIO m, MonadMask m)
  => QueryT 'RW alg a m x
  -> m (Maybe x)
queryRWT q = flip runQueryRWT q =<< askConnectionRW

-- | Same as 'mustQueryRW' but for 'QueryT'
mustQueryRWT
  :: (MonadDB m alg a, MonadIO m, MonadMask m)
  => QueryT 'RW alg a m x
  -> m x
mustQueryRWT q = throwNothing UnexpectedRollback =<< flip runQueryRWT q =<< askConnectionRW


----------------------------------------------------------------
-- Query monad
----------------------------------------------------------------

-- | Query which doesn't allow any other effect except interaction
--   with database.
newtype Query rw alg a x = Query { unQuery :: ReaderT (Connection rw alg a) IO x }
  deriving newtype (Functor, Applicative, MonadThrow)

instance Monad (Query rm alg a) where
  return = Query . return
  Query m >>= f = Query $ (\(Query q) -> q) . f =<< m
  fail _ = Query $ throwM Rollback

instance Fail.MonadFail (Query rw alg a) where
  fail _ = Query $ throwM Rollback

instance MonadQueryRO (Query rw alg a) alg a where
  -- NOTE: We need coerce to implement both:
  --        - Query 'RO -> Query 'RO
  --        - Query 'RO -> Query 'RW
  liftQueryRO = coerce

instance MonadQueryRW (Query 'RW alg a) alg a where
  -- NOTE: We need coerce to implement both:
  --        - Query 'RO -> Query 'RW
  --        - Query 'RW -> Query 'RW
  liftQueryRW = coerce

-- | Run read-only query. Note that read-only couldn't be rolled back
--   so they always succeed
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

-- | Run read-only query using Query monad. Note that read-only
--   couldn't be rolled back so they always succeed.
queryRO
  :: (MonadReadDB m alg a, MonadIO m)
  => Query 'RO alg a x
  -> m x
queryRO q = flip runQueryRO q =<< askConnectionRO

-- | Run read-write query using Query monad. Return @Nothing@ is query
--   was rolled back
queryRW
  :: (MonadDB m alg a, MonadIO m)
  => Query 'RW alg a x
  -> m (Maybe x)
queryRW q = flip runQueryRW q =<< askConnectionRW

-- | Run read-write query using Query monad. Throws
--   'UnexpectedRollback' exception if query is rolled back was rolled
--   back.
mustQueryRW
  :: (MonadDB m alg a, MonadThrow m, MonadIO m)
  => Query 'RW alg a x
  -> m x
mustQueryRW q
  = throwNothing UnexpectedRollback =<< flip runQueryRW q =<< askConnectionRW


----------------------------------------------------------------
-- Implementation
----------------------------------------------------------------

runQueryWorker
  :: (MonadIO m, MonadMask m)
  => Bool
  -> Connection rw alg a
  -> m x -> m (Maybe x)
runQueryWorker isWrite connection sql = withMutex mutex $ uninterruptibleMask $ \restore -> do
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
    mutex      = connMutex connection
    conn       = connConn  connection


-- | Newtype wrapper which provides CBOR-encoded To\/FromField
--   instance for values
newtype CBORed a = CBORed { unCBORed :: a }
  deriving Show

instance (Serialise a) => SQL.FromField (CBORed a) where
  fromField f = CBORed . deserialise <$> SQL.fromField f

instance (Serialise a) => SQL.ToField (CBORed a) where
  toField (CBORed a) = SQL.toField (serialise a)
