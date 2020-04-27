{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DefaultSignatures          #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}
-- |
-- Building blocks wrapping SQL queries into accessors functions. It's
-- not internal per se but is quite low level and API doesn't provide
-- many guarantees. Use with care.
module HSChain.PoW.Store.Internal.Query (
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
  , basicLastInsertRowId
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
import Data.Int
import qualified Data.Cache.LRU                   as LRU
import qualified Database.SQLite.Simple           as SQL
import qualified Database.SQLite.Simple.ToField   as SQL
import qualified Database.SQLite.Simple.FromField as SQL
import Pipes (Proxy)

import HSChain.PoW.Types
import HSChain.Control.Mutex
import HSChain.Control.Util
import HSChain.PoW.Exceptions
import HSChain.PoW.Logger.Class


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
data Connection (rw :: Access) a = Connection
  { connMutex    :: !Mutex
  , connConn     :: !SQL.Connection
  , connClosed   :: !(IORef Bool)
  , connCacheGen :: !(IORef (Maybe (Block a)))
  , connCacheBlk :: !(IORef (LRU.LRU Height (Block a)))
  }

-- | Convert read-only or read-write connection to read-only one.
connectionRO :: Connection rw a -> Connection 'RO a
connectionRO = coerce

-- | Open connection to database and set necessary pragmas
openConnection :: MonadIO m => FilePath -> m (Connection rw a)
openConnection db = liftIO $ do
  connMutex    <- newMutex
  connConn     <- SQL.open db
  connClosed   <- newIORef False
  connCacheGen <- newIORef Nothing
  connCacheBlk <- newIORef $ LRU.newLRU (Just 8)
  -- SQLite have support for retrying transactions in case database is
  -- busy. Here we switch ot on
  SQL.execute_ connConn "PRAGMA busy_timeout = 10"
  return $! Connection{..}

-- | Close connection to database. Note that we can only close
--   connection if we have read-write acces to it.
closeConnection :: MonadIO m => Connection 'RW a -> m ()
closeConnection = closeConnection'

closeConnection' :: MonadIO m => Connection rw a -> m ()
closeConnection' Connection{..} = liftIO $ do
  uninterruptibleMask_ $ do
    withMutex connMutex $ do
      writeIORef connClosed True
      SQL.close connConn

withConnection
  :: (MonadMask m, MonadIO m)
  => FilePath -> (Connection rw a -> m x) -> m x
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
class Monad m => MonadReadDB m a | m -> a where
  askConnectionRO :: m (Connection 'RO a)
  --
  default askConnectionRO :: (m ~ t f, MonadReadDB f a, MonadTrans t)
                          => m (Connection 'RO a)
  askConnectionRO = lift askConnectionRO

-- | Monad which provides access to read-write connection to
--   database. It's half-internal API since user code shouldn't write
--   to the database and writing should be handled by library.
--
--   Default implementation uses lift.
class MonadReadDB m a => MonadDB m a | m -> a where
  askConnectionRW :: m (Connection 'RW a)
  --
  default askConnectionRW :: (m ~ t f, MonadDB f a, MonadTrans t)
                          => m (Connection 'RW a)
  askConnectionRW = lift askConnectionRW

-- | Monad which allows to perform read-only queries. API is just thin
--   wrappers about @sqlite-simple@ functions thus they shouldn't be
--   used directly. Also note that it doesn't have 'MonadIO' superclass.
class Monad m => MonadQueryRO m a | m -> a where
  liftQueryRO :: Query 'RO a x -> m x
  --
  default liftQueryRO :: (m ~ t f, MonadQueryRO f a, MonadTrans t)
                      => Query 'RO a x -> m x
  liftQueryRO = lift . liftQueryRO

-- | Monad which can perform read-write queries. @basic*RW@ functions
--   are same as @RO@ variants and intended to be used as
class (MonadQueryRO m a) => MonadQueryRW m a | m -> a where
  liftQueryRW :: Query rw a x -> m x
  --
  default liftQueryRW :: (m ~ t f, MonadQueryRW f a, MonadTrans t)
                      => Query rw a x -> m x
  liftQueryRW = lift . liftQueryRW

basicQuery1 :: (SQL.ToRow row, SQL.FromRow x, MonadQueryRO m a)
  => SQL.Query             -- ^ SQL query
  -> row                   -- ^ Query parameters
  -> m (Maybe x)
basicQuery1 sql param =
  basicQuery sql param >>= \case
    []  -> return Nothing
    [x] -> return $ Just x
    _   -> error "Impossible"

basicLastInsertRowId :: Query 'RW a Int64
basicLastInsertRowId = Query $ do
  conn <- asks connConn
  liftIO $ SQL.lastInsertRowId conn

basicQuery :: (MonadQueryRO m a, SQL.ToRow p, SQL.FromRow q) => SQL.Query -> p -> m [q]
basicQuery sql p = liftQueryRO $ Query $ do
  conn <- asks connConn
  liftIO $ SQL.query conn sql p

basicQuery_ :: forall m a q. (MonadQueryRO m a, SQL.FromRow q) => SQL.Query -> m [q]
basicQuery_ sql = liftQueryRO $ Query $ do
  conn <- asks connConn
  liftIO $ SQL.query_ conn sql


basicExecute :: (MonadQueryRW m a, SQL.ToRow p) => SQL.Query -> p -> m ()
basicExecute sql param = liftQueryRW $ Query $ do
  conn <- asks connConn
  liftIO $ SQL.execute conn sql param

basicExecute_ :: (MonadQueryRW m a) => SQL.Query -> m ()
basicExecute_ sql = liftQueryRW $ Query $ do
  conn <- asks connConn
  liftIO $ SQL.execute_ conn sql

-- | Roll back execution of transaction. It's executed as throwing
--   of exception which is caught in the 'runQueryT'. Thus other
--   effects in monadic stack may or may not be rolled back as well.
rollback :: (MonadQueryRW m a) => m x
rollback = liftQueryRW $ Query $ throwM Rollback


basicCacheGenesis
  :: Query 'RO a (Maybe (Block a))
  -> Query 'RO a (Maybe (Block a))
basicCacheGenesis (Query query) = Query $ do
  ref <- asks connCacheGen
  liftIO (readIORef ref) >>= \case
    Just b  -> return (Just b)
    Nothing -> do !b <- query
                  liftIO $ b <$ atomicWriteIORef ref b

basicCacheBlock
  :: (Height -> Query 'RO a (Maybe (Block a)))
  ->  Height -> Query 'RO a (Maybe (Block a))
basicCacheBlock query h = Query $ do
  ref <- asks connCacheBlk
  r   <- liftIO (atomicModifyIORef' ref (LRU.lookup h))
  case r of
    Just _  -> return r
    Nothing -> do mb <- unQuery $ query h
                  liftIO $ forM_ mb $ \b -> atomicModifyIORef' ref $ (,()) . LRU.insert h b
                  return mb

basicPutCacheBlock :: Block a -> Query 'RW a ()
basicPutCacheBlock b = Query $ do
  let h = blockHeight b
  ref <- asks connCacheBlk
  liftIO $ atomicModifyIORef' ref $ (,()) . LRU.insert h b


instance MonadReadDB  m a => MonadReadDB  (IdentityT m) a
instance MonadDB      m a => MonadDB      (IdentityT m) a
instance MonadQueryRO m a => MonadQueryRO (IdentityT m) a
instance MonadQueryRW m a => MonadQueryRW (IdentityT m) a

instance MonadReadDB  m a => MonadReadDB  (MaybeT m) a
instance MonadDB      m a => MonadDB      (MaybeT m) a
instance MonadQueryRO m a => MonadQueryRO (MaybeT m) a
instance MonadQueryRW m a => MonadQueryRW (MaybeT m) a

instance MonadReadDB  m a => MonadReadDB  (ExceptT e m) a
instance MonadDB      m a => MonadDB      (ExceptT e m) a
instance MonadQueryRO m a => MonadQueryRO (ExceptT e m) a
instance MonadQueryRW m a => MonadQueryRW (ExceptT e m) a

instance MonadReadDB  m a => MonadReadDB  (ReaderT r m) a
instance MonadDB      m a => MonadDB      (ReaderT r m) a
instance MonadQueryRO m a => MonadQueryRO (ReaderT r m) a
instance MonadQueryRW m a => MonadQueryRW (ReaderT r m) a

instance MonadReadDB  m a => MonadReadDB  (SS.StateT s m) a
instance MonadDB      m a => MonadDB      (SS.StateT s m) a
instance MonadQueryRO m a => MonadQueryRO (SS.StateT s m) a
instance MonadQueryRW m a => MonadQueryRW (SS.StateT s m) a

instance MonadReadDB  m a => MonadReadDB  (SL.StateT s m) a
instance MonadDB      m a => MonadDB      (SL.StateT s m) a
instance MonadQueryRO m a => MonadQueryRO (SL.StateT s m) a
instance MonadQueryRW m a => MonadQueryRW (SL.StateT s m) a

instance MonadReadDB m a => MonadReadDB (Proxy x x' y y' m) a
instance MonadDB     m a => MonadDB     (Proxy x x' y y' m) a

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
newtype QueryT (rw :: Access) a m x = QueryT { unQueryT :: m x }
  deriving newtype ( Functor, Applicative, MonadIO, MonadThrow, MonadCatch, MonadMask, MonadLogger)

instance MFunctor (QueryT rw a) where
  hoist f (QueryT m) = QueryT (f m)

instance MonadThrow m => Monad (QueryT rw a m) where
  return         = QueryT . return
  QueryT m >>= f = QueryT $ unQueryT . f =<< m
#if !MIN_VERSION_base(4,11,0)
  fail _         = throwM Rollback
#endif

instance MonadThrow m => Fail.MonadFail (QueryT rw a m) where
  fail _ = throwM Rollback

-- | Exception thrown in order to roll back transaction
data Rollback = Rollback
  deriving (Show,Eq)
instance Exception Rollback


instance MonadTrans (QueryT rr a) where
  lift = QueryT

instance (MonadIO m, MonadThrow m, MonadReadDB m a) => MonadQueryRO (QueryT rw a m) a where
  liftQueryRO (Query action) = QueryT $ do
    -- NOTE: coercion is needed since we need to implement for both
    --       QueryT 'RO/'RW
    liftIO . runReaderT action . coerce =<< askConnectionRO

instance (MonadIO m, MonadThrow m, MonadDB m a) => MonadQueryRW (QueryT 'RW a m) a where
  liftQueryRW (Query action) = QueryT $ do
    -- NOTE: coercion is needed since we need to implement for both
    --       Query 'RO/'RW
    liftIO . runReaderT action . coerce =<< askConnectionRW

-- | Run read-only query. Note that read-only couldn't be rolled back
--  so they always succeed
runQueryROT
  :: (MonadIO m, MonadMask m)
  => Connection rw a
  -> QueryT 'RO a m x -> m x
runQueryROT c q = do
  r <- runQueryWorker False c . unQueryT $ q
  case r of
    Nothing -> error "Query 'RO couldn't be rolled back"
    Just a  -> return a

-- | Run read-write query
runQueryRWT
  :: (MonadIO m, MonadMask m)
  => Connection 'RW a
  -> QueryT 'RW a m x
  -> m (Maybe x)
runQueryRWT c = runQueryWorker True c . unQueryT

-- | Same as 'queryRO' but for 'QueryT'
queryROT
  :: (MonadReadDB m a, MonadIO m, MonadMask m)
  => QueryT 'RO a m x
  -> m x
queryROT q = flip runQueryROT q =<< askConnectionRO

-- | Same as 'queryRW' but for 'QueryT'
queryRWT
  :: (MonadDB m a, MonadIO m, MonadMask m)
  => QueryT 'RW a m x
  -> m (Maybe x)
queryRWT q = flip runQueryRWT q =<< askConnectionRW

-- | Same as 'mustQueryRW' but for 'QueryT'
mustQueryRWT
  :: (MonadDB m a, MonadIO m, MonadMask m)
  => QueryT 'RW a m x
  -> m x
mustQueryRWT q = throwNothing UnexpectedRollback =<< flip runQueryRWT q =<< askConnectionRW


----------------------------------------------------------------
-- Query monad
----------------------------------------------------------------

-- | Query which doesn't allow any other effect except interaction
--   with database.
newtype Query rw a x = Query { unQuery :: ReaderT (Connection rw a) IO x }
  deriving newtype (Functor, Applicative, MonadThrow)

instance Monad (Query rm a) where
  return = Query . return
  Query m >>= f = Query $ (\(Query q) -> q) . f =<< m
#if !MIN_VERSION_base(4,11,0)
  fail _ = Query $ throwM Rollback
#endif

instance Fail.MonadFail (Query rw a) where
  fail _ = Query $ throwM Rollback

instance MonadQueryRO (Query rw a) a where
  -- NOTE: We need coerce to implement both:
  --        - Query 'RO -> Query 'RO
  --        - Query 'RO -> Query 'RW
  liftQueryRO = coerce

instance MonadQueryRW (Query 'RW a) a where
  -- NOTE: We need coerce to implement both:
  --        - Query 'RO -> Query 'RW
  --        - Query 'RW -> Query 'RW
  liftQueryRW = coerce

-- | Run read-only query. Note that read-only couldn't be rolled back
--   so they always succeed
runQueryRO
  :: (MonadIO m)
  => Connection rw a
  -> Query 'RO a x -> m x
runQueryRO c (Query q) = liftIO $ do
  r <- runQueryWorker False c $ runReaderT q (connectionRO c)
  case r of
    Nothing -> error "Query 'RO couldn't be rolled back"
    Just a  -> return a

-- | Run read-write query
runQueryRW
  :: (MonadIO m)
  => Connection 'RW a
  -> Query 'RW a x
  -> m (Maybe x)
runQueryRW c (Query q) = liftIO $ runQueryWorker True c $ runReaderT q c

-- | Run read-only query using Query monad. Note that read-only
--   couldn't be rolled back so they always succeed.
queryRO
  :: (MonadReadDB m a, MonadIO m)
  => Query 'RO a x
  -> m x
queryRO q = flip runQueryRO q =<< askConnectionRO

-- | Run read-write query using Query monad. Return @Nothing@ is query
--   was rolled back
queryRW
  :: (MonadDB m a, MonadIO m)
  => Query 'RW a x
  -> m (Maybe x)
queryRW q = flip runQueryRW q =<< askConnectionRW

-- | Run read-write query using Query monad. Throws
--   'UnexpectedRollback' exception if query is rolled back was rolled
--   back.
mustQueryRW
  :: (MonadDB m a, MonadThrow m, MonadIO m)
  => Query 'RW a x
  -> m x
mustQueryRW q
  = throwNothing UnexpectedRollback =<< flip runQueryRW q =<< askConnectionRW


----------------------------------------------------------------
-- Implementation
----------------------------------------------------------------

runQueryWorker
  :: (MonadIO m, MonadMask m)
  => Bool
  -> Connection rw a
  -> m x -> m (Maybe x)
runQueryWorker isWrite connection sql = withMutex mutex $ do
  closed <- liftIO $ readIORef $ connClosed connection
  when closed $ throwM DatabaseIsClosed
  uninterruptibleMask $ \restore -> do
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
