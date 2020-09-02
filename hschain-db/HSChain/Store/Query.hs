{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DefaultSignatures          #-}
{-# LANGUAGE DeriveAnyClass             #-}
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
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}
-- |
-- Wrappers for sqlite-simple that provide thread safety, ability to
-- implement read\/write access, caching. It's intended to be used as
-- foundation for implementation of higher level function for working
-- with database
module HSChain.Store.Query (
    -- * Connection
    Access(..)
  , Connection(..)
  , connectionRO
  , openConnection
  , closeConnection
  , withConnection
    -- * Monadic API
    -- ** Monad with access to connection
  , MonadReadDB(..)
  , MonadDB(..)
    -- * MonadQuery
  , MonadQueryRO(..)
  , MonadQueryRW(..)
    -- * Query monad
  , Query(..)
  , runQueryRO
  , runQueryRW
  , queryRO
  , queryRW
  , mustQueryRW
    -- ** Primitive operations
  , basicLastInsertRowId
  , basicQuery
  , basicQuery1
  , basicQueryWith
  , basicQueryWith1
  , basicQuery_
  , basicQueryWith_
  , basicExecute
  , basicExecute_
  , rollback
    -- ** Prepared statements
  , PreparedStmt
  , PreparedQuery
  , prepareStatement
  , prepareQuery
  , preparedQuery
  , preparedQuery1
  , preparedExecute
    -- * Query transformer
  , QueryT(..)
  , runQueryROT
  , runQueryRWT
  , queryROT
  , queryRWT
  , mustQueryRWT
    -- * Exceptions
  , Rollback(..)
  , UnexpectedRollback(..)
    -- * Newtype wrappers for DerivingVia
  , CBORed(..)
  , ByteRepred(..)
  , ID(..)
    -- ** Field & row parsers
  , SQL.field
  , fieldCBOR
  , nullableFieldCBOR
  , fieldByteRepr
  , nullableFieldByteRepr
    -- ** Deriving via for MonadDB
  , DatabaseByField(..)
  , DatabaseByType(..)
  , DatabaseByReader(..)
    -- * Reexports
  , SQL.Only(..)
  ) where

import Codec.Serialise                (Serialise, deserialise, serialise)
import Control.Concurrent.MVar
import Control.Lens
import Control.Monad.Catch
import qualified Control.Monad.Fail as Fail
import Control.Monad.IO.Class
import Control.Monad.Morph            (MFunctor(..))
import Control.Monad.Trans.Class
import Control.Monad.Reader
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.State.Strict as SS(StateT(..))
import Control.Monad.Trans.State.Lazy   as SL(StateT(..))
import Control.Monad.Trans.Except            (ExceptT(..))
import Control.Monad.Trans.Identity          (IdentityT(..))
import Data.Coerce
import Data.Int
import Data.IORef
import Data.Generics.Product.Fields (HasField'(..))
import Data.Generics.Product.Typed  (HasType(..))
import qualified Database.SQLite.Simple           as SQL
import qualified Database.SQLite.Simple.ToField   as SQL
import qualified Database.SQLite.Simple.FromField as SQL
import qualified Database.SQLite.Simple.FromRow   as SQL
import Pipes (Proxy)

import HSChain.Crypto.Classes (ByteRepr(..))
import HSChain.Control.Mutex
import HSChain.Control.Util


----------------------------------------------------------------
-- Connection to SQL database
----------------------------------------------------------------

-- | Access rights for storage or whether database queries alter
--   underlying data
data Access = RO                -- ^ Read-only access
            | RW                -- ^ Read-write access
            deriving (Show)

-- | Connection to sqlite database. It's tagged by level of access to
--   database (read only or read\/write) and parametrized by cache
data Connection (rw :: Access) = Connection
  { connMutex    :: !(MVar Bool)
  , connConn     :: !SQL.Connection
  , connPrepared :: !(IORef [SQL.Statement])
  }

-- | Convert read-only or read-write connection to read-only one.
connectionRO :: Connection rw -> Connection 'RO
connectionRO = coerce

-- | Open connection to database and set necessary pragmas
openConnection :: (MonadIO m) => FilePath -> m (Connection rw)
openConnection db = liftIO $
  bracketOnError (SQL.open db) (SQL.close) $ \connConn -> do
    connMutex    <- newMVar True
    connPrepared <- newIORef []
    -- SQLite have support for retrying transactions in case database is
    -- busy. Here we switch it on
    SQL.execute_ connConn "PRAGMA busy_timeout = 10"
    return $! Connection{..}

-- | Close connection to database. Note that we can only close
--   connection if we have read-write acces to it.
closeConnection :: MonadIO m => Connection 'RW -> m ()
closeConnection = closeConnection'

-- | Wrapper for @bracket openConnection closeConnection@ which allows
--   to open read-only connections as well.
withConnection
  :: (MonadMask m, MonadIO m)
  => FilePath -> (Connection rw -> m x) -> m x
withConnection db = bracket (openConnection db) closeConnection'

closeConnection' :: MonadIO m => Connection rw -> m ()
closeConnection' Connection{..} = liftIO $ uninterruptibleMask_ $ do
  modifyMVarM_ connMutex $ \case
    False -> return False
    True  -> do
      mapM_ SQL.closeStatement =<< readIORef connPrepared
      SQL.close connConn
      return False


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
class Monad m => MonadReadDB m where
  askConnectionRO :: m (Connection 'RO)
  --
  default askConnectionRO :: (m ~ t f, MonadReadDB f, MonadTrans t)
                          => m (Connection 'RO)
  askConnectionRO = lift askConnectionRO

-- | Monad which provides access to read-write connection to
--   database. It's half-internal API since user code shouldn't write
--   to the database and writing should be handled by library.
--
--   Default implementation uses lift.
class MonadReadDB m => MonadDB m where
  askConnectionRW :: m (Connection 'RW)
  --
  default askConnectionRW :: (m ~ t f, MonadDB f, MonadTrans t)
                          => m (Connection 'RW)
  askConnectionRW = lift askConnectionRW


-- | Monad which allows to perform read-only queries. API is just thin
--   wrappers about @sqlite-simple@ functions thus they shouldn't be
--   used directly. Also note that it doesn't have 'MonadIO' superclass.
class Monad m => MonadQueryRO m where
  liftQueryRO :: Query 'RO x -> m x
  --
  default liftQueryRO :: (m ~ t f, MonadQueryRO f, MonadTrans t)
                      => Query 'RO x -> m x
  liftQueryRO = lift . liftQueryRO

-- | Monad which can perform read-write queries. @basic*RW@ functions
--   are same as @RO@ variants and intended to be used as
class (MonadQueryRO m) => MonadQueryRW m where
  liftQueryRW :: Query rw x -> m x
  --
  default liftQueryRW :: (m ~ t f, MonadQueryRW f, MonadTrans t)
                      => Query rw x -> m x
  liftQueryRW = lift . liftQueryRW


instance MonadReadDB  m => MonadReadDB  (IdentityT m)
instance MonadDB      m => MonadDB      (IdentityT m)
instance MonadQueryRO m => MonadQueryRO (IdentityT m)
instance MonadQueryRW m => MonadQueryRW (IdentityT m)

instance MonadReadDB  m => MonadReadDB  (MaybeT m)
instance MonadDB      m => MonadDB      (MaybeT m)
instance MonadQueryRO m => MonadQueryRO (MaybeT m)
instance MonadQueryRW m => MonadQueryRW (MaybeT m)

instance MonadReadDB  m => MonadReadDB  (ExceptT e m)
instance MonadDB      m => MonadDB      (ExceptT e m)
instance MonadQueryRO m => MonadQueryRO (ExceptT e m)
instance MonadQueryRW m => MonadQueryRW (ExceptT e m)

instance MonadReadDB  m => MonadReadDB  (ReaderT r m)
instance MonadDB      m => MonadDB      (ReaderT r m)
instance MonadQueryRO m => MonadQueryRO (ReaderT r m)
instance MonadQueryRW m => MonadQueryRW (ReaderT r m)

instance MonadReadDB  m => MonadReadDB  (SS.StateT s m)
instance MonadDB      m => MonadDB      (SS.StateT s m)
instance MonadQueryRO m => MonadQueryRO (SS.StateT s m)
instance MonadQueryRW m => MonadQueryRW (SS.StateT s m)

instance MonadReadDB  m => MonadReadDB  (SL.StateT s m)
instance MonadDB      m => MonadDB      (SL.StateT s m)
instance MonadQueryRO m => MonadQueryRO (SL.StateT s m)
instance MonadQueryRW m => MonadQueryRW (SL.StateT s m)

instance MonadReadDB m => MonadReadDB (Proxy x x' y y' m)
instance MonadDB     m => MonadDB     (Proxy x x' y y' m)



----------------------------------------------------------------
-- Query monad
----------------------------------------------------------------

-- | Query which doesn't allow any other effect except interaction
--   with database.
newtype Query rw x = Query { unQuery :: ReaderT (Connection rw) IO x }
  deriving newtype (Functor, Applicative, MonadThrow)

instance Monad (Query rm) where
  return = Query . return
  Query m >>= f = Query $ (\(Query q) -> q) . f =<< m
#if !MIN_VERSION_base(4,11,0)
  fail _ = Query $ throwM Rollback
#endif

instance Fail.MonadFail (Query rw) where
  fail _ = Query $ throwM Rollback

instance MonadQueryRO (Query rw) where
  -- NOTE: We need coerce to implement both:
  --        - Query 'RO -> Query 'RO
  --        - Query 'RO -> Query 'RW
  liftQueryRO = coerce

instance rw ~ 'RW => MonadQueryRW (Query rw) where
  -- NOTE: We need coerce to implement both:
  --        - Query 'RO -> Query 'RW
  --        - Query 'RW -> Query 'RW
  liftQueryRW = coerce

-- | Run read-only query. Note that read-only couldn't be rolled back
--   so they always succeed
runQueryRO
  :: (MonadIO m)
  => Connection rw
  -> Query 'RO a
  -> m a
runQueryRO c (Query q) = liftIO $ do
  r <- runQueryWorker False c $ runReaderT q (connectionRO c)
  case r of
    Nothing -> liftIO $ throwM UnexpectedRollback
    Just a  -> return a

-- | Run read-write query
runQueryRW
  :: (MonadIO m)
  => Connection 'RW
  -> Query 'RW a
  -> m (Maybe a)
runQueryRW c (Query q) = liftIO $ runQueryWorker True c $ runReaderT q c

-- | Run read-only query using Query monad. Note that read-only
--   couldn't be rolled back so they always succeed.
queryRO
  :: (MonadReadDB m, MonadIO m)
  => Query 'RO a
  -> m a
queryRO q = flip runQueryRO q =<< askConnectionRO

-- | Run read-write query using Query monad. Return @Nothing@ is query
--   was rolled back
queryRW
  :: (MonadDB m, MonadIO m)
  => Query 'RW a
  -> m (Maybe a)
queryRW q = flip runQueryRW q =<< askConnectionRW

-- | Run read-write query using Query monad. Throws
--   'UnexpectedRollback' exception if query is rolled back was rolled
--   back.
mustQueryRW
  :: (MonadDB m, MonadThrow m, MonadIO m)
  => Query 'RW a
  -> m a
mustQueryRW q
  = throwNothing UnexpectedRollback =<< flip runQueryRW q =<< askConnectionRW


basicQuery :: (SQL.ToRow p, SQL.FromRow q, MonadQueryRO m) => SQL.Query -> p -> m [q]
basicQuery sql p = liftQueryRO $ Query $ do
  conn <- asks connConn
  liftIO $ SQL.query conn sql p

basicQuery_ :: (SQL.FromRow q, MonadQueryRO m) => SQL.Query -> m [q]
basicQuery_ sql = liftQueryRO $ Query $ do
  conn <- asks connConn
  liftIO $ SQL.query_ conn sql

basicQueryWith :: (SQL.ToRow p, MonadQueryRO m) => SQL.RowParser q -> SQL.Query -> p -> m [q]
basicQueryWith parser sql p = liftQueryRO $ Query $ do
  conn <- asks connConn
  liftIO $ SQL.queryWith parser conn sql p

basicQueryWith1 :: (SQL.ToRow p, MonadQueryRO m) => SQL.RowParser q -> SQL.Query -> p -> m (Maybe q)
basicQueryWith1 parser sql p =
  basicQueryWith parser sql p >>= \case
    []  -> return Nothing
    [x] -> return $ Just x
    _   -> error "Impossible"

basicQueryWith_ :: (MonadQueryRO m) => SQL.RowParser q -> SQL.Query -> m [q]
basicQueryWith_ parser sql = liftQueryRO $ Query $ do
  conn <- asks connConn
  liftIO $ SQL.queryWith_ parser conn sql

basicQuery1 :: (SQL.ToRow row, SQL.FromRow a, MonadQueryRO m)
  => SQL.Query             -- ^ SQL query
  -> row                   -- ^ Query parameters
  -> m (Maybe a)
basicQuery1 sql param =
  basicQuery sql param >>= \case
    []  -> return Nothing
    [x] -> return $ Just x
    _   -> error "Impossible"

basicLastInsertRowId :: MonadQueryRW m => m Int64
basicLastInsertRowId = liftQueryRW $ Query $ do
  conn <- asks connConn
  liftIO $ SQL.lastInsertRowId conn


basicExecute :: (SQL.ToRow p, MonadQueryRW m) => SQL.Query -> p -> m ()
basicExecute sql param = liftQueryRW $ Query $ do
  conn <- asks connConn
  liftIO $ SQL.execute conn sql param

basicExecute_ :: (MonadQueryRW m) => SQL.Query -> m ()
basicExecute_ sql = liftQueryRW $ Query $ do
  conn <- asks connConn
  liftIO $ SQL.execute_ conn sql

-- | Roll back execution of transaction. It's executed as throwing
--   of exception which is caught in the 'runQueryT'. Thus other
--   effects in monadic stack may or may not be rolled back as well.
rollback :: (MonadQueryRW m) => m a
rollback = liftQueryRW $ Query $ throwM Rollback


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
newtype QueryT (rw :: Access) m x = QueryT { unQueryT :: m x }
  deriving newtype ( Functor, Applicative, MonadIO, MonadThrow, MonadCatch, MonadMask)

instance MFunctor (QueryT rw) where
  hoist f (QueryT m) = QueryT (f m)

instance MonadThrow m => Monad (QueryT rw m) where
  return         = QueryT . return
  QueryT m >>= f = QueryT $ unQueryT . f =<< m
#if !MIN_VERSION_base(4,11,0)
  fail _         = throwM Rollback
#endif

instance MonadThrow m => Fail.MonadFail (QueryT rw m) where
  fail _ = throwM Rollback

-- | Exception thrown in order to roll back transaction
data Rollback = Rollback
  deriving (Show,Eq)
instance Exception Rollback

data UnexpectedRollback = UnexpectedRollback
  deriving (Show,Eq)
instance Exception UnexpectedRollback

-- | Trying to access closed database
data DatabaseIsClosed = DatabaseIsClosed
  deriving stock    (Show)
  deriving anyclass (Exception)


instance MonadTrans (QueryT rw) where
  lift = QueryT

instance (MonadIO m, MonadThrow m, MonadReadDB m) => MonadQueryRO (QueryT rw m) where
  liftQueryRO (Query action) = QueryT $ do
    -- NOTE: coercion is needed since we need to implement for both
    --       QueryT 'RO/'RW
    liftIO . runReaderT action . coerce =<< askConnectionRO

instance (MonadIO m, MonadThrow m, MonadDB m) => MonadQueryRW (QueryT 'RW m) where
  liftQueryRW (Query action) = QueryT $ do
    -- NOTE: coercion is needed since we need to implement for both
    --       Query 'RO/'RW
    liftIO . runReaderT action . coerce =<< askConnectionRW

-- | Run read-only query. Note that read-only couldn't be rolled back
--  so they always succeed
runQueryROT
  :: (MonadIO m, MonadMask m)
  => Connection rw
  -> QueryT 'RO m a -> m a
runQueryROT c q = do
  r <- runQueryWorker False c . unQueryT $ q
  case r of
    Nothing -> error "Query 'RO couldn't be rolled back"
    Just a  -> return a

-- | Run read-write query
runQueryRWT
  :: (MonadIO m, MonadMask m)
  => Connection 'RW
  -> QueryT 'RW m a
  -> m (Maybe a)
runQueryRWT c = runQueryWorker True c . unQueryT

-- | Same as 'queryRO' but for 'QueryT'
queryROT
  :: (MonadReadDB m, MonadIO m, MonadMask m)
  => QueryT 'RO m a
  -> m a
queryROT q = flip runQueryROT q =<< askConnectionRO

-- | Same as 'queryRW' but for 'QueryT'
queryRWT
  :: (MonadDB m, MonadIO m, MonadMask m)
  => QueryT 'RW m a
  -> m (Maybe a)
queryRWT q = flip runQueryRWT q =<< askConnectionRW

-- | Same as 'mustQueryRW' but for 'QueryT'
mustQueryRWT
  :: (MonadDB m, MonadIO m, MonadMask m)
  => QueryT 'RW m a
  -> m a
mustQueryRWT q = throwNothing UnexpectedRollback =<< flip runQueryRWT q =<< askConnectionRW


----------------------------------------------------------------
-- Prepared statements
----------------------------------------------------------------

newtype PreparedStmt p = PreparedStmt SQL.Statement

newtype PreparedQuery p res = PreparedQuery SQL.Statement

prepareStatement
  :: (MonadMask m, MonadIO m, MonadDB m)
  => SQL.Query -> m (PreparedStmt p)
prepareStatement sql = do
  c <- askConnectionRW
  PreparedStmt <$> basicPrepare c sql

prepareQuery
  :: (MonadMask m, MonadIO m, MonadReadDB m)
  => SQL.Query -> m (PreparedQuery p r)
prepareQuery sql = do
  c <- askConnectionRO
  PreparedQuery <$> basicPrepare c sql

basicPrepare
  :: (MonadMask m, MonadIO m)
  => Connection rw -> SQL.Query -> m SQL.Statement
basicPrepare Connection{..} sql = liftIO $ mask_ $ do
  stmt <- SQL.openStatement connConn sql
  atomicModifyIORef' connPrepared (\ss -> (stmt:ss, ()))
  return stmt

preparedExecute :: (SQL.ToRow p, MonadQueryRW m) => PreparedStmt p -> p -> m ()
preparedExecute (PreparedStmt stmt) p = liftQueryRW $ Query $ liftIO $ do
  -- There's no special veriosn for prepared statements that doesn't
  -- return data. So we execute with dummy dictionary.
  SQL.bind stmt p
  _ <- SQL.nextRow @[Int] stmt `finally` SQL.reset stmt
  return ()

preparedQuery :: (SQL.ToRow p, SQL.FromRow q, MonadQueryRO m) => PreparedQuery p q -> p -> m [q]
preparedQuery (PreparedQuery stmt) p = liftQueryRO $ Query $ liftIO $ do
  SQL.bind stmt p
  let fetchAll xs = SQL.nextRow stmt >>= \case
        Nothing -> return xs
        Just x  -> fetchAll (xs . (x:))
  (($ []) <$> fetchAll id) `finally` SQL.reset stmt

preparedQuery1 :: (SQL.ToRow p, SQL.FromRow q, MonadQueryRO m) => PreparedQuery p q -> p -> m (Maybe q)
preparedQuery1 (PreparedQuery stmt) p = liftQueryRO $ Query $ liftIO $ do
  SQL.bind stmt p
  SQL.nextRow stmt `finally` SQL.reset stmt



----------------------------------------------------------------
-- Running queries
----------------------------------------------------------------

runQueryWorker
  :: (MonadIO m, MonadMask m)
  => Bool
  -> Connection rw
  -> m x -> m (Maybe x)
runQueryWorker isWrite connection sql = withMVarM mutex $ \case
  False -> throwM DatabaseIsClosed
  True  -> uninterruptibleMask $ \restore -> do
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


----------------------------------------------------------------
-- Newtypes
----------------------------------------------------------------

-- | Newtype wrapper which provides CBOR-encoded To\/FromField
--   instance for values
newtype CBORed a = CBORed { unCBORed :: a }
  deriving Show

instance (Serialise a) => SQL.FromField (CBORed a) where
  fromField f = CBORed . deserialise <$> SQL.fromField f

instance (Serialise a) => SQL.ToField (CBORed a) where
  toField (CBORed a) = SQL.toField (serialise a)


-- | Newtype wrapper which provides To\/FromField
--   instance for values using 'ByteRepr' type class.
newtype ByteRepred a = ByteRepred { unByteRepr :: a }
  deriving Show

instance (ByteRepr a) => SQL.FromField (ByteRepred a) where
  fromField f = do bs <- SQL.fromField f
                   case decodeFromBS bs of
                     Just a  -> return (ByteRepred a)
                     Nothing -> error "ByteRepred: invalid encoding"

instance (ByteRepr a) => SQL.ToField (ByteRepred a) where
  toField (ByteRepred a) = SQL.toField (encodeToBS a)

-- | Newtype wrapper for primary key of database. Its only use to
--   distinguish 'Int64' used in such role from any other 'Int64' and
--   to allow distinguishing ID for different tables.
newtype ID a = ID Int64
  deriving newtype (Show,Eq,Ord)
  deriving newtype (SQL.FromField, SQL.ToField)


fieldCBOR :: (Serialise a) => SQL.RowParser a
fieldCBOR = fmap unCBORed SQL.field

fieldByteRepr :: (ByteRepr a) => SQL.RowParser a
fieldByteRepr = fmap unByteRepr SQL.field

nullableFieldCBOR :: (Serialise a) => SQL.RowParser (Maybe a)
nullableFieldCBOR = (fmap . fmap) unCBORed SQL.field

nullableFieldByteRepr :: (ByteRepr a) => SQL.RowParser (Maybe a)
nullableFieldByteRepr = (fmap . fmap) unByteRepr SQL.field


-- | Newtype wrapper which allows to derive 'MonadReadDB' and
--   'MonadDB' instances using deriving via mechanism by specifying name
--   of field in record carried by reader.
newtype DatabaseByField conn m x = DatabaseByField (m x)
  deriving newtype (Functor,Applicative,Monad)

instance ( MonadReader r m
         , HasField' conn r (Connection 'RW)
         ) => MonadReadDB (DatabaseByField conn m) where
  askConnectionRO = DatabaseByField $ connectionRO <$> view (field' @conn)
  {-# INLINE askConnectionRO #-}

instance ( MonadReader r m
         , HasField' conn r (Connection 'RW)
         ) => MonadDB (DatabaseByField conn m) where
  askConnectionRW = DatabaseByField $ view (field' @conn)
  {-# INLINE askConnectionRW #-}



-- | Newtype wrapper which allows to derive 'MonadReadDB' and
--   'MonadDB' instances using deriving via mechanism by using type of
--   field in record carried by reader.
newtype DatabaseByType m x = DatabaseByType (m x)
  deriving newtype (Functor,Applicative,Monad)

instance ( MonadReader r m
         , HasType (Connection 'RW) r
         ) => MonadReadDB (DatabaseByType m) where
  askConnectionRO = DatabaseByType $ connectionRO <$> view (typed @(Connection 'RW))
  {-# INLINE askConnectionRO #-}

instance ( MonadReader r m
         , HasType (Connection 'RW) r
         ) => MonadDB (DatabaseByType m) where
  askConnectionRW = DatabaseByType $ view typed
  {-# INLINE askConnectionRW #-}


-- | Newtype wrapper which allows to derive 'MonadReadDB' and
--   'MonadDB' instances using deriving via mechanism when connection
--   is carried by reader.
newtype DatabaseByReader m x = DatabaseByReader (m x)
  deriving newtype (Functor,Applicative,Monad)

instance ( MonadReader (Connection 'RW) m
         ) => MonadReadDB (DatabaseByReader m) where
  askConnectionRO = DatabaseByReader $ asks connectionRO
  {-# INLINE askConnectionRO #-}

instance ( MonadReader (Connection 'RW) m
         ) => MonadDB (DatabaseByReader m) where
  askConnectionRW = DatabaseByReader ask
  {-# INLINE askConnectionRW #-}
