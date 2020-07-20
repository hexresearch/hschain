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
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}
-- |
-- Building blocks wrapping SQL queries into accessors functions. It's
-- not internal per se but is quite low level and API doesn't provide
-- many guarantees. Use with care.
module HSChain.Store.Internal.Query (
    -- * Connection
    DB.Connection
  , DB.connectionRO
  , DB.openConnection
  , DB.closeConnection
  , DB.withConnection
  , DB.MonadReadDB(..)
  , DB.MonadDB(..)
    -- * MonadQuery
  , DB.MonadQueryRO(..)
  , DB.MonadQueryRW(..)
  , DB.basicLastInsertRowId
  , DB.basicQuery
  , DB.basicQuery1
  , DB.basicQuery_
  , DB.basicExecute
  , DB.basicExecute_
  , DB.rollback
    -- * Caching
    -- ** Data types & type classes
  , Cache(..)
  , Cached(..)
  , newCached
  , MonadCached(..)
  , WithCache(..)
    -- ** Query monad
    -- ** Operations
  , basicCacheGenesis
  , basicCacheBlock
  , basicPutCacheBlock
  , basicPutValidatorSet
  , basicCacheValidatorSet
    -- * Database queries
  , Access(..)
    -- ** Plain queries
  , Query(..)
  , queryRO
  , queryRW
  , mustQueryRW
    -- * sqlite-simple helpers
  , DB.CBORed(..)
  ) where

import Control.Concurrent.MVar
import Control.Monad
import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.State.Strict as SS(StateT(..),get,put)
import Control.Monad.Trans.State.Lazy   as SL(StateT(..))
import Control.Monad.Trans.Except            (ExceptT(..))
import Control.Monad.Trans.Identity          (IdentityT(..))
import Data.Tuple (swap)
import Lens.Micro
import Lens.Micro.TH
import qualified Data.Cache.LRU                   as LRU
import Pipes (Proxy)

import HSChain.Types.Blockchain
import HSChain.Types.Validators
import           HSChain.Store.Query   (Access(..))
import qualified HSChain.Store.Query as DB

----------------------------------------------------------------
-- Connection to SQL database
----------------------------------------------------------------

-- | Newtype wrapper holding MVar with cache for database
--   requests. It's used outside of transaction and carried around in
--   some sort of reader monad.
newtype Cached a = Cached (MVar (Cache a))

newCached :: MonadIO m => m (Cached a)
newCached = liftIO $ Cached <$> newMVar Cache
  { _cacheGen  = Nothing
  , _cacheBlk  = LRU.newLRU (Just 8)
  , _cacheVSet = LRU.newLRU (Just 8)
  }

-- | Monad which has access to mutable reference holding cache for
--   database requests.
class Monad m => MonadCached a m | m -> a where
  askCached :: m (Cached a)

-- | In-memory cache for database requests.
data Cache a = Cache
  { _cacheGen  :: !(Maybe (Block a))
    -- ^ Genesis
  , _cacheBlk  :: !(LRU.LRU Height (Block a))
    -- ^ LRU cache for blocks
  , _cacheVSet :: !(LRU.LRU Height (ValidatorSet (Alg a)))
    -- ^ LRU cache for validator sets
  }

$(makeLenses ''Cache)


-- | Monad which works with cache as state monad. It's meant to be
--   used as part of transaction.
class Monad m => WithCache a m | m -> a where
  putCache :: Cache a -> m ()
  getCache :: m (Cache a)

----------------------------------------------------------------
-- Query monad supporting querying
----------------------------------------------------------------

newtype Query rw a x = Query (SS.StateT (Cache a) (DB.Query rw) x)
  deriving newtype ( Functor, Applicative, Monad, MonadThrow, MonadFail
                   , DB.MonadQueryRO)

deriving newtype instance (rw ~ 'RW) => DB.MonadQueryRW (Query rw a)

instance WithCache a (Query rw a) where
  getCache = Query   SS.get
  putCache = Query . SS.put

queryRO
  :: (DB.MonadReadDB m, MonadCached a m, MonadIO m)
  => Query 'RO a x -> m x
queryRO (Query query) = do
  conn      <- DB.askConnectionRO
  Cached mv <- askCached
  liftIO $ modifyMVar mv $ fmap swap . DB.runQueryRO conn . SS.runStateT query

queryRW
  :: (DB.MonadDB m, MonadCached a m, MonadIO m)
  => Query 'RW a x -> m (Maybe x)
queryRW (Query query) = do
  conn      <- DB.askConnectionRW
  Cached mv <- askCached
  liftIO $ modifyMVar mv $ \c -> do
    res <- DB.runQueryRW conn $ SS.runStateT query c
    return $ case res of
      Nothing     -> (c , Nothing)
      Just (a,c') -> (c', Just a )

mustQueryRW
  :: (DB.MonadDB m, MonadCached a m, MonadIO m)
  => Query 'RW a x -> m x
mustQueryRW (Query query) = do
  conn      <- DB.askConnectionRW
  Cached mv <- askCached
  liftIO $ modifyMVar mv $ \c -> do
    res <- DB.runQueryRW conn $ SS.runStateT query c
    case res of
      Nothing     -> throwM DB.UnexpectedRollback
      Just (a,c') -> return (c', a)





----------------------------------------------------------------
-- Primitives for caching
----------------------------------------------------------------

basicCacheGenesis
  :: WithCache a m => m (Maybe (Block a)) -> m (Maybe (Block a))
basicCacheGenesis query = do
  c <- getCache
  case c^.cacheGen of
    Just b  -> return (Just b)
    Nothing -> do !b <- query
                  putCache $ c & cacheGen .~ b
                  return b

basicCacheBlock
  :: WithCache a m
  => (Height -> m (Maybe (Block a)))
  -> (Height -> m (Maybe (Block a)))
basicCacheBlock = basicCacheLRU cacheBlk

basicCacheValidatorSet
  :: WithCache a m
  => (Height -> m (Maybe (ValidatorSet (Alg a))))
  -> (Height -> m (Maybe (ValidatorSet (Alg a))))
basicCacheValidatorSet = basicCacheLRU cacheVSet

basicPutCacheBlock :: WithCache a m => Block a -> m ()
basicPutCacheBlock b = basicPutCacheLRU cacheBlk (blockHeight b) b

basicPutValidatorSet :: WithCache a m => Height -> ValidatorSet (Alg a) -> m ()
basicPutValidatorSet = basicPutCacheLRU cacheVSet


basicCacheLRU
  :: (WithCache a m, Ord k)
  => Lens' (Cache a) (LRU.LRU k b)
  -> (k -> m (Maybe b))
  -> (k -> m (Maybe b))
basicCacheLRU len query h = do
  c <- getCache
  case h `LRU.lookup` (c^.len) of
    (cb, Just b ) -> do putCache (c & len .~ cb)
                        return (Just b)
    (_ , Nothing) -> do mb <- query h
                        forM_ mb $ \b -> putCache $ c & len %~ LRU.insert h b
                        return mb

basicPutCacheLRU
  :: (WithCache a m, Ord k)
  => Lens' (Cache a) (LRU.LRU k b) -> k -> b -> m ()
basicPutCacheLRU len h b = do
  c <- getCache
  putCache $ c & len %~ LRU.insert h b


----------------------------------------------------------------
-- Instances
----------------------------------------------------------------

instance MonadCached x m => MonadCached x (Proxy a b c d m) where
  askCached = lift askCached

instance MonadCached a m => MonadCached a (SS.StateT s m) where
  askCached = lift askCached
instance MonadCached a m => MonadCached a (SL.StateT s m) where
  askCached = lift askCached
instance MonadCached a m => MonadCached a (ReaderT r m) where
  askCached = lift askCached
instance MonadCached a m => MonadCached a (IdentityT m) where
  askCached = lift askCached
instance MonadCached a m => MonadCached a (MaybeT m) where
  askCached = lift askCached
instance MonadCached a m => MonadCached a (ExceptT e m) where
  askCached = lift askCached
