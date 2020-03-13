{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE MultiWayIf                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE ViewPatterns               #-}
-- |
-- This module provides API for working with persistent (blockchain)
-- and no so persistent (mempool) storage.
module HSChain.PoW.Store (
    -- * Working with database
    -- ** Connection
    Connection
  , Access(..)
  , connectionRO
  , MonadReadDB(..)
  , MonadDB(..)
  , queryRO
  , queryRW
  , mustQueryRW
  , queryROT
  , queryRWT
  , mustQueryRWT
    -- ** Opening\/closing database
  , openConnection
  , closeConnection
  , withConnection
  , initDatabase
  , withDatabase
    -- * Querying database
    -- ** Query monads
  , MonadQueryRO(..)
  , MonadQueryRW(..)
  , Query
  , QueryT
    -- ** Standard DB wrapper
  , DBT(..)
  , dbtRO
  , runDBT
  ) where

import Control.Monad             ((<=<), foldM, forM, unless)
import Control.Monad.Catch       (MonadMask,MonadThrow,MonadCatch)
import Control.Monad.Fail        (MonadFail)
import Control.Monad.Morph       (MFunctor(..))
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Writer

import Data.Foldable             (forM_)
import Data.Maybe                (isNothing, maybe)
import Data.Text                 (Text)
import qualified Data.List.NonEmpty as NE
import GHC.Generics              (Generic)

import HSChain.PoW.Control                (MonadFork)
import HSChain.Crypto
--import HSChain.Crypto.Containers
import HSChain.PoW.Logger                 (MonadLogger)
import HSChain.PoW.Store.Internal.Query
import HSChain.PoW.Store.Internal.BlockDB

----------------------------------------------------------------
-- Monadic API for DB access
----------------------------------------------------------------

-- | Monad transformer which provides 'MonadReadDB' and 'MonadDB'
--   instances.
newtype DBT rw a m x = DBT (ReaderT (Connection rw a) m x)
  deriving ( Functor, Applicative, Monad
           , MonadIO, MonadThrow, MonadCatch, MonadMask
           , MonadFork, MonadLogger, MonadFail
           )

instance MFunctor (DBT rw a) where
  hoist f (DBT m) = DBT $ hoist f m

instance MonadTrans (DBT rw a) where
  lift = DBT . lift

-- | Lift monad which provides read-only access to read-write access.
dbtRO :: DBT 'RO a m x -> DBT rw a m x
dbtRO (DBT m) = DBT (withReaderT connectionRO m)

runDBT :: Connection rw a -> DBT rw a m x -> m x
runDBT c (DBT m) = runReaderT m c

instance MonadIO m => MonadReadDB (DBT rw a m) a where
  askConnectionRO = connectionRO <$> DBT ask
instance MonadIO m => MonadDB (DBT 'RW a m) a where
  askConnectionRW = DBT ask


-- | Helper function which opens database, initializes it and ensures
--   that it's closed on function exit
withDatabase
  :: (MonadIO m, MonadMask m)
  => FilePath         -- ^ Path to the database
  -> (Connection 'RW a -> m x) -> m x
withDatabase path cont
  = withConnection path $ \c -> initDatabase c >> cont c

-- | Initialize all required tables in database.
initDatabase
  :: (MonadIO m)
  => Connection 'RW a  -- ^ Opened connection to database
  -> m ()
initDatabase c = do
  r <- runQueryRW c initializeBlockchainTables
  case r of
    Nothing -> error "Cannot initialize tables!"
    Just () -> return ()


