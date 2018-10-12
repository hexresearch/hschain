{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- |
module Thundermint.Store.SQLite (
    newSQLiteBlockStorage
  , withSQLiteBlockStorage
  , newSQLiteBlockStorageRO
  , withSQLiteBlockStorageRO
  ) where

import Codec.Serialise (Serialise,serialise,deserialiseOrFail)
import Control.Applicative
import Control.Monad.Trans.Maybe
import Data.Int
-- import qualified Database.SQLite.Simple.FromField as SQL
import qualified Database.SQLite.Simple           as SQL
import           Database.SQLite.Simple             (Only(..))

import Thundermint.Control
import Thundermint.Consensus.Types
import Thundermint.Crypto
import Thundermint.Crypto.Containers
import Thundermint.Store



-- | Create new block storage using specified file as database
newSQLiteBlockStorage
  :: (Crypto alg, Serialise a)
  => FilePath
  -> Block alg a
  -> ValidatorSet alg
  -> IO (BlockStorage 'RW IO alg a)
newSQLiteBlockStorage dbFile gBlock initalVals = do
  conn <- SQL.open dbFile
  initializeDatabase conn gBlock initalVals
  newSQLiteBlockStorageConn conn

-- | Create new block storage using specified file as database
withSQLiteBlockStorage
  :: (Crypto alg, Serialise a)
  => FilePath
  -> Block alg a
  -> ValidatorSet alg
  -> (BlockStorage 'RW IO alg a -> IO b)
  -> IO b
withSQLiteBlockStorage dbFile gBlock initalVals action
  = SQL.withConnection dbFile $ \conn -> do
      initializeDatabase conn gBlock initalVals
      action =<< newSQLiteBlockStorageConn conn

-- | Create new block storage using specified file as database
newSQLiteBlockStorageRO
  :: (Crypto alg, Serialise a)
  => FilePath
  -> IO (BlockStorage 'RO IO alg a)
newSQLiteBlockStorageRO dbFile = do
  conn <- SQL.open dbFile
  makeReadOnly <$> newSQLiteBlockStorageConn conn

-- | Create new block storage using specified file as database
withSQLiteBlockStorageRO
  :: (Crypto alg, Serialise a)
  => FilePath
  -> (BlockStorage 'RO IO alg a -> IO b)
  -> IO b
withSQLiteBlockStorageRO dbFile action
  = SQL.withConnection dbFile $ \conn ->
      action . makeReadOnly =<< newSQLiteBlockStorageConn conn


-- | Create new block storage from connection to SQLite database
newSQLiteBlockStorageConn
  :: forall alg a. (Crypto alg, Serialise a)
  => SQL.Connection
  -> IO (BlockStorage 'RW IO alg a)
newSQLiteBlockStorageConn conn = do
  mutex <- newMutex
  --
  let fetchCommit :: Height -> IO (Maybe (Commit alg a))
      fetchCommit (Height h) = do
        mb <- singleQ conn "SELECT block FROM blockchain WHERE height = ?" (Only (h+1))
        return $ blockLastCommit =<< mb
  -- Make dictionary
  return BlockStorage
    { blockchainHeight = withMutex mutex $
        SQL.query_ conn "SELECT MAX(height) FROM blockchain" >>= \case
          []       -> error "Blockchain cannot be empty"
          [Only i] -> return (Height i)
          _        -> error "Impossible"
    --
    , retrieveBlock = \(Height h) -> withMutex mutex $
        singleQ conn "SELECT block FROM blockchain WHERE height = ?" (Only h)
    --
    , retrieveBlockID = \(Height h) -> withMutex mutex $
        singleQ conn "SELECT bid FROM blockchain WHERE height = ?" (Only h)
    --
    , retrieveCommitRound = \(Height h) -> withMutex mutex $ runMaybeT $ do
        c <-  MaybeT (fetchCommit (Height h))
          <|> MaybeT (singleQ conn "SELECT cmt FROM commits WHERE height = ?" (Only h))
        let getRound (Commit _ (v:_)) = voteRound (signedValue v)
            getRound _                = error "Impossible"
        return $ getRound c
    --
    , retrieveCommit = withMutex mutex . fetchCommit
    --
    , retrieveLocalCommit = \(Height h) -> withMutex mutex $
        singleQ conn "SELECT cmt FROM commits WHERE height = ?" (Only h)
    --
    , retrieveValidatorSet = \(Height h) -> withMutex mutex $
        singleQ conn "SELECT valset FROM validators WHERE height = ?" (Only h)
    --
    , storeCommit = \vals cmt blk -> withMutex mutex $ do
        let Height h = headerHeight $ blockHeader blk
        SQL.execute_ conn "BEGIN TRANSACTION"
        SQL.execute conn "INSERT INTO commits VALUES (?,?)" (h, serialise cmt)
        SQL.execute conn "INSERT INTO blockchain VALUES (?,?,?)"
          ( h
          , serialise (blockHash blk :: BlockID alg a)
          , serialise blk
          )
        SQL.execute  conn "INSERT INTO validators VALUES (?,?)"
          (h+1, serialise vals)
        SQL.execute_ conn "COMMIT"
    --
    , closeBlockStorage = SQL.close conn
    -- We don't write identical messages on disk
    , writeToWAL = \(Height h) msg -> withMutex mutex $
        SQL.execute conn "INSERT OR IGNORE INTO wal VALUES (NULL,?,?)" (h, serialise msg)
    --
    , resetWAL = \(Height h) -> withMutex mutex $
        SQL.execute conn "DELETE FROM wal WHERE height < ?" (Only h)
    --
    , readWAL = \(Height h) -> withMutex mutex $ do
        rows <- SQL.query conn "SELECT message FROM wal WHERE height = ? ORDER BY id" (Only h)
        return [ case deserialiseOrFail bs of
                   Right a -> a
                   Left  e -> error ("CBOR encoding error: " ++ show e)
               | Only bs <- rows
               ]
    }

initializeDatabase
  :: forall alg a. (Crypto alg, Serialise a)
  => SQL.Connection
  -> Block alg a
  -> ValidatorSet alg
  -> IO ()
initializeDatabase conn gBlock initalVals = do
  -- Initialize tables
  SQL.execute_ conn
    "CREATE TABLE IF NOT EXISTS blockchain \
    \  ( height INT NOT NULL UNIQUE \
    \  , bid    BLOB NOT NULL \
    \  , block  BLOB NOT NULL)"
  SQL.execute_ conn
    "CREATE TABLE IF NOT EXISTS commits \
    \  ( height INT  NOT NULL UNIQUE \
    \  , cmt    BLOB NOT NULL)"
  SQL.execute_ conn
    "CREATE TABLE IF NOT EXISTS validators \
    \  ( height INT  NOT NULL UNIQUE \
    \  , valset BLOB NOT NULL)"
  -- ID field is used to keep order of messages. Primary key is
  -- incremented automatically and any new key will be large than any
  -- existing key
  --
  -- Note UNIQUE constraint. Receiving identical message at the same
  -- height is noop so there's no point in storing them. Performance
  -- gains are massive since writing to disk is slow, no SLOW.
  SQL.execute_ conn
    "CREATE TABLE IF NOT EXISTS wal \
    \  ( id      INTEGER PRIMARY KEY \
    \  , height  INTEGER NOT NULL \
    \  , message BLOB NOT NULL \
    \  , UNIQUE(height,message))"
  -- Insert genesis block if needed
  SQL.query_ conn "SELECT MAX(height) FROM blockchain" >>= \case
    [Only Nothing] -> do
      SQL.execute conn "INSERT INTO blockchain VALUES (?,?,?)"
        ( 0 :: Int64
        , serialise (blockHash gBlock :: BlockID alg a)
        , serialise gBlock)
      SQL.execute conn "INSERT INTO validators VALUES (?,?)"
        ( 1 :: Int64
        , serialise initalVals
        )
    (_::[Only (Maybe Int64)]) -> return ()
  

----------------------------------------------------------------
-- Helpers
----------------------------------------------------------------

-- Query that returns 0 or 1 result which is CBOR-encoded value
singleQ :: (SQL.ToRow p, Serialise a)
        => SQL.Connection -> SQL.Query -> p -> IO (Maybe a)
singleQ conn query p =
  SQL.query conn query p >>= \case
    []        -> return Nothing
    [Only bs] -> case deserialiseOrFail bs of
      Right a -> return (Just a)
      Left  e -> error ("CBOR encoding error: " ++ show e)
    _         -> error "Impossible"

---- Query that returns 0 or 1 result which is CBOR-encoded value
--singleFld :: (SQL.ToRow p, SQL.FromField a)
--        => SQL.Connection -> SQL.Query -> p -> IO (Maybe a)
--singleFld conn query p =
--  SQL.query conn query p >>= \case
--    []       -> return Nothing
--    [Only a] -> return (Just a)
--    _        -> error "Impossible"
