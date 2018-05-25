{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- |
module Thundermint.Store.SQLite (
  newSQLiteBlockStorage
  ) where

import Codec.Serialise (Serialise,serialise,deserialiseOrFail)
import Control.Concurrent.MVar
import Data.Int
import qualified Database.SQLite.Simple as SQL
import           Database.SQLite.Simple   (Only(..))

import Thundermint.Consensus.Types
import Thundermint.Crypto
import Thundermint.Store



newSQLiteBlockStorage
  :: forall alg a. (Crypto alg, Serialise a)
  => FilePath
  -> Block alg a
  -> IO (BlockStorage 'RW IO alg a)
newSQLiteBlockStorage dbFile gBlock = do
  mutex <- newMutex
  conn  <- SQL.open dbFile
  -- Initialize tables
  SQL.execute_ conn
    "CREATE TABLE IF NOT EXISTS blockchain \
    \  ( height INT NOT NULL UNIQUE \
    \  , bid    BLOB NOT NULL \
    \  , block  BLOB NOT NULL)"
  SQL.execute_ conn
    "CREATE TABLE IF NOT EXISTS commits \
    \  ( height INT NOT NULL UNIQUE \
    \  , cmt    BLOB NOT NULL)"
  -- Insert genesis block if needed
  SQL.query_ conn "SELECT MAX(height) FROM blockchain" >>= \case
    [Only Nothing] -> SQL.execute conn "INSERT INTO blockchain VALUES (?,?,?)"
                        ( 0 :: Int64
                        , serialise (blockHash gBlock :: BlockID alg a)
                        , serialise gBlock)
    (_::[Only (Maybe Int64)]) -> return ()
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
    , retrieveCommit = \(Height h) -> withMutex mutex $
        singleQ conn "SELECT cmt FROM commits WHERE height = ?" (Only h)
    --
    , retrieveLastCommit = withMutex mutex $
        singleQ conn "SELECT cmt FROM commits ORDER BY height DESC LIMIT 1" ()
    --
    , storeCommit = \cmt blk -> withMutex mutex $ do
        let Height h = headerHeight $ blockHeader blk
        SQL.execute_ conn "BEGIN TRANSACTION"
        SQL.execute conn "INSERT INTO commits VALUES (?,?)" (h, serialise cmt)
        SQL.execute conn "INSERT INTO blockchain VALUES (?,?,?)"
          ( h
          , serialise (blockHash blk :: BlockID alg a)
          , serialise blk
          )
        SQL.execute_ conn "COMMIT"
    }

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


newtype Mutex = Mutex (MVar ())

newMutex :: IO Mutex
newMutex = Mutex <$> newMVar ()

withMutex :: Mutex -> IO a -> IO a
withMutex (Mutex mvar) = withMVar mvar . const
