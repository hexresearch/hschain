{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
-- |
module Thundermint.Store.SQLite (
  newSQLiteBlockStorage
  ) where

import Codec.Serialise (Serialise,serialise,deserialiseOrFail)
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
  conn <- SQL.open dbFile
  SQL.execute_ conn "CREATE TABLE IF NOT EXISTS blockchain (height INT NOT NULL UNIQUE, bid BLOB NOT NULL, block BLOB NOT NULL)"
  SQL.execute_ conn "CREATE TABLE IF NOT EXISTS commits (height INT NOT NULL UNIQUE, cmt BLOB NOT NULL)"
  SQL.query_ conn "SELECT MAX(height) FROM blockchain" >>= \case
    [Only Nothing] -> SQL.execute conn "INSERT INTO blockchain VALUES (?,?,?)"
                        (0::Int64
                        , serialise (blockHash gBlock :: BlockID alg a)
                        , serialise gBlock)
    (_::[Only (Maybe Int64)]) -> return ()
  --
  
  return BlockStorage
    { blockchainHeight = SQL.query_ conn "SELECT MAX(height) FROM blockchain" >>= \case
        []       -> error "Blockchain cannot be empty"
        [Only i] -> return (Height i)
        _        -> error "Impossible"
    --
    , retrieveBlock = \(Height h) ->
        SQL.query conn "SELECT block FROM blockchain WHERE height = ?" (Only h) >>= \case
          [] -> return Nothing
          [Only bs] -> case deserialiseOrFail bs of
            Right b -> return (Just b)
            Left  e -> error (show e)
          _         -> error "Impossible"
    --
    , retrieveBlockID = \(Height h) ->
        SQL.query conn "SELECT bid FROM blockchain WHERE height = ?" (Only h) >>= \case
          [] -> return Nothing
          [Only bs] -> case deserialiseOrFail bs of
            Right b -> return (Just b)
            Left  e -> error (show e)
          _         -> error "Impossible"
    --
    , retrieveCommit = \(Height h) -> do
        SQL.query conn "SELECT cmt FROM commits WHERE height = ?" (Only h) >>= \case
          [] -> return Nothing
          [Only bs] -> case deserialiseOrFail bs of
            Right c -> return (Just c)
            Left  e -> error (show e)
          _         -> error "Impossible"
    --
    , retrieveLastCommit = do
        SQL.query_ conn "SELECT cmt FROM commits ORDER BY height DESC LIMIT 1" >>= \case
          [] -> return Nothing
          [Only bs] -> case deserialiseOrFail bs of
            Right c -> return (Just c)
            Left  e -> error (show e)
          _         -> error "Impossible"
    --
    , storeCommit = \cmt blk -> do
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
