{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- |
-- Data types for primary database. Namely storage of blocks, commits and validators
module Thundermint.Store.Internal.BlockDB (
    BlockStorage(..)
  , blockStorage
  , initializeBlockhainTables
  ) where

import Codec.Serialise (Serialise,serialise,deserialiseOrFail)
import Control.Applicative
import Control.Monad.Trans.Maybe
import Data.Int
import Data.Text (Text)
-- import qualified Database.SQLite.Simple.FromField as SQL
import qualified Database.SQLite.Simple           as SQL
import           Database.SQLite.Simple             (Only(..))

import Thundermint.Control
import Thundermint.Blockchain.Types
import Thundermint.Blockchain.Internal.Message
import Thundermint.Crypto
import Thundermint.Crypto.Containers
import Thundermint.Store.Internal.Query

----------------------------------------------------------------
--
----------------------------------------------------------------

-- | Create tables for storing blockchain data
initializeBlockhainTables
  :: forall alg a. (Crypto alg, Eq (PublicKey alg), Serialise a, Eq a)
  => Block alg a                -- ^ Genesis block
  -> ValidatorSet alg           -- ^ Initial validator set
  -> Query 'RW ()
initializeBlockhainTables genesis initialVals = do
  -- Initialize tables for storage of blockchain
  execute_
    "CREATE TABLE IF NOT EXISTS blockchain \
    \  ( height INT NOT NULL UNIQUE \
    \  , bid    BLOB NOT NULL \
    \  , block  BLOB NOT NULL)"
  execute_
    "CREATE TABLE IF NOT EXISTS commits \
    \  ( height INT  NOT NULL UNIQUE \
    \  , cmt    BLOB NOT NULL)"
  execute_
    "CREATE TABLE IF NOT EXISTS validators \
    \  ( height INT  NOT NULL UNIQUE \
    \  , valset BLOB NOT NULL)"
  -- Checkpoints for user state
  execute_
    "CREATE TABLE IF NOT EXISTS thm_checkpoints \
    \  ( tableName TEXT NOT NULL \
    \  , height    INT  NOT NULL \
    \  , id        INT \
    \  , UNIQUE (tableName, height))"
  -- WAL
  --
  -- ID field is used to keep order of messages. Primary key is
  -- incremented automatically and any new key will be large than any
  -- existing key
  --
  -- Note UNIQUE constraint. Receiving identical message at the same
  -- height is noop so there's no point in storing them. Performance
  -- gains are massive since writing to disk is slow, no SLOW.
  execute_
    "CREATE TABLE IF NOT EXISTS wal \
    \  ( id      INTEGER PRIMARY KEY \
    \  , height  INTEGER NOT NULL \
    \  , message BLOB NOT NULL \
    \  , UNIQUE(height,message))"
  -- Insert genesis block if needed
  storedGen  <- query "SELECT block  FROM blockchain WHERE height = 0" ()
  storedVals <- query "SELECT valset FROM validators WHERE height = 1" ()
  case () of
     -- Fresh DB without genesis block
    _| [] <- storedGen
     , [] <- storedVals
       -> do execute "INSERT INTO blockchain VALUES (?,?,?)"
               ( 0 :: Int64
               , serialise (blockHash genesis :: BlockID alg a)
               , serialise genesis
               )
             execute "INSERT INTO validators VALUES (?,?)"
               ( 1 :: Int64
               , serialise initialVals
               )
     -- Genesis and validator set matches ones recorded
     | [Only blk ]    <- storedGen
     , Right genesis' <- deserialiseOrFail blk
     , genesis == genesis'
     , [Only vals]        <- storedVals
     , Right initialVals' <- deserialiseOrFail vals
     , initialVals == initialVals'
       -> return ()
     -- Otherwise we're reading wrong database. No other way but fiery death
     | otherwise -> error "initializeBlockhainTables: inconsistent genesis/initial validators"



----------------------------------------------------------------
-- Public API
----------------------------------------------------------------

-- | API for persistent storage of blockchain and related
--   information. All assertion about behavior obviously hold only if
--   database backing store is not corrupted.
--
--   NOTE: previously this data type was closure over DB connection,
--         now it's just collection of functions and we only need to
--         implement 'retrieveCommitRound'. It doesn't have @a@ nor
--         @alg@ in its type signature yet depends on these instances.
data BlockStorage alg a = BlockStorage
  { blockchainHeight   :: forall rw. Query rw Height
    -- ^ Current height of blockchain (height of last commited block).

  , retrieveBlock      :: forall rw. Height -> Query rw (Maybe (Block alg a))
    -- ^ Retrieve block at given height.
    --
    --   Must return block for every height @0 <= h <= blockchainHeight@
  , retrieveBlockID    :: forall rw. Height -> Query rw (Maybe (BlockID alg a))
    -- ^ Retrieve ID of block at given height. Must return same result
    --   as @fmap blockHash . retrieveBlock@ but implementation could
    --   do that more efficiently.
  , retrieveCommit     :: forall rw. Height -> Query rw (Maybe (Commit alg a))
    -- ^ Retrieve commit justifying commit of block at height
    --   @h@. Must return same result as @fmap blockLastCommit . retrieveBlock . next@
    --   but do it more efficiently.
    --
    --   Note that this method returns @Nothing@ for last block since
    --   its commit is not persisted in blockchain yet and there's no
    --   commit for genesis block (h=0)
  , retrieveCommitRound :: forall rw. Height -> Query rw (Maybe Round)
    -- ^ Retrieve round when commit was made.

  , storeCommit :: ValidatorSet alg -> Commit alg a -> Block alg a -> Query 'RW ()
    -- ^ Write block and commit justifying it into persistent storage.

  , retrieveLocalCommit :: forall rw. Height -> Query rw (Maybe (Commit alg a))
    -- ^ Retrieve local commit justifying commit of block as known by
    --   node at moment of the commit. Implementation only MUST store
    --   commit for the last block but may choose to store earlier
    --   commits as well.
    --
    --   Note that commits returned by this functions may to differ
    --   from ones returned by @retrieveCommit@ by set of votes since
    --   1) @retrieveCommit@ retrieve commit as seen by proposer not
    --   local node 2) each node collect straggler precommits for some
    --   time interval after commit.
  , retrieveValidatorSet :: forall rw. Height -> Query rw (Maybe (ValidatorSet alg))
    -- ^ Retrieve set of validators for given round.
    --
    --   Must return validator set for every @0 < h <= blockchainHeight + 1@

  , writeToWAL :: Height -> MessageRx 'Unverified alg a -> Query 'RW ()
    -- ^ Add message to Write Ahead Log. Height parameter is height
    --   for which we're deciding block.
  , resetWAL   :: forall rw. Height -> Query rw ()
    -- ^ Remove all entries from WAL which comes from height less than
    --   parameter.
  , readWAL    :: forall rw. Height -> Query rw [MessageRx 'Unverified alg a]
    -- ^ Get all parameters from WAL in order in which they were
    --   written
  }

-- | Create new block storage from connection to SQLite database
blockStorage
  :: forall alg a. (Crypto alg, Serialise a)
  => BlockStorage alg a
blockStorage = BlockStorage
  { blockchainHeight =
      query "SELECT MAX(height) FROM blockchain" () >>= \case
        []       -> error "Blockchain cannot be empty"
        [Only i] -> return (Height i)
        _        -> error "Impossible"
  --
  , retrieveBlock = \(Height h) ->
      singleQ "SELECT block FROM blockchain WHERE height = ?" (Only h)
  --
  , retrieveBlockID = \(Height h) ->
      singleQ "SELECT bid FROM blockchain WHERE height = ?" (Only h)
  --
  , retrieveCommitRound = \(Height h) -> runMaybeT $ do
      c <-  MaybeT (fetchCommit (Height h))
        <|> MaybeT (singleQ "SELECT cmt FROM commits WHERE height = ?" (Only h))
      let getRound (Commit _ (v:_)) = voteRound (signedValue v)
          getRound _                = error "Impossible"
      return $ getRound c
  --
  , retrieveCommit = fetchCommit
  --
  , retrieveLocalCommit = \(Height h) ->
      singleQ "SELECT cmt FROM commits WHERE height = ?" (Only h)
  --
  , retrieveValidatorSet = \(Height h) ->
      singleQ "SELECT valset FROM validators WHERE height = ?" (Only h)
  --
  , storeCommit = \vals cmt blk -> do
      let Height h = headerHeight $ blockHeader blk
      execute "INSERT INTO commits VALUES (?,?)" (h, serialise cmt)
      execute "INSERT INTO blockchain VALUES (?,?,?)"
        ( h
        , serialise (blockHash blk :: BlockID alg a)
        , serialise blk
        )
      execute "INSERT INTO validators VALUES (?,?)"
        (h+1, serialise vals)
  -- NOTE: We don't write identical messages on disk
  , writeToWAL = \(Height h) msg ->
      execute "INSERT OR IGNORE INTO wal VALUES (NULL,?,?)" (h, serialise msg)
  --
  , resetWAL = \(Height h) ->
      execute "DELETE FROM wal WHERE height < ?" (Only h)
  --
  , readWAL = \(Height h) -> do
      rows <- query "SELECT message FROM wal WHERE height = ? ORDER BY id" (Only h)
      return [ case deserialiseOrFail bs of
                 Right a -> a
                 Left  e -> error ("CBOR encoding error: " ++ show e)
             | Only bs <- rows
             ]
  }
  where
    fetchCommit :: Height -> Query rw (Maybe (Commit alg a))
    fetchCommit (Height h) = do
      mb <- singleQ "SELECT block FROM blockchain WHERE height = ?" (Only (h+1))
      return $ blockLastCommit =<< mb

-- Query that returns 0 or 1 result which is CBOR-encoded value
singleQ :: (SQL.ToRow p, Serialise a)
        => Text -> p -> Query rw (Maybe a)
singleQ sql p =
  query sql p >>= \case
    []        -> return Nothing
    [Only bs] -> case deserialiseOrFail bs of
      Right a -> return (Just a)
      Left  e -> error ("CBOR encoding error: " ++ show e)
    _         -> error "Impossible"
