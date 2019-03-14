{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
-- |
-- Data types for primary database. Namely storage of blocks, commits and validators
module Thundermint.Store.Internal.BlockDB where

import Codec.Serialise (Serialise,serialise,deserialiseOrFail)
import Control.Applicative
import Control.Monad (when)
import Control.Monad.Trans.Maybe
import qualified Data.ByteString.Lazy as LBS
import Data.Int
import Data.Text (Text)
import qualified Database.SQLite.Simple           as SQL
import           Database.SQLite.Simple             (Only(..))

import Thundermint.Types.Blockchain
import Thundermint.Blockchain.Internal.Types
import Thundermint.Crypto
import Thundermint.Types.Validators
import Thundermint.Store.Internal.Query


----------------------------------------------------------------
--
----------------------------------------------------------------

-- | Create tables for storing blockchain data
initializeBlockhainTables
  :: (Crypto alg, Eq (PublicKey alg), Serialise a, Eq a)
  => Block alg a                -- ^ Genesis block
  -> ValidatorSet alg           -- ^ Initial validator set
  -> Query 'RW alg a ()
initializeBlockhainTables genesis initialVals = do
  -- Initialize tables for storage of blockchain
  execute_
    "CREATE TABLE IF NOT EXISTS blockchain \
    \  ( height INT NOT NULL UNIQUE \
    \  , bid    BLOB NOT NULL \
    \  , block  BLOB NOT NULL)"
  execute_
    "CREATE TABLE IF NOT EXISTS state_snapshot \
    \  ( height           INT  NOT NULL \
    \  , snapshot_blob    BLOB NOT NULL)"
  r <- query "SELECT height FROM state_snapshot" ()
  when (null (r :: [[SQL.SQLData]])) $ execute_ "INSERT INTO state_snapshot (height, snapshot_blob) VALUES (-1, X'')"
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
    \  , ver       INT \
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
               , serialise (blockHash genesis)
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

-- | Current height of blockchain (height of last commited block).
blockchainHeight :: Query rw alg a Height
blockchainHeight =
  query "SELECT MAX(height) FROM blockchain" () >>= \case
    []       -> error "Blockchain cannot be empty"
    [Only i] -> return (Height i)
    _        -> error "Impossible"


-- | Retrieve block at given height.
--
--   Must return block for every height @0 <= h <= blockchainHeight@
retrieveBlock :: (Serialise a, Crypto alg) => Height -> Query rw alg a (Maybe (Block alg a))
retrieveBlock (Height h) =
  singleQ "SELECT block FROM blockchain WHERE height = ?" (Only h)

-- | Retrieve ID of block at given height. Must return same result
--   as @fmap blockHash . retrieveBlock@ but implementation could
--   do that more efficiently.
retrieveBlockID :: Height -> Query rw alg a (Maybe (BlockID alg a))
retrieveBlockID (Height h) =
  singleQ "SELECT bid FROM blockchain WHERE height = ?" (Only h)

-- | Retrieve commit justifying commit of block at height
--   @h@. Must return same result as @fmap blockLastCommit . retrieveBlock . next@
--   but do it more efficiently.
--
--   Note that this method returns @Nothing@ for last block since
--   its commit is not persisted in blockchain yet and there's no
--   commit for genesis block (h=0)
retrieveCommit :: (Serialise a, Crypto alg) => Height -> Query rw alg a (Maybe (Commit alg a))
retrieveCommit (Height h) = do
  mb <- singleQ "SELECT block FROM blockchain WHERE height = ?" (Only (h+1))
  return $ blockLastCommit =<< mb

-- | Retrieve round when commit was made.
retrieveCommitRound :: (Serialise a, Crypto alg) => Height -> Query rw alg a (Maybe Round)
retrieveCommitRound (Height h) = runMaybeT $ do
  c <-  MaybeT (retrieveCommit (Height h))
    <|> MaybeT (singleQ "SELECT cmt FROM commits WHERE height = ?" (Only h))
  let getRound (Commit _ (v:_)) = voteRound (signedValue v)
      getRound _                = error "Impossible"
  return $ getRound c


-- | Retrieve local commit justifying commit of block as known by
--   node at moment of the commit. Implementation only MUST store
--   commit for the last block but may choose to store earlier
--   commits as well.
--
--   Note that commits returned by this functions may to differ
--   from ones returned by @retrieveCommit@ by set of votes since
--   1) @retrieveCommit@ retrieve commit as seen by proposer not
--   local node 2) each node collect straggler precommits for some
--   time interval after commit.
retrieveLocalCommit :: Height -> Query rw alg a (Maybe (Commit alg a))
retrieveLocalCommit (Height h) =
  singleQ "SELECT cmt FROM commits WHERE height = ?" (Only h)

-- | Retrieve set of validators for given round.
--
--   Must return validator set for every @0 < h <= blockchainHeight + 1@
retrieveValidatorSet :: (Crypto alg) =>  Height -> Query rw alg a (Maybe (ValidatorSet alg))
retrieveValidatorSet (Height h) =
  singleQ "SELECT valset FROM validators WHERE height = ?" (Only h)

-- |Retrieve height and state saved as snapshot.
--
retrieveSavedState :: Serialise s => Query 'RO alg a (Maybe (Height, s))
retrieveSavedState =
  singleQWithParser parse "SELECT height, snapshot_blob FROM state_snapshot" ()
  where
    parse [SQL.SQLInteger h, SQL.SQLBlob s]
      | h > 0
      , Right r <- deserialiseOrFail (LBS.fromStrict s) = Just (Height $ fromIntegral h, r)
      | otherwise = Nothing
    parse _ = Nothing


----------------------------------------------------------------
-- Writing to DB
----------------------------------------------------------------

-- | Write block and commit justifying it into persistent storage.
storeCommit
  :: (Crypto alg, Serialise a)
  => Commit alg a -> Block alg a -> Query 'RW alg a ()
storeCommit cmt blk = do
  let Height h = headerHeight $ blockHeader blk
  execute "INSERT INTO commits VALUES (?,?)" (h, serialise cmt)
  execute "INSERT INTO blockchain VALUES (?,?,?)"
    ( h
    , serialise (blockHash blk)
    , serialise blk
    )

-- | Write state snapshot into DB.
-- @maybeSnapshot@ contains a serialized value of a state associated with the processed block.
storeStateSnapshot
  :: Serialise s =>
  Height -> s -> Query 'RW alg a ()
storeStateSnapshot (Height h) state = do
  execute "UPDATE state_snapshot SET height = ?, snapshot_blob = ?" (h, serialise state)


-- | Write validator set for next round into database
storeValSet :: (Crypto alg) => Block alg a -> ValidatorSet alg -> Query 'RW alg a ()
storeValSet blk vals = do
  let Height h = headerHeight $ blockHeader blk
  execute "INSERT INTO validators VALUES (?,?)"
    (h+1, serialise vals)

-- | Add message to Write Ahead Log. Height parameter is height
--   for which we're deciding block.
writeToWAL :: (Serialise a, Crypto alg) => Height -> MessageRx 'Unverified alg a -> Query 'RW alg a ()
writeToWAL (Height h) msg =
  execute "INSERT OR IGNORE INTO wal VALUES (NULL,?,?)" (h, serialise msg)

-- | Remove all entries from WAL which comes from height less than
--   parameter.
resetWAL :: Height -> Query 'RW alg a ()
resetWAL (Height h) =
  execute "DELETE FROM wal WHERE height < ?" (Only h)

-- | Get all parameters from WAL in order in which they were
--   written
readWAL :: (Serialise a, Crypto alg) => Height -> Query rw alg a [MessageRx 'Unverified alg a]
readWAL (Height h) = do
  rows <- query "SELECT message FROM wal WHERE height = ? ORDER BY id" (Only h)
  return [ case deserialiseOrFail bs of
             Right a -> a
             Left  e -> error ("CBOR encoding error: " ++ show e)
         | Only bs <- rows
         ]


-- Query that returns 0 or 1 result which is CBOR-encoded value
singleQ :: (SQL.ToRow p, Serialise x)
        => Text -> p -> Query rw alg a (Maybe x)
singleQ sql p =
  query sql p >>= \case
    []        -> return Nothing
    [Only bs] -> case deserialiseOrFail bs of
      Right a -> return (Just a)
      Left  e -> error ("CBOR encoding error: " ++ show e)
    _         -> error "Impossible"

-- Query that returns results parsed from single row ().
singleQWithParser :: (SQL.ToRow p)
        => ([SQL.SQLData] -> Maybe x) -> Text -> p -> Query rw alg a (Maybe x)
singleQWithParser resultsParser sql p =
  query sql p >>= \case
    [x] -> return (resultsParser x)
    _ -> error $ "SQL statement resulted in too many (>1) or zero result rows: " ++ show sql
  where
