{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE MultiWayIf          #-}
{-# LANGUAGE OverloadedStrings   #-}

-- |
-- Data types for primary database. Namely storage of blocks, commits and validators
module HSChain.Store.Internal.BlockDB where

import Codec.Serialise (Serialise, serialise, deserialiseOrFail)
import Control.Monad (when)
import qualified Data.List.NonEmpty   as NE
import qualified Data.ByteString.Lazy as LBS
import qualified Database.SQLite.Simple           as SQL
import           Database.SQLite.Simple             (Only(..))

import HSChain.Types.Blockchain
import HSChain.Blockchain.Internal.Types
import HSChain.Crypto
import HSChain.Types.Validators
import HSChain.Store.Internal.Query


----------------------------------------------------------------
--
----------------------------------------------------------------

-- | Create tables for storing blockchain data
initializeBlockhainTables
  :: (Crypto alg, Serialise a, Eq a, MonadQueryRW m alg a, Show a)
  => Block alg a                -- ^ Genesis block
  -> m ()
initializeBlockhainTables genesis = do
  -- Initialize tables for storage of blockchain
  basicExecute_
    "CREATE TABLE IF NOT EXISTS thm_blockchain \
    \  ( height INT NOT NULL UNIQUE \
    \  , round  INT NOT NULL  \
    \  , bid    BLOB NOT NULL \
    \  , block  BLOB NOT NULL)"
  basicExecute_
    "CREATE TABLE IF NOT EXISTS state_snapshot \
    \  ( height           INT  NOT NULL \
    \  , snapshot_blob    BLOB NOT NULL)"
  r <- basicQuery_ "SELECT height FROM state_snapshot"
  when (null (r :: [[SQL.SQLData]])) $
    basicExecute_ "INSERT INTO state_snapshot (height, snapshot_blob) VALUES (-1, X'')"
  basicExecute_
    "CREATE TABLE IF NOT EXISTS thm_commits \
    \  ( height INT  NOT NULL UNIQUE \
    \  , cmt    BLOB NOT NULL)"
  basicExecute_
    "CREATE TABLE IF NOT EXISTS thm_validators \
    \  ( height INT  NOT NULL UNIQUE \
    \  , valset BLOB NOT NULL)"
  -- Checkpoints for user state
  basicExecute_
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
  basicExecute_
    "CREATE TABLE IF NOT EXISTS thm_wal \
    \  ( id      INTEGER PRIMARY KEY \
    \  , height  INTEGER NOT NULL \
    \  , message BLOB NOT NULL \
    \  , UNIQUE(height,message))"
  -- Insert genesis block if needed
  storedGen  <- basicQuery_ "SELECT block  FROM thm_blockchain WHERE height = 0"
  storedVals <- basicQuery_ "SELECT valset FROM thm_validators WHERE height = 1"
  let initialVals = case changeValidators (blockValChange genesis) emptyValidatorSet of
        Just v  -> v
        Nothing -> error "initializeBlockhainTables: cannot apply change of validators"
      checkResult = genCheck <> valCheck
      genCheck    = case storedGen of
        []         -> []
        [Only blk] -> case deserialiseOrFail blk of
          Right genesis'
            | genesis == genesis' -> []
            | otherwise           ->
                [ "Genesis blocks do not match:"
                , "  stored: " ++ show genesis'
                , "  expected: " ++ show genesis
                ]
          Left e -> [ "Deserialisation for genesis failed:"
                    , "  " ++ show e
                    ]
        _        -> ["DB corruption. Multiple blocks at H=0"]
      valCheck = case storedVals of
        []        -> []
        [Only vs] -> case deserialiseOrFail vs of
          Right initialVals'
            | initialVals == initialVals' -> []
            | otherwise                   ->
                [ "Validators set are not equal:"
                , "  stored:   " ++ show initialVals'
                , "  expected: " ++ show initialVals
                ]
          Left e -> [ "Unable to deserialise validator set"
                    , "  " ++ show e
                    ]
        _        -> ["DB corruption. Multiple validator sets at H=1"]
  if -- Initial validator set must not be empty
     | validatorSetSize initialVals == 0
       -> error "initializeBlockhainTables: Invalid genesis: empty validator set"
     -- Fresh DB without genesis block
     | [] <- storedGen
     , [] <- storedVals
       -> do basicExecute "INSERT INTO thm_blockchain VALUES (0,0,?,?)"
               ( serialise (blockHash genesis)
               , serialise genesis
               )
             basicExecute "INSERT INTO thm_validators VALUES (1,?)"
               (Only (serialise initialVals))
     -- We have errors
     | _:_ <- checkResult
       -> error $ unlines $ "initializeBlockhainTables:" : checkResult
     -- Everything OK
     | [_]  <- storedGen
     , [_] <- storedVals
       -> return ()
     -- Error otherwise
     | otherwise
       -> error "initializeBlockhainTables: either only genesis or only validator set are present"


----------------------------------------------------------------
-- Public API
----------------------------------------------------------------

-- | Current height of blockchain (height of last commited block).
blockchainHeight :: MonadQueryRO m alg a => m Height
blockchainHeight =
  basicQuery_ "SELECT MAX(height) FROM thm_blockchain" >>= \case
    []       -> error "Blockchain cannot be empty"
    [Only h] -> return h
    _        -> error "Impossible"


-- | Retrieve block at given height.
--
--   Must return block for every height @0 <= h <= blockchainHeight@
retrieveBlock :: (Serialise a, Crypto alg, MonadQueryRO m alg a) => Height -> m (Maybe (Block alg a))
retrieveBlock height = liftQueryRO $ case height of
  Height 0 -> basicCacheGenesis $ query height
  _        -> basicCacheBlock     query height
  where
    query = singleQ "SELECT block FROM thm_blockchain WHERE height = ?" . Only

-- | Retrieve ID of block at given height. Must return same result
--   as @fmap blockHash . retrieveBlock@ but implementation could
--   do that more efficiently.
retrieveBlockID :: (MonadQueryRO m alg a) => Height -> m (Maybe (BlockID alg a))
retrieveBlockID h =
  singleQ "SELECT bid FROM thm_blockchain WHERE height = ?" (Only h)

-- | Retrieve commit justifying commit of block at height
--   @h@. Must return same result as @fmap blockLastCommit . retrieveBlock . next@
--   but do it more efficiently.
--
--   Note that this method returns @Nothing@ for last block since
--   its commit is not persisted in blockchain yet and there's no
--   commit for genesis block (h=0)
retrieveCommit :: (Serialise a, Crypto alg, MonadQueryRO m alg a) => Height -> m (Maybe (Commit alg a))
retrieveCommit h = do
  mb <- retrieveBlock $ succ h
  return $ blockLastCommit =<< mb

-- | Retrieve round when commit was made.
retrieveCommitRound :: (MonadQueryRO m alg a) => Height -> m (Maybe Round)
retrieveCommitRound h =
  basicQuery "SELECT round FROM thm_blockchain WHERE height = ?" (Only h) >>= \case
    []       -> return Nothing
    [Only r] -> return $! Just $ Round r
    _        -> error "Impossible"


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
retrieveLocalCommit :: (MonadQueryRO m alg a) => Height -> m (Maybe (Commit alg a))
retrieveLocalCommit h =
  singleQ "SELECT cmt FROM thm_commits WHERE height = ?" (Only h)

-- | Retrieve set of validators for given round.
--
--   Must return validator set for every @0 < h <= blockchainHeight + 1@
retrieveValidatorSet :: (Crypto alg, MonadQueryRO m alg a) => Height -> m (Maybe (ValidatorSet alg))
retrieveValidatorSet h =
  singleQ "SELECT valset FROM thm_validators WHERE height = ?" (Only h)

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
  :: (Crypto alg, Serialise a, MonadQueryRW m alg a)
  => Commit alg a -> Block alg a -> ValidatorSet alg -> m ()
storeCommit cmt blk vals = liftQueryRW $ do
  let h = headerHeight $ blockHeader blk
      r = voteRound $ signedValue $ NE.head $ commitPrecommits cmt
  basicExecute "INSERT INTO thm_commits VALUES (?,?)" (h, serialise cmt)
  basicExecute "INSERT INTO thm_blockchain VALUES (?,?,?,?)"
    ( h
    , r
    , serialise (blockHash blk)
    , serialise blk
    )
  basicExecute "INSERT INTO thm_validators VALUES (?,?)"
    (succ h, serialise vals)
  basicPutCacheBlock blk

-- | Write state snapshot into DB.
-- @maybeSnapshot@ contains a serialized value of a state associated with the processed block.
storeStateSnapshot :: (Serialise s, MonadQueryRW m alg a) => Height -> s -> m ()
storeStateSnapshot (Height h) state = do
  basicExecute "UPDATE state_snapshot SET height = ?, snapshot_blob = ?" (h, serialise state)

-- | Add message to Write Ahead Log. Height parameter is height
--   for which we're deciding block.
writeToWAL
  :: (Serialise a, Crypto alg, MonadQueryRW m alg a)
  => Height -> MessageRx 'Unverified alg a -> m ()
writeToWAL h msg =
  basicExecute "INSERT OR IGNORE INTO thm_wal VALUES (NULL,?,?)" (h, serialise msg)

-- | Remove all entries from WAL which comes from height less than
--   parameter.
resetWAL :: (MonadQueryRW m alg a) => Height -> m ()
resetWAL h =
  basicExecute "DELETE FROM thm_wal WHERE height < ?" (Only h)

-- | Get all parameters from WAL in order in which they were
--   written
readWAL
  :: (Serialise a, Crypto alg, MonadQueryRO m alg a)
  => Height -> m [MessageRx 'Unverified alg a]
readWAL h = do
  rows <- basicQuery "SELECT message FROM thm_wal WHERE height = ? ORDER BY id" (Only h)
  return [ case deserialiseOrFail bs of
             Right a -> a
             Left  e -> error ("CBOR encoding error: " ++ show e)
         | Only bs <- rows
         ]


-- Query that returns 0 or 1 result which is CBOR-encoded value
singleQ :: (SQL.ToRow p, Serialise x, MonadQueryRO m alg a)
        => SQL.Query -> p -> m (Maybe x)
singleQ sql p =
  basicQuery sql p >>= \case
    []        -> return Nothing
    [Only bs] -> case deserialiseOrFail bs of
      Right a -> return (Just a)
      Left  e -> error ("CBOR encoding error: " ++ show e)
    _         -> error "Impossible"

-- Query that returns results parsed from single row ().
singleQWithParser
  :: (SQL.ToRow p, MonadQueryRO m alg a)
  => ([SQL.SQLData] -> Maybe x) -> SQL.Query -> p -> m (Maybe x)
singleQWithParser resultsParser sql p =
  basicQuery sql p >>= \case
    [x] -> return (resultsParser x)
    _ -> error $ "SQL statement resulted in too many (>1) or zero result rows: " ++ show sql

