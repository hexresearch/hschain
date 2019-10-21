{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE MultiWayIf          #-}
{-# LANGUAGE OverloadedStrings   #-}
-- |
-- Queries for interacting with database. Ones that constitute public
-- API are reexported from "HSChain.Store".
module HSChain.Store.Internal.BlockDB where

import Codec.Serialise     (Serialise, serialise, deserialiseOrFail)
import Control.Monad       (when)
import Control.Monad.Catch (MonadThrow(..))
import qualified Data.List.NonEmpty   as NE
import qualified Data.ByteString.Lazy as LBS
import qualified Database.SQLite.Simple           as SQL
import           Database.SQLite.Simple             (Only(..))

import HSChain.Control (throwNothing)
import HSChain.Exceptions
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
  -- WAL
  --
  -- ID field is used to keep order of messages. Primary key is
  -- incremented automatically and any new key will be larger than any
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
  basicExecute_
    "CREATE TABLE IF NOT EXISTS thm_proposals \
    \ ( height INTEGER NOT NULL \
    \ , round  INTEGER NOT NULL \
    \ , block  BLOB    NOT NULL \
    \ , UNIQUE (height,round))"
  basicExecute_
    "CREATE TABLE IF NOT EXISTS thm_ready_block \
    \ ( height  INTEGER NOT NULL \
    \ , attempt INTEGER NOT NULL \
    \ , result  BOOLEAN NOT NULL \
    \ , UNIQUE (height, attempt))"
  -- Byzantine evidence. We store every piece of evidence in this
  -- table with second field indicating whether evidence is recorded
  -- in blockchain
  basicExecute_
    "CREATE TABLE IF NOT EXISTS thm_evidence \
    \  ( evidence  BLOB    NOT NULL \
    \  , recorded  BOOLEAN NOT NULL \
    \  , UNIQUE (evidence))"
  -- Insert genesis block if needed
  storedGen  <- singleQ_ "SELECT block  FROM thm_blockchain WHERE height = 0"
  storedVals <- singleQ_ "SELECT valset FROM thm_validators WHERE height = 1"
  let initialVals = case changeValidators (blockValChange genesis) emptyValidatorSet of
        Just v  -> v
        Nothing -> error "initializeBlockhainTables: cannot apply change of validators"
  case (storedGen, storedVals) of
    -- Fresh DB
    (Nothing, Nothing) -> do
      basicExecute "INSERT INTO thm_blockchain VALUES (0,0,?,?)"
        ( serialise (blockHash genesis)
        , serialise genesis
        )
      basicExecute "INSERT INTO thm_validators VALUES (1,?)"
        (Only (serialise initialVals))
    -- Otherwise check that stored and provided geneses match
    (Just genesis', Just initialVals') ->
      case checks of
        [] -> return ()
        _  -> error $ unlines $ "initializeBlockhainTables:" : concat checks
      where
        checks = [ [ "Genesis blocks do not match:"
                   , "  stored: " ++ show genesis'
                   , "  expected: " ++ show genesis
                   ]
                 | genesis /= genesis'
                 ]
                 ++
                 [ [ "Validators set are not equal:"
                   , "  stored:   " ++ show initialVals'
                   , "  expected: " ++ show initialVals
                   ]
                 | initialVals /= initialVals'
                 ]
    --
    (_,_) -> error "initializeBlockhainTables: database corruption"


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

mustRetrieveBlockID :: (MonadThrow m, MonadQueryRO m alg a) => Height -> m (BlockID alg a)
mustRetrieveBlockID h = throwNothing (DBMissingBlockID h) =<< retrieveBlockID h

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
mustRetrieveCommitRound :: (MonadThrow m, MonadQueryRO m alg a) => Height -> m Round
mustRetrieveCommitRound h =
  basicQuery "SELECT round FROM thm_blockchain WHERE height = ?" (Only h) >>= \case
    []       -> throwM $ DBMissingRound h
    [Only r] -> return $! Round r
    _        -> error "Impossible"


-- | Retrieve local commit justifying commit of block as known by
--   node at moment of the commit. Implementation only MUST store
--   commit for the last block but may choose to store earlier
--   commits as well.
--
--   Note that commits returned by this functions may differ
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

-- | Same as 'retrieveBlock' but throws 'DBMissingBlock' if there's no
--   such block in database.
mustRetrieveBlock
  :: (Serialise a, Crypto alg, MonadThrow m, MonadQueryRO m alg a)
  => Height -> m (Block alg a)
mustRetrieveBlock h
  = throwNothing (DBMissingBlock h) =<< retrieveBlock h

-- | Same as 'retrieveValidatorSet' but throws 'DBMissingValSet' if
--   there's no validator set in database.
mustRetrieveValidatorSet
  :: (Crypto alg, MonadQueryRO m alg a, MonadThrow m)
  => Height -> m (ValidatorSet alg)
mustRetrieveValidatorSet h
  = throwNothing (DBMissingValSet h) =<< retrieveValidatorSet h


-- | Retrieve height and state saved as snapshot.
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


----------------------------------------------------------------
-- WAL
----------------------------------------------------------------

-- | Add message to Write Ahead Log. Height parameter is height
--   for which we're deciding block.
writeToWAL
  :: (Serialise a, Crypto alg, MonadQueryRW m alg a)
  => Height -> MessageRx 'Unverified alg a -> m ()
writeToWAL h msg =
  basicExecute "INSERT OR IGNORE INTO thm_wal VALUES (NULL,?,?)" (h, serialise msg)

writeBlockToWAL
  :: (MonadQueryRW m alg a, Serialise a, Crypto alg)
  => Round -> Block alg a -> m ()
writeBlockToWAL r b = do
  basicExecute "INSERT INTO thm_proposals VALUES (?,?,?)"
    (h,r,serialise b)
  where
    h = headerHeight $ blockHeader b

retrieveBlockFromWAL
  :: (MonadQueryRO m alg a, Serialise a, Crypto alg)
  => Height -> Round -> m (Maybe (Block alg a))
retrieveBlockFromWAL h r =
  singleQ "SELECT block FROM thm_proposals WHERE height = ? AND round = ?" (h,r)

writeBlockReadyToWAL
  :: (MonadQueryRW m alg a)
  => Height -> Int -> Bool -> m ()
writeBlockReadyToWAL h n can =
  basicExecute "INSERT INTO thm_ready_block VALUES (?,?,?)" (h,n,can)

retrieveBlockReadyFromWAL
  :: (MonadQueryRO m alg a)
  => Height -> Int -> m (Maybe Bool)
retrieveBlockReadyFromWAL h n =
  singleQ "SELECT result FROM thm_ready_block WHERE height = ? AND attempt = ?" (h,n)

-- | Remove all entries from WAL which comes from height less than
--   parameter.
resetWAL :: (MonadQueryRW m alg a) => Height -> m ()
resetWAL h = do
  basicExecute "DELETE FROM thm_wal         WHERE height < ?" (Only h)
  basicExecute "DELETE FROM thm_proposals   WHERE height < ?" (Only h)
  basicExecute "DELETE FROM thm_ready_block WHERE height < ?" (Only h)

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

----------------------------------------------------------------
-- Evidence
----------------------------------------------------------------

-- | Store fresh evidence in database. Fresh means we just observed it.
storeFreshEvidence
  :: (Serialise a, Crypto alg, MonadQueryRW m alg a)
  => ByzantineEvidence alg a -> m ()
storeFreshEvidence ev = do
  basicExecute "INSERT OR IGNORE INTO thm_evidence VALUES (?,?)" (serialise ev, False)

-- | Store evidence from blockchain. That is valid evidence from
--   commited block
storeBlockchainEvidence
  :: (Serialise a, Crypto alg, MonadQueryRW m alg a)
  => ByzantineEvidence alg a -> m ()
storeBlockchainEvidence ev = do
  basicExecute "INSERT OR REPLACE INTO thm_evidence VALUES (?,?)" (serialise ev, True)

-- | Check whether evidence is recorded. @Just True@ mens that it's
--   already in blockchain
evidenceRecordedState
  :: (Serialise a, Crypto alg, MonadQueryRO m alg a)
  => ByzantineEvidence alg a -> m (Maybe Bool)
evidenceRecordedState e = do
  basicQuery "SELECT recorded FROM thm_evidence WHERE evidence = ?" (Only (serialise e)) >>= \case
    []       -> return Nothing
    [Only x] -> return (Just x)
    _        -> error "impossible"

-- | Retrieve all unrecorded evidence
retrieveUnrecordedEvidence
  :: (Serialise a, Crypto alg, MonadQueryRO m alg a)
  => m [ByzantineEvidence alg a]
retrieveUnrecordedEvidence = do
  rs <- basicQuery_ "SELECT evidence FROM thm_evidence WHERE recorded = 0"
  return $ unCBORed . fromOnly <$> rs


----------------------------------------------------------------
-- Helpers
----------------------------------------------------------------

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

singleQ_ :: (Serialise x, MonadQueryRO m alg a)
         => SQL.Query -> m (Maybe x)
singleQ_ sql =
  basicQuery_ sql >>= \case
    []        -> return Nothing
    [Only bs] -> case deserialiseOrFail bs of
      Right a -> return (Just a)
      Left  e -> error ("CBOR encoding error: " ++ show e)
    _         -> error "Impossible"
