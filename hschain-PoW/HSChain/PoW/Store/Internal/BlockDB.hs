{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE MultiWayIf          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- |
-- Queries for interacting with database. Ones that constitute public
-- API are reexported from "HSChain.Store".
module HSChain.Store.Internal.BlockDB
  ( initializeBlockhainTables
  , storeGenesis
    -- * Fetching data
  , blockchainHeight
  , mustRetrieveCommitRound
  , retrieveBlockID
  , mustRetrieveBlockID
  , retrieveBlock
  , retrieveHeader
  , mustRetrieveBlock
  , retrieveValidatorSet
  , hasValidatorSet
  , mustRetrieveValidatorSet
  , retrieveLocalCommit
    -- * Storing blockchain
  , storeCommit
  , storeValSet
    -- * Evidence
  , storeFreshEvidence
  , storeBlockchainEvidence
  , evidenceRecordedState
  , retrieveUnrecordedEvidence
    -- * WAL
  , resetWAL
  , writeToWAL
  , readWAL
  , writeBlockReadyToWAL
  , retrieveBlockReadyFromWAL
  , writeBlockToWAL
  , retrieveBlockFromWAL
    -- * State snapshots
  , storeStateSnapshot
  , retrieveSavedState
  ) where

import Codec.Serialise     (Serialise, serialise, deserialiseOrFail)
import Control.Monad       (when,(<=<))
import Control.Monad.Catch (MonadThrow(..))
import Data.Int
import qualified Data.List.NonEmpty   as NE
import qualified Data.ByteString.Lazy as LBS
import qualified Database.SQLite.Simple           as SQL
import qualified Database.SQLite.Simple.FromField as SQL
import           Database.SQLite.Simple             (Only(..))

import HSChain.Control (throwNothing)
import HSChain.Exceptions
import HSChain.Types.Blockchain
import HSChain.Types.Merkle.Types
import HSChain.Blockchain.Internal.Types
import HSChain.Crypto
import HSChain.Types.Validators
import HSChain.Store.Internal.Query

----------------------------------------------------------------
--
----------------------------------------------------------------

-- | Create tables for storing blockchain data
initializeBlockhainTables :: (MonadQueryRW m a) => m ()
initializeBlockhainTables = do
  -- Content addressable storage.
  --
  -- ID is used as primary key in order to make it possible to create
  -- foreign keys
  basicExecute_
    "CREATE TABLE IF NOT EXISTS thm_cas \
    \ ( id   INTEGER PRIMARY KEY  \
    \ , hash BLOB NOT NULL UNIQUE \
    \ , blob BLOB NOT NULL )"
  -- Tables for blockchain data. Tables contains only references to
  -- CAS store
  basicExecute_
    "CREATE TABLE IF NOT EXISTS thm_blockchain \
    \ ( height    INTEGER NOT NULL UNIQUE \
    \ , round     INTEGER NOT NULL \
    \ , blockref  INTEGER NOT NULL \
    \ , commitref INTEGER \
    \ , FOREIGN KEY (blockref)  REFERENCES thm_cas(id) \
    \ , FOREIGN KEY (commitref) REFERENCES thm_cas(id))"
  basicExecute_
    "CREATE TABLE IF NOT EXISTS thm_validators \
    \  ( height INTEGER NOT NULL UNIQUE \
    \  , valref INTEGER NOT NULL\
    \  , FOREIGN KEY (valref) REFERENCES thm_cas(id))"
  -- Snapshots of blockchain state
  basicExecute_
    "CREATE TABLE IF NOT EXISTS state_snapshot \
    \  ( height           INT  NOT NULL \
    \  , snapshot_blob    BLOB NOT NULL)"
  r <- basicQuery_ "SELECT height FROM state_snapshot"
  when (null (r :: [[SQL.SQLData]])) $
    basicExecute_ "INSERT INTO state_snapshot (height, snapshot_blob) VALUES (-1, X'')"
  -- WAL
  --
  -- For PBFT we need to record all received messages which will be
  -- replayed if node crashes. We also need to store proposed blocks
  -- and results of ready-to-create-block check in order to replay
  -- deterministically.
  --
  -- ID field is used to keep order of messages. Primary key is
  -- incremented automatically and any new key will be larger than any
  -- existing key
  --
  -- Note UNIQUE constraint. Receiving identical message at the same
  -- height is noop so there's no point in storing them. Performance
  -- gains are massive since disk IO is _slow_.
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
  -- Evidence of byzantine behavior. We store every piece of evidence
  -- in this table with second field indicating whether evidence is
  -- recorded in blockchain or not
  basicExecute_
    "CREATE TABLE IF NOT EXISTS thm_evidence \
    \  ( evidence  BLOB    NOT NULL \
    \  , recorded  BOOLEAN NOT NULL \
    \  , UNIQUE (evidence))"


storeGenesis
  :: (Crypto (Alg a), CryptoHashable a, MonadQueryRW m a, Serialise a, Eq a, Show a)
  => Genesis a                -- ^ Genesis block
  -> m ()
storeGenesis BChEval{..} = do
  -- Insert genesis block if needed
  storedGen  <- retrieveBlock        (Height 0)
  storedVals <- retrieveValidatorSet (Height 0)
  --
  case (storedGen, storedVals) of
    -- Fresh DB
    (Nothing, Nothing) -> do
      storeCommitWrk Nothing    bchValue
      storeValSet   (Height 0)  validatorSet
    -- Otherwise check that stored and provided geneses match
    (Just genesis', Just initialVals') ->
      case checks of
        [] -> return ()
        _  -> error $ unlines $ "initializeBlockhainTables:" : concat checks
      where
        checks = [ [ "Genesis blocks do not match:"
                   , "  stored: " ++ show genesis'
                   , "  expected: " ++ show bchValue
                   ]
                 | bchValue /= genesis'
                 ]
                 ++
                 [ [ "Validators set are not equal:"
                   , "  stored:   " ++ show initialVals'
                   , "  expected: " ++ show (merkleValue validatorSet)
                   ]
                 | merkleValue validatorSet /= initialVals'
                 ]
    --
    (_,_) -> error "initializeBlockhainTables: database corruption"


----------------------------------------------------------------
-- Low-level CAS API
----------------------------------------------------------------

storeBlob
  :: (Serialise b)
  => MerkleNode IdNode (Alg a) b
  -> Query 'RW a Int64
storeBlob x = do
  basicQuery "SELECT id FROM thm_cas WHERE hash = ?" (Only (CBORed h)) >>= \case
    []       -> do basicExecute "INSERT INTO thm_cas VALUES (NULL,?,?)"
                     (CBORed h, CBORed v)
                   basicLastInsertRowId
    [Only i] -> return i
    _        -> error "Impossible"
  where
    h = merkleHash  x
    v = merkleValue x

retrieveBlobByHash
  :: (Serialise b, CryptoHashable b, CryptoHash (Alg a))
  => MerkleNode Hashed (Alg a) b
  -> Query 'RO a (Maybe (MerkleNode IdNode (Alg a) b))
retrieveBlobByHash x = do
  r <- singleQ "SELECT blob FROM thm_cas WHERE hash = ?" (Only $ CBORed $ merkleHashed x)
  return $ merkled <$> r

mustRetrieveBlobByHash
  :: (Serialise b, CryptoHashable b, CryptoHash (Alg a))
  => MerkleNode Hashed (Alg a) b
  -> Query 'RO a (MerkleNode IdNode (Alg a) b)
mustRetrieveBlobByHash = throwNothing DBMissingBlob <=< retrieveBlobByHash

retrieveBlobByID
  :: (Serialise b, CryptoHashable b, CryptoHash (Alg a))
  => Int64
  -> Query 'RO a (Maybe (MerkleNode IdNode (Alg a) b))
retrieveBlobByID i = do
  r <- singleQ "SELECT blob FROM thm_cas WHERE id = ?" (Only i)
  return $ merkled <$> r

mustRetrieveBlobByID
  :: (Serialise b, CryptoHashable b, CryptoHash (Alg a))
  => Int64
  -> Query 'RO a (MerkleNode IdNode (Alg a) b)
mustRetrieveBlobByID = throwNothing DBMissingBlob <=< retrieveBlobByID


----------------------------------------------------------------
-- Public API
----------------------------------------------------------------

-- | Current height of blockchain (height of last commited block).
blockchainHeight :: MonadQueryRO m a => m Height
blockchainHeight =
  basicQuery_ "SELECT MAX(height) FROM thm_blockchain" >>= \case
    []       -> error "Blockchain cannot be empty"
    [Only h] -> return h
    _        -> error "Impossible"

-- | Retrieve ID of block at given height. Must return same result
--   as @fmap blockHash . retrieveBlock@ but implementation could
--   do that more efficiently.
retrieveBlockID :: (MonadQueryRO m a) => Height -> m (Maybe (BlockID a))
retrieveBlockID h
  = fmap (fmap BlockID)
  $ singleQ "SELECT hash \
            \  FROM thm_blockchain \
            \  JOIN thm_cas ON blockref = id \
            \ WHERE height = ?" (Only h)

mustRetrieveBlockID :: (MonadThrow m, MonadQueryRO m a) => Height -> m (BlockID a)
mustRetrieveBlockID h = throwNothing (DBMissingBlockID h) =<< retrieveBlockID h

-- | Retrieve only block header for block at given height
retrieveHeader
  :: (Serialise a, CryptoHashable a, Crypto (Alg a), MonadQueryRO m a)
  => Height -> m (Maybe (Header a))
retrieveHeader h =
  singleQ "SELECT blob \
          \  FROM thm_blockchain \
          \  JOIN thm_cas ON blockref = id \
          \ WHERE height = ?" (Only h)

-- | Retrieve block at given height.
retrieveBlock
  :: forall m a. (Serialise a, CryptoHashable a, Crypto (Alg a), MonadQueryRO m a)
  => Height -> m (Maybe (Block a))
retrieveBlock height = liftQueryRO $ case height of
  Height 0 -> basicCacheGenesis $ query height
  _        -> basicCacheBlock     query height
  where
    -- We now need to fetch every field that is abbreviated here
    query :: Height -> Query 'RO a (Maybe (Block a))
    query h =
      basicQuery "SELECT blockref FROM thm_blockchain WHERE height = ?" (Only h) >>= \case
        [] -> return Nothing
        -- All references values MUST be in database
        [Only i] -> do
          header :: Header a <- merkleValue <$> mustRetrieveBlobByID i
          ev  <- mustRetrieveBlobByHash $ blockEvidence header
          dat <- mustRetrieveBlobByHash $ blockData     header
          cmt <- mapM mustRetrieveBlobByHash $ blockPrevCommit header
          return $ Just $ header { blockEvidence   = ev
                                 , blockData       = dat
                                 , blockPrevCommit = cmt
                                 }
        _ -> error "Impossible"

-- | Retrieve round when commit was made.
mustRetrieveCommitRound :: (MonadThrow m, MonadQueryRO m a) => Height -> m Round
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
retrieveLocalCommit :: (MonadQueryRO m a) => Height -> m (Maybe (Commit a))
retrieveLocalCommit h =
  singleQ "SELECT blob \
          \  FROM thm_blockchain \
          \  JOIN thm_cas ON commitref = id \
          \ WHERE height = ?" (Only h)

-- | Retrieve set of validators for given round.
--
--   Must return validator set for every @0 < h <= blockchainHeight + 1@
retrieveValidatorSet :: (Crypto (Alg a), MonadQueryRO m a) => Height -> m (Maybe (ValidatorSet (Alg a)))
retrieveValidatorSet h =
  singleQ "SELECT blob \
          \  FROM thm_validators \
          \  JOIN thm_cas ON valref = id \
          \ WHERE height = ?" (Only h)

hasValidatorSet :: (MonadQueryRO m a) => Height -> m Bool
hasValidatorSet h = do
  r <- basicQuery "SELECT 1 FROM thm_validators WHERE height = ?" (Only h)
  return $! not $ null (r :: [Only Int])

-- | Same as 'retrieveBlock' but throws 'DBMissingBlock' if there's no
--   such block in database.
mustRetrieveBlock
  :: (Serialise a, CryptoHashable a, Crypto (Alg a), MonadThrow m, MonadQueryRO m a)
  => Height -> m (Block a)
mustRetrieveBlock h
  = throwNothing (DBMissingBlock h) =<< retrieveBlock h

-- | Same as 'retrieveValidatorSet' but throws 'DBMissingValSet' if
--   there's no validator set in database.
mustRetrieveValidatorSet
  :: (Crypto (Alg a), MonadQueryRO m a, MonadThrow m)
  => Height -> m (ValidatorSet (Alg a))
mustRetrieveValidatorSet h
  = throwNothing (DBMissingValSet h) =<< retrieveValidatorSet h




----------------------------------------------------------------
-- State snapshots
----------------------------------------------------------------

-- | Retrieve height and state saved as snapshot.
retrieveSavedState :: Serialise s => Query 'RO a (Maybe (Height, s))
retrieveSavedState =
  singleQWithParser parse "SELECT height, snapshot_blob FROM state_snapshot" ()
  where
    parse [SQL.SQLInteger h, SQL.SQLBlob s]
      | h > 0
      , Right r <- deserialiseOrFail (LBS.fromStrict s) = Just (Height $ fromIntegral h, r)
      | otherwise = Nothing
    parse _ = Nothing

-- | Write state snapshot into DB. @maybeSnapshot@ contains a
--  serialized value of a state associated with the processed block.
storeStateSnapshot :: (Serialise s, MonadQueryRW m a) => Height -> s -> m ()
storeStateSnapshot (Height h) state = do
  basicExecute "UPDATE state_snapshot SET height = ?, snapshot_blob = ?" (h, serialise state)


----------------------------------------------------------------
-- Writing to DB
----------------------------------------------------------------

-- | Write block and commit justifying it into persistent storage.
storeCommit
  :: (Crypto (Alg a), Serialise a, CryptoHashable a, MonadQueryRW m a)
  => Commit a -> Block a -> m ()
storeCommit = storeCommitWrk . Just

storeCommitWrk
  :: (Crypto (Alg a), Serialise a, CryptoHashable a, MonadQueryRW m a)
  => Maybe (Commit a) -> Block a -> m ()
storeCommitWrk mcmt blk = liftQueryRW $ do
  let h = blockHeight blk
      r = case mcmt of
        Just cmt -> voteRound $ signedValue $ NE.head $ commitPrecommits cmt
        Nothing  -> Round 0
  _     <- storeBlob $ blockEvidence blk
  _     <- storeBlob $ blockData     blk
  _     <- mapM_ storeBlob $ blockPrevCommit blk
  iBlk  <- storeBlob $ merkled $ toHeader blk
  miCmt <- mapM (storeBlob . merkled) mcmt
  basicExecute "INSERT INTO thm_blockchain VALUES (?,?,?,?)"
    ( h, r, iBlk, miCmt )
  basicPutCacheBlock blk

storeValSet
  :: (Crypto (Alg a), MonadQueryRW m a)
  => Height -> MerkleNode IdNode (Alg a) (ValidatorSet (Alg a)) -> m ()
storeValSet h vals = liftQueryRW $ do
  i <- storeBlob vals
  basicExecute "INSERT INTO thm_validators VALUES (?,?)" (h, i)


----------------------------------------------------------------
-- WAL
----------------------------------------------------------------

-- | Add message to Write Ahead Log. Height parameter is height
--   for which we're deciding block.
writeToWAL
  :: (Serialise a, CryptoHashable a, Crypto (Alg a), MonadQueryRW m a)
  => Height -> MessageRx 'Unverified a -> m ()
writeToWAL h msg =
  basicExecute "INSERT OR IGNORE INTO thm_wal VALUES (NULL,?,?)" (h, serialise msg)

writeBlockToWAL
  :: (MonadQueryRW m a, Serialise a, CryptoHashable a, Crypto (Alg a))
  => Round -> Block a -> m ()
writeBlockToWAL r b = do
  basicExecute "INSERT INTO thm_proposals VALUES (?,?,?)"
    (h,r,serialise b)
  where
    h = blockHeight b

retrieveBlockFromWAL
  :: (MonadQueryRO m a, Serialise a, CryptoHashable a, Crypto (Alg a))
  => Height -> Round -> m (Maybe (Block a))
retrieveBlockFromWAL h r =
  singleQ "SELECT block FROM thm_proposals WHERE height = ? AND round = ?" (h,r)

writeBlockReadyToWAL
  :: (MonadQueryRW m a)
  => Height -> Int -> Bool -> m ()
writeBlockReadyToWAL h n can =
  basicExecute "INSERT INTO thm_ready_block VALUES (?,?,?)" (h,n,can)

retrieveBlockReadyFromWAL
  :: (MonadQueryRO m a)
  => Height -> Int -> m (Maybe Bool)
retrieveBlockReadyFromWAL h n =
  query1 "SELECT result FROM thm_ready_block WHERE height = ? AND attempt = ?" (h,n)

-- | Remove all entries from WAL which comes from height less than
--   parameter.
resetWAL :: (MonadQueryRW m a) => Height -> m ()
resetWAL h = do
  basicExecute "DELETE FROM thm_wal         WHERE height < ?" (Only h)
  basicExecute "DELETE FROM thm_proposals   WHERE height < ?" (Only h)
  basicExecute "DELETE FROM thm_ready_block WHERE height < ?" (Only h)

-- | Get all parameters from WAL in order in which they were
--   written
readWAL
  :: (Serialise a, CryptoHashable a, Crypto (Alg a), MonadQueryRO m a)
  => Height -> m [MessageRx 'Unverified a]
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
  :: (MonadQueryRW m a)
  => ByzantineEvidence a -> m ()
storeFreshEvidence ev = do
  basicExecute "INSERT OR IGNORE INTO thm_evidence VALUES (?,?)" (serialise ev, False)

-- | Store evidence from blockchain. That is valid evidence from
--   commited block
storeBlockchainEvidence
  :: (MonadQueryRW m a)
  => ByzantineEvidence a -> m ()
storeBlockchainEvidence ev = do
  basicExecute "INSERT OR REPLACE INTO thm_evidence VALUES (?,?)" (serialise ev, True)

-- | Check whether evidence is recorded. @Just True@ mens that it's
--   already in blockchain
evidenceRecordedState
  :: (MonadQueryRO m a)
  => ByzantineEvidence a -> m (Maybe Bool)
evidenceRecordedState e = do
  basicQuery "SELECT recorded FROM thm_evidence WHERE evidence = ?" (Only (serialise e)) >>= \case
    []       -> return Nothing
    [Only x] -> return (Just x)
    _        -> error "impossible"

-- | Retrieve all unrecorded evidence
retrieveUnrecordedEvidence
  :: (MonadQueryRO m a)
  => m [ByzantineEvidence a]
retrieveUnrecordedEvidence = do
  rs <- basicQuery_ "SELECT evidence FROM thm_evidence WHERE recorded = 0"
  return $ unCBORed . fromOnly <$> rs


----------------------------------------------------------------
-- Helpers
----------------------------------------------------------------

query1 :: (SQL.ToRow p, SQL.FromField x, MonadQueryRO m a)
             => SQL.Query -> p -> m (Maybe x)
query1 sql p =
  basicQuery sql p >>= \case
    []       -> return Nothing
    [Only a] -> return (Just a)
    _        -> error "Impossible"

-- Query that returns 0 or 1 result which is CBOR-encoded value
singleQ :: (SQL.ToRow p, Serialise x, MonadQueryRO m a)
        => SQL.Query -> p -> m (Maybe x)
singleQ sql p = (fmap . fmap) unCBORed
              $ query1 sql p

-- Query that returns results parsed from single row ().
singleQWithParser
  :: (SQL.ToRow p, MonadQueryRO m a)
  => ([SQL.SQLData] -> Maybe x) -> SQL.Query -> p -> m (Maybe x)
singleQWithParser resultsParser sql p =
  basicQuery sql p >>= \case
    [x] -> return (resultsParser x)
    _ -> error $ "SQL statement resulted in too many (>1) or zero result rows: " ++ show sql
