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
module HSChain.PoW.Store.Internal.BlockDB
  ( initializeBlockchainTables
  ) where

--import Codec.Serialise     (Serialise, serialise, deserialiseOrFail)
--import Control.Monad       (when,(<=<))
--import Control.Monad.Catch (MonadThrow(..))
--import Data.Int
--import qualified Data.List.NonEmpty   as NE
--import qualified Data.ByteString.Lazy as LBS
--import qualified Database.SQLite.Simple           as SQL
--import qualified Database.SQLite.Simple.FromField as SQL
--import           Database.SQLite.Simple             (Only(..))

--import HSChain.PoW.Control (throwNothing)
--import HSChain.PoW.Exceptions
--import HSChain.PoW.Types
--import HSChain.Types.Merkle.Types
--import HSChain.Blockchain.Internal.Types
--import HSChain.Crypto
--import HSChain.Types.Validators
import HSChain.PoW.Store.Internal.Query

-- | Create tables for storing blockchain data
initializeBlockchainTables :: (MonadQueryRW m a) => m ()
initializeBlockchainTables = return ()
{-
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
  :: (MonadQueryRW m a, Serialise a, Eq a, Show a)
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

mustRetrieveBlockID :: (MonadThrow m, MonadQueryRO m a) => Height -> m (BlockID a)
mustRetrieveBlockID h = throwNothing (DBMissingBlockID h) =<< retrieveBlockID h

-- | Retrieve only block header for block at given height
retrieveHeader
  :: (Serialise (Block a), MonadQueryRO m a)
  => Height -> m (Maybe (Header a))
retrieveHeader h =
  singleQ "SELECT blob \
          \  FROM thm_blockchain \
          \  JOIN thm_cas ON blockref = id \
          \ WHERE height = ?" (Only h)

-- | Retrieve block at given height.
retrieveBlock
  :: forall m a. (Serialise (Block a), MonadQueryRO m a)
  => Hash -> m (Maybe (Block a))
retrieveBlock blockId = liftQueryRO $ case height of
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
          return $ Just $         _ -> error "Impossible"


-- | Same as 'retrieveBlock' but throws 'DBMissingBlock' if there's no
--   such block in database.
mustRetrieveBlock
  :: (Serialise (Block a), MonadThrow m, MonadQueryRO m a)
  => Height -> m (Block a)
mustRetrieveBlock h
  = throwNothing (DBMissingBlock h) =<< retrieveBlock h

----------------------------------------------------------------
-- Writing to DB
----------------------------------------------------------------


----------------------------------------------------------------
-- WAL
----------------------------------------------------------------

writeBlockToWAL
  :: (MonadQueryRW m a, Serialise (Block a))
  => Block a -> m ()
writeBlockToWAL r b = do
  basicExecute "INSERT INTO thm_proposals VALUES (?,?,?)"
    (h,r,serialise b)
  where
    h = blockHeight b

retrieveBlockFromWAL
  :: (MonadQueryRO m a, Serialise (Block a))
  => Height -> m (Maybe (Block a))
retrieveBlockFromWAL h r =
  singleQ "SELECT block FROM thm_proposals WHERE height = ? AND round = ?" (h,r)

-- | Remove all entries from WAL which comes from height less than
--   parameter.
resetWAL :: (MonadQueryRW m a) => Height -> m ()
resetWAL h = do
  basicExecute "DELETE FROM thm_wal         WHERE height < ?" (Only h)
  basicExecute "DELETE FROM thm_proposals   WHERE height < ?" (Only h)
  basicExecute "DELETE FROM thm_ready_block WHERE height < ?" (Only h)


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
-}
