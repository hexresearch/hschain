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
  ) where

import Codec.Serialise     (Serialise, serialise, deserialiseOrFail)
import Control.Monad       ((<=<))
import Control.Monad.Catch (MonadThrow(..))
import Data.Int
import qualified Data.List.NonEmpty   as NE
import qualified Database.SQLite.Simple           as SQL
import qualified Database.SQLite.Simple.FromField as SQL
import           Database.SQLite.Simple             (Only(..))

import HSChain.Control.Util (throwNothing)
import HSChain.Crypto
import HSChain.Exceptions
import HSChain.Internal.Types.Messages
import HSChain.Internal.Types.Consensus
import HSChain.Store.Internal.Query
import HSChain.Types.Blockchain
import HSChain.Types.Merkle.Types
import HSChain.Types.Validators

----------------------------------------------------------------
--
----------------------------------------------------------------

-- | Create tables for storing blockchain data
initializeBlockhainTables :: Query 'RW a ()
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
  :: (Crypto (Alg a), CryptoHashable a, Serialise a, Eq a, Show a)
  => Genesis a                -- ^ Genesis block
  -> Query 'RW a ()
storeGenesis Genesis{..} = do
  -- Insert genesis block if needed
  storedGen  <- retrieveBlock        (Height 0)
  storedVals <- retrieveValidatorSet (Height 0)
  --
  case (storedGen, storedVals) of
    -- Fresh DB
    (Nothing, Nothing) -> do
      storeCommitWrk Nothing    genesisBlock
      storeValSet   (Height 0)  (merkled genesisValSet)
    -- Otherwise check that stored and provided geneses match
    (Just genesis, Just initialVals) ->
      case checks of
        [] -> return ()
        _  -> error $ unlines $ "initializeBlockhainTables:" : concat checks
      where
        checks = [ [ "Genesis blocks do not match:"
                   , "  stored:   " ++ show genesis
                   , "  expected: " ++ show genesisBlock
                   ]
                 | genesisBlock /= genesis
                 ]
                 ++
                 [ [ "Validators set are not equal:"
                   , "  stored:   " ++ show initialVals
                   , "  expected: " ++ show genesisValSet
                   ]
                 | genesisValSet /= initialVals
                 ]
    --
    (_,_) -> error "initializeBlockhainTables: database corruption"


----------------------------------------------------------------
-- Low-level CAS API
----------------------------------------------------------------

storeBlob
  :: (Serialise b)
  => MerkleNode Identity (Alg a) b
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
  => MerkleNode Proxy (Alg a) b
  -> Query rw a (Maybe (MerkleNode Identity (Alg a) b))
retrieveBlobByHash x = do
  r <- singleQ "SELECT blob FROM thm_cas WHERE hash = ?" (Only $ CBORed $ merkleHashed x)
  return $ merkled <$> r

mustRetrieveBlobByHash
  :: (Serialise b, CryptoHashable b, CryptoHash (Alg a))
  => MerkleNode Proxy (Alg a) b
  -> Query rw a (MerkleNode Identity (Alg a) b)
mustRetrieveBlobByHash = throwNothing DBMissingBlob <=< retrieveBlobByHash

retrieveBlobByID
  :: (Serialise b, CryptoHashable b, CryptoHash (Alg a))
  => Int64
  -> Query rw a (Maybe (MerkleNode Identity (Alg a) b))
retrieveBlobByID i = do
  r <- singleQ "SELECT blob FROM thm_cas WHERE id = ?" (Only i)
  return $ merkled <$> r

mustRetrieveBlobByID
  :: (Serialise b, CryptoHashable b, CryptoHash (Alg a))
  => Int64
  -> Query rw a (MerkleNode Identity (Alg a) b)
mustRetrieveBlobByID = throwNothing DBMissingBlob <=< retrieveBlobByID


----------------------------------------------------------------
-- Public API
----------------------------------------------------------------

-- | Current height of blockchain (height of last commited block).
blockchainHeight :: Query rw a Height
blockchainHeight =
  basicQuery_ "SELECT MAX(height) FROM thm_blockchain" >>= \case
    []       -> error "Blockchain cannot be empty"
    [Only h] -> return h
    _        -> error "Impossible"

-- | Retrieve ID of block at given height. Must return same result
--   as @fmap blockHash . retrieveBlock@ but implementation could
--   do that more efficiently.
retrieveBlockID :: Height -> Query rw a (Maybe (BlockID a))
retrieveBlockID h
  = fmap (fmap BlockID)
  $ singleQ "SELECT hash \
            \  FROM thm_blockchain \
            \  JOIN thm_cas ON blockref = id \
            \ WHERE height = ?" (Only h)

mustRetrieveBlockID :: Height -> Query rw a (BlockID a)
mustRetrieveBlockID h = throwNothing (DBMissingBlockID h) =<< retrieveBlockID h

-- | Retrieve only block header for block at given height
retrieveHeader
  :: (Serialise a, CryptoHashable a, Crypto (Alg a))
  => Height -> Query rw a (Maybe (Header a))
retrieveHeader h =
  singleQ "SELECT blob \
          \  FROM thm_blockchain \
          \  JOIN thm_cas ON blockref = id \
          \ WHERE height = ?" (Only h)

-- | Retrieve block at given height.
retrieveBlock
  :: forall rw a. (Serialise a, CryptoHashable a, Crypto (Alg a))
  => Height -> Query rw a (Maybe (Block a))
retrieveBlock height = case height of
  Height 0 -> basicCacheGenesis $ query height
  _        -> basicCacheBlock     query height
  where
    -- We now need to fetch every field that is abbreviated here
    query :: Height -> Query rw a (Maybe (Block a))
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
mustRetrieveCommitRound :: Height -> Query rw a Round
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
retrieveLocalCommit :: Height -> Query rw a (Maybe (Commit a))
retrieveLocalCommit h =
  singleQ "SELECT blob \
          \  FROM thm_blockchain \
          \  JOIN thm_cas ON commitref = id \
          \ WHERE height = ?" (Only h)

-- | Retrieve set of validators for given round.
--
--   Must return validator set for every @0 < h <= blockchainHeight + 1@
retrieveValidatorSet :: (Crypto (Alg a)) => Height -> Query rw a (Maybe (ValidatorSet (Alg a)))
retrieveValidatorSet = basicCacheValidatorSet query
  where
    query h = singleQ "SELECT blob \
          \  FROM thm_validators \
          \  JOIN thm_cas ON valref = id \
          \ WHERE height = ?" (Only h)

hasValidatorSet :: Height -> Query rw a Bool
hasValidatorSet h = do
  r <- basicQuery "SELECT 1 FROM thm_validators WHERE height = ?" (Only h)
  return $! not $ null (r :: [Only Int])

-- | Same as 'retrieveBlock' but throws 'DBMissingBlock' if there's no
--   such block in database.
mustRetrieveBlock
  :: (Serialise a, CryptoHashable a, Crypto (Alg a))
  => Height -> Query rw a (Block a)
mustRetrieveBlock h
  = throwNothing (DBMissingBlock h) =<< retrieveBlock h

-- | Same as 'retrieveValidatorSet' but throws 'DBMissingValSet' if
--   there's no validator set in database.
mustRetrieveValidatorSet
  :: (Crypto (Alg a))
  => Height -> Query rw a (ValidatorSet (Alg a))
mustRetrieveValidatorSet h
  = throwNothing (DBMissingValSet h) =<< retrieveValidatorSet h




----------------------------------------------------------------
-- Writing to DB
----------------------------------------------------------------

-- | Write block and commit justifying it into persistent storage.
storeCommit
  :: (Crypto (Alg a), Serialise a, CryptoHashable a)
  => Commit a -> Block a -> Query 'RW a ()
storeCommit = storeCommitWrk . Just

storeCommitWrk
  :: (Crypto (Alg a), Serialise a, CryptoHashable a)
  => Maybe (Commit a) -> Block a -> Query 'RW a ()
storeCommitWrk mcmt blk = do
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
  :: (Crypto (Alg a))
  => Height
  -> MerkleNode Identity (Alg a) (ValidatorSet (Alg a))
  -> Query 'RW a ()
storeValSet h vals = do
  i <- storeBlob vals
  basicExecute "INSERT INTO thm_validators VALUES (?,?)" (h, i)
  basicPutValidatorSet h (merkleValue vals)


----------------------------------------------------------------
-- WAL
----------------------------------------------------------------

-- | Add message to Write Ahead Log. Height parameter is height
--   for which we're deciding block.
writeToWAL
  :: (Serialise a, CryptoHashable a, Crypto (Alg a))
  => Height -> MessageRx 'Unverified a -> Query 'RW a ()
writeToWAL h msg =
  basicExecute "INSERT OR IGNORE INTO thm_wal VALUES (NULL,?,?)" (h, serialise msg)

writeBlockToWAL
  :: (Serialise a, CryptoHashable a, Crypto (Alg a))
  => Round -> Block a -> Query 'RW a ()
writeBlockToWAL r b = do
  basicExecute "INSERT INTO thm_proposals VALUES (?,?,?)"
    (h,r,serialise b)
  where
    h = blockHeight b

retrieveBlockFromWAL
  :: (Serialise a, CryptoHashable a, Crypto (Alg a))
  => Height -> Round -> Query rw a (Maybe (Block a))
retrieveBlockFromWAL h r =
  singleQ "SELECT block FROM thm_proposals WHERE height = ? AND round = ?" (h,r)

writeBlockReadyToWAL
  :: Height -> Int -> Bool -> Query 'RW a ()
writeBlockReadyToWAL h n can =
  basicExecute "INSERT INTO thm_ready_block VALUES (?,?,?)" (h,n,can)

retrieveBlockReadyFromWAL
  :: Height -> Int -> Query rw a (Maybe Bool)
retrieveBlockReadyFromWAL h n =
  query1 "SELECT result FROM thm_ready_block WHERE height = ? AND attempt = ?" (h,n)

-- | Remove all entries from WAL which comes from height less than
--   parameter.
resetWAL :: Height -> Query 'RW a ()
resetWAL h = do
  basicExecute "DELETE FROM thm_wal         WHERE height < ?" (Only h)
  basicExecute "DELETE FROM thm_proposals   WHERE height < ?" (Only h)
  basicExecute "DELETE FROM thm_ready_block WHERE height < ?" (Only h)

-- | Get all parameters from WAL in order in which they were
--   written
readWAL
  :: (Serialise a, CryptoHashable a, Crypto (Alg a))
  => Height -> Query rw a [MessageRx 'Unverified a]
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
storeFreshEvidence :: ByzantineEvidence a -> Query 'RW a ()
storeFreshEvidence ev = do
  basicExecute "INSERT OR IGNORE INTO thm_evidence VALUES (?,?)" (serialise ev, False)

-- | Store evidence from blockchain. That is valid evidence from
--   commited block
storeBlockchainEvidence :: ByzantineEvidence a -> Query 'RW a ()
storeBlockchainEvidence ev = do
  basicExecute "INSERT OR REPLACE INTO thm_evidence VALUES (?,?)" (serialise ev, True)

-- | Check whether evidence is recorded. @Just True@ mens that it's
--   already in blockchain
evidenceRecordedState :: ByzantineEvidence a -> Query rw a (Maybe Bool)
evidenceRecordedState e = do
  basicQuery "SELECT recorded FROM thm_evidence WHERE evidence = ?" (Only (serialise e)) >>= \case
    []       -> return Nothing
    [Only x] -> return (Just x)
    _        -> error "impossible"

-- | Retrieve all unrecorded evidence
retrieveUnrecordedEvidence
  :: Query rw a [ByzantineEvidence a]
retrieveUnrecordedEvidence = do
  rs <- basicQuery_ "SELECT evidence FROM thm_evidence WHERE recorded = 0"
  return $ unCBORed . fromOnly <$> rs


----------------------------------------------------------------
-- Helpers
----------------------------------------------------------------

query1 :: (SQL.ToRow p, SQL.FromField x, MonadQueryRO m)
       => SQL.Query -> p -> m (Maybe x)
query1 sql p =
  basicQuery sql p >>= \case
    []       -> return Nothing
    [Only a] -> return (Just a)
    _        -> error "Impossible"

-- Query that returns 0 or 1 result which is CBOR-encoded value
singleQ :: (SQL.ToRow p, Serialise x, MonadQueryRO m)
        => SQL.Query -> p -> m (Maybe x)
singleQ sql p = (fmap . fmap) unCBORed
              $ query1 sql p
