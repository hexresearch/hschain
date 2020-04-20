{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE MonoLocalBinds      #-}
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
  , storeBlock
  , retrieveBlock
  , storeState
  , retrieveState
  ) where

import Codec.Serialise     (Serialise, serialise, deserialiseOrFail)
import Control.Exception
--import Control.Monad       (when,(<=<))
--import Control.Monad.Catch (MonadThrow(..))
--import Data.Int
--import qualified Data.List.NonEmpty   as NE
import qualified Data.ByteString.Lazy as LBS
import qualified Database.SQLite.Simple           as SQL
import qualified Database.SQLite.Simple.FromField as SQL
import           Database.SQLite.Simple             (Only(..))

--import HSChain.PoW.Control (throwNothing)
import HSChain.PoW.Exceptions
import HSChain.PoW.Types
--import HSChain.Types.Merkle.Types
--import HSChain.Blockchain.Internal.Types
--import HSChain.Crypto
--import HSChain.Types.Validators
import HSChain.PoW.Store.Internal.Query

-- | Create tables for storing blockchain data
initializeBlockchainTables :: (MonadQueryRW m a) => m ()
initializeBlockchainTables = do
  -- Content addressable storage.
  --
  -- ID is used as primary key in order to make it possible to create
  -- foreign keys
  basicExecute_ $
      "CREATE TABLE IF NOT EXISTS blocks \
      \ ( block_id   BLOB PRIMARY KEY  \
      \ , block_blob BLOB NOT NULL )"

  -- Storage for genesis block.
  --
  -- That is a special case of block, must be single one.
  basicExecute_ $
      "CREATE TABLE IF NOT EXISTS genesis_block \
      \ ( block_id   BLOB PRIMARY KEY -- can be used for join purposes. \
      \ , block_blob BLOB NOT NULL )"

  -- The state of the blockchain. It is opaque for storage.
  --
  -- It is tied to one of the blocks in the chain, actually.
  basicExecute_ $
    "CREATE TABLE IF NOT EXISTS current_state \
    \ ( identifier INTEGER PRIMARY KEY -- must be 1 at all times \
    \ , snapshot   BLOB NOT NULL ) -- SELECT COUNT(*) must be 1 at all times."
  

storeBlock :: (Serialise (BlockID b), Serialise (Block b), BlockData b, MonadQueryRW m b)
           => Block b
           -> m ()
storeBlock blk = do
  basicExecute
    "INSERT INTO blocks (block_id, block_blob) VALUES (?, ?)"
      (CBORed $ blockID blk,CBORed blk)

retrieveBlock :: (Serialise (BlockID b), Serialise (Block b), MonadQueryRO m b)
              => BlockID b -> m (Maybe (Block b))
retrieveBlock blockId = do
  r <- singleQ "SELECT blob FROM thm_cas WHERE id = ?" (Only $ serialise blockId)
  case r of
    Nothing -> return Nothing
    Just b -> case deserialiseOrFail b of
                Right blk -> return $ Just blk
                Left _err -> throw DBInvalidBlock

-- | Retrieve height and state saved as snapshot.
retrieveState :: Serialise s => Query 'RO a (Maybe s)
retrieveState =
  singleQWithParser parse "SELECT snapshot FROM current_state WHERE identifier = 1" ()
  where
    parse [SQL.SQLBlob s]
      | Right r <- deserialiseOrFail (LBS.fromStrict s) = Just (r)
      | otherwise = Nothing
    parse _ = Nothing

-- | Write state snapshot into DB. @maybeSnapshot@ contains a
--  serialized value of a state associated with the processed block.
storeState :: (Serialise s, MonadQueryRW m a) => s -> m ()
storeState state = do
  basicExecute
    "UPDATE OR INSERT state_snapshot SET identifier = 1, snapshot_blob = ?"
     (Only $ CBORed state)


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

{-
----------------------------------------------------------------
--
----------------------------------------------------------------


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


-- Query that returns results parsed from single row ().
singleQWithParser
  :: (SQL.ToRow p, MonadQueryRO m a)
  => ([SQL.SQLData] -> Maybe x) -> SQL.Query -> p -> m (Maybe x)
singleQWithParser resultsParser sql p =
  basicQuery sql p >>= \case
    [x] -> return (resultsParser x)
    _ -> error $ "SQL statement resulted in too many (>1) or zero result rows: " ++ show sql
-}
