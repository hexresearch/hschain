{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
-- |
-- Abstract API for storing of blockchain. Storage works as follows:
--
--  * Blockchain is stored in some databases (on in memory for testing
--    purposes)
--
--  * Rounds' state is kept in memory so it will be lost in case of
--    crash but incoming messages are stored in write ahead log so
--    they could be replayed.
module Thundermint.Store (
    -- * Standard DB wrapper
    DBT(..)
  , dbtRO
  , runDBT
    -- * Monadic API for DB access
  , Access(..)
  , MonadReadDB(..)
  , MonadDB(..)
  , Query
  , Connection
  , connectionRO
  , queryRO
  , queryRW
    -- ** Opening database
  , openConnection
  , closeConnection
  , withConnection
  , initDatabase
  , withDatabase
    -- * Block storage
  , blockchainHeight
  , retrieveBlock
  , retrieveBlockID
  , retrieveCommit
  , retrieveLocalCommit
  , retrieveCommitRound
  , retrieveValidatorSet
    -- * In memory store for proposals
  , Writable
  , ProposalStorage(..)
  , hoistPropStorageRW
  , hoistPropStorageRO
  , makeReadOnlyPS
    -- * Mempool
  , MempoolCursor(..)
  , Mempool(..)
  , MempoolInfo(..)
  , hoistMempoolCursor
  , hoistMempool
  , nullMempool
  , nullMempoolAny
  -- * Blockchain invariants checkers
  , BlockchainInconsistency
  , checkStorage
  , checkProposedBlock
  ) where

import qualified Katip

import Codec.Serialise           (Serialise)
import Control.Monad             ((<=<), foldM, forM)
import Control.Monad.Catch       (MonadMask,MonadThrow,MonadCatch)
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Writer
import Data.Foldable             (forM_)
import Data.Maybe                (isNothing, maybe)
import Data.Text                 (isPrefixOf, Text)
import Data.List                 (nub)
import GHC.Generics              (Generic)

import qualified Data.Aeson      as JSON
import qualified Data.Aeson.TH   as JSON
import qualified Data.ByteString as BS

import Thundermint.Types.Blockchain
import Thundermint.Blockchain.Internal.Types
import Thundermint.Control                (MonadFork,FloatOut(..),foldF)
import Thundermint.Crypto
import Thundermint.Crypto.Containers
import Thundermint.Debug.Trace
import Thundermint.Logger                 (MonadLogger)
import Thundermint.Store.Internal.Query
import Thundermint.Store.Internal.BlockDB
import Thundermint.Store.SQL
import Thundermint.Types.Validators


----------------------------------------------------------------
-- Monadic API for DB access
----------------------------------------------------------------

newtype DBT rw alg a m x = DBT (ReaderT (Connection rw alg a) m x)
  deriving ( Functor, Applicative, Monad
           , MonadIO, MonadThrow, MonadCatch, MonadMask
           , MonadFork, MonadLogger, MonadTrace
           )

instance MonadTrans (DBT rw alg a) where
  lift = DBT . lift

dbtRO :: DBT 'RO alg a m x -> DBT rw alg a m x
dbtRO (DBT m) = DBT (withReaderT connectionRO m)

runDBT :: Monad m => Connection rw alg a -> DBT rw alg a m x -> m x
runDBT c (DBT m) = runReaderT m c

instance MonadIO m => MonadReadDB (DBT rw alg a m) alg a where
  askConnectionRO = connectionRO <$> DBT ask
instance MonadIO m => MonadDB (DBT 'RW alg a m) alg a where
  askConnectionRW = DBT ask


-- | Helper function which opens database, initializes it and ensures
--   that it's closed on function exit
withDatabase
  :: (MonadIO m, MonadMask m, FloatOut dct, Crypto alg, Serialise a, Eq a, Eq (PublicKey alg))
  => FilePath         -- ^ Path to the database
  -> dct Persistent   -- ^ Users state. If no state is stored in the
  -> Block alg a      -- ^ Genesis block
  -> ValidatorSet alg -- ^ Initial validators
  -> (Connection 'RW alg a -> m x) -> m x
withDatabase path dct genesis vals cont
  = withConnection path $ \c -> initDatabase c dct genesis vals >> cont c

-- | Initialize all required tables in database.
initDatabase
  :: (MonadIO m, FloatOut dct, Crypto alg, Serialise a, Eq a, Eq (PublicKey alg))
  => Connection 'RW alg a  -- ^ Opened connection to database
  -> dct Persistent        -- ^ Users state. If no state is stored in the
  -> Block alg a           -- ^ Genesis block
  -> ValidatorSet alg      -- ^ Initial validators
  -> m ()
initDatabase c dct genesis vals = do
  -- 1. Check that all tables has distinct names
  let names = foldF ((:[]) . persistentTableName) dct
  case () of
    _| names /= nub names               -> error "Duplicate table names"
     | any (=="wal")              names -> error "'wal' is not acceptable table name"
     | any (=="blockchain")       names -> error "'blockchain' is not acceptable table name"
     | any (=="commit")           names -> error "'commit' is not acceptable table name"
     | any (=="validators")       names -> error "'validators' is not acceptable table name"
     | any (isPrefixOf "thm_")    names -> error "'thm_' is not acceptable prefix for table"
     | any (isPrefixOf "sqlite_") names -> error "'sqlite_' is not acceptable prefix for table"
     | otherwise                        -> return ()
  -- 2. Create tables for block
  r <- runQueryRW c $ do
    initializeBlockhainTables genesis vals
    traverseEff persistentCreateTable dct
  case r of
    -- FIXME: Resource leak!
    Nothing -> error "Cannot initialize tables!"
    Just () -> return ()

-- | Execute query.
queryRO :: (MonadReadDB m alg a) => Query 'RO alg a x -> m x
queryRO q = flip runQueryRO q =<< askConnectionRO

-- | Execute query. @Nothing@ means that query violated some invariant
--   and was rolled back.
queryRW :: (MonadDB m alg a) => Query 'RW alg a x -> m (Maybe x)
queryRW q = flip runQueryRW q =<< askConnectionRW



----------------------------------------------------------------
-- Storage for consensus
----------------------------------------------------------------

type family Writable (rw :: Access) a where
  Writable 'RO a = ()
  Writable 'RW a = a


-- | Storage for proposed blocks that are not commited yet.
data ProposalStorage rw m alg a = ProposalStorage
  { currentHeight      :: !(m Height)
    -- ^ Height for which we store proposed blocks
  , retrievePropByID   :: !(Height -> BlockID alg a -> m (Maybe (Block alg a)))
    -- ^ Retrieve proposed block by its ID
  , retrievePropByR    :: !(Height -> Round -> m (Maybe (Block alg a, BlockID alg a)))
    -- ^ Retrieve proposed block by round number.

  , advanceToHeight    :: !(Writable rw (Height -> m ()))
    -- ^ Advance to given height. If height is different from current
    --   all stored data is discarded
  , allowBlockID       :: !(Writable rw (Round -> BlockID alg a -> m ()))
    -- ^ Mark block ID as one that we could accept
  , storePropBlock     :: !(Writable rw (Block alg a -> m ()))
    -- ^ Store block proposed at given height. If height is different
    --   from height we are at block is ignored.
  }

makeReadOnlyPS :: ProposalStorage rw m alg a -> ProposalStorage 'RO m alg a
makeReadOnlyPS ProposalStorage{..} =
  ProposalStorage { advanceToHeight = ()
                  , storePropBlock  = ()
                  , allowBlockID    = ()
                  , ..
                  }

hoistPropStorageRW
  :: (forall x. m x -> n x)
  -> ProposalStorage 'RW m alg a
  -> ProposalStorage 'RW n alg a
hoistPropStorageRW fun ProposalStorage{..} =
  ProposalStorage { currentHeight      = fun currentHeight
                  , retrievePropByID   = \h x -> fun (retrievePropByID h x)
                  , retrievePropByR    = \h x -> fun (retrievePropByR  h x)
                  , advanceToHeight    = fun . advanceToHeight
                  , storePropBlock     = fun . storePropBlock
                  , allowBlockID       = \r bid -> fun (allowBlockID r bid)
                  }

hoistPropStorageRO
  :: (forall x. m x -> n x)
  -> ProposalStorage 'RO m alg a
  -> ProposalStorage 'RO n alg a
hoistPropStorageRO fun ProposalStorage{..} =
  ProposalStorage { currentHeight      = fun currentHeight
                  , retrievePropByID   = \h x -> fun (retrievePropByID h x)
                  , retrievePropByR    = \h x -> fun (retrievePropByR  h x)
                  , ..
                  }

----------------------------------------------------------------
--
----------------------------------------------------------------

-- | Statistics about mempool
data MempoolInfo = MempoolInfo
  { mempool'size      :: !Int
  -- ^ Number of transactions currently in mempool
  , mempool'added     :: !Int
  -- ^ Number of transactions added to mempool since program start
  , mempool'discarded :: !Int
  -- ^ Number of transaction discarded immediately since program start
  , mempool'filtered  :: !Int
  -- ^ Number of transaction removed during filtering
  }
  deriving (Show,Generic)
JSON.deriveJSON JSON.defaultOptions
  { JSON.fieldLabelModifier = drop 8 } ''MempoolInfo

instance Katip.ToObject MempoolInfo
instance Katip.LogItem  MempoolInfo where
  payloadKeys Katip.V0 _ = Katip.SomeKeys []
  payloadKeys _        _ = Katip.AllKeys

-- | Cursor into mempool which is used for gossiping data
data MempoolCursor m alg tx = MempoolCursor
  { pushTransaction :: !(tx -> m (Maybe (Hash alg)))
    -- ^ Add transaction to the mempool. It's preliminary checked and
    --   if check fails it immediately discarded. If transaction is
    --   accepted its hash is computed and returned
  , advanceCursor   :: !(m (Maybe tx))
    -- ^ Take transaction from front and advance cursor. If cursor
    -- points at the end of queue nothing happens.
  }

-- | Mempool which is used for storing transactions before they're
--   added into blockchain. Transactions are stored in FIFO manner
data Mempool m alg tx = Mempool
  { peekNTransactions :: !(Maybe Int -> m [tx])
    -- ^ Take up to N transactions from mempool. If Nothing is passed
    --   that all transactions will be returned. This operation does
    --   not alter mempool state
  , filterMempool     :: !(m ())
    -- ^ Remove transactions that are no longer valid from mempool
  , getMempoolCursor  :: !(m (MempoolCursor m alg tx))
    -- ^ Get cursor pointing to be
  , txInMempool       :: !(Hash alg -> m Bool)
    -- ^ Checks whether transaction is mempool
  , mempoolStats      :: !(m MempoolInfo)
    -- ^ Number of elements in mempool
  , mempoolSize       :: !(m Int)
    -- ^ Number of transactions in mempool
  , mempoolSelfTest   :: !(m [String])
    -- ^ Check mempool for internal consistency. Each returned string
    --   is internal inconsistency
  }

hoistMempoolCursor :: (forall a. m a -> n a) -> MempoolCursor m alg tx -> MempoolCursor n alg tx
hoistMempoolCursor fun MempoolCursor{..} = MempoolCursor
  { pushTransaction = fun . pushTransaction
  , advanceCursor   = fun advanceCursor
  }

hoistMempool :: Functor n => (forall a. m a -> n a) -> Mempool m alg tx -> Mempool n alg tx
hoistMempool fun Mempool{..} = Mempool
  { peekNTransactions = fun . peekNTransactions
  , filterMempool     = fun filterMempool
  , getMempoolCursor  = hoistMempoolCursor fun <$> fun getMempoolCursor
  , txInMempool       = fun . txInMempool
  , mempoolStats      = fun mempoolStats
  , mempoolSize       = fun mempoolSize
  , mempoolSelfTest   = fun mempoolSelfTest
  }


nullMempoolAny :: (Monad m) => Mempool m alg tx
nullMempoolAny = Mempool
  { peekNTransactions = const (return [])
  , filterMempool     = return ()
  , mempoolStats      = return $ MempoolInfo 0 0 0 0
  , mempoolSize       = return 0
  , txInMempool       = const (return False)
  , getMempoolCursor  = return MempoolCursor
      { pushTransaction = const $ return Nothing
      , advanceCursor   = return Nothing
      }
  , mempoolSelfTest   = return []
  }

nullMempool :: (Monad m) => Mempool m alg ()
nullMempool = nullMempoolAny

----------------------------------------------------------------
-- validate blockchain invariants
----------------------------------------------------------------

-- | Blockchain inconsistency types
data BlockchainInconsistency
  = MissingBlock        !Height
  -- ^ Missing block at given height.
  | MissingLocalCommit  !Height
  -- ^ Commit justifying commit of block at height H is missing
  | MissingValidatorSet !Height
  -- ^ Validator set for block at height H is missing

  | BlockWrongChainID  !Height
  -- ^ Block at height H has blockchain ID different from genesis block
  | BlockHeightMismatch !Height
  -- ^ Height in header of block at height H is not equal to H
  | BlockValidatorHashMismatch !Height
  -- ^ Hash of set of validators in block header does not match hash
  --   of actual set
  | HeaderHashMismatch !Height !Text
  -- ^ Hash of field of block in header and actual hash do not match.
  | BlockInvalidPrevBID !Height
  -- ^ Previous block is does not match ID of block commited to
  --   blockchain.
  | GenesisHasLastCommit
  -- ^ Genesis block has last commit field set.
  | GenesisHasHeaderLastBlockID
  -- ^ Block ID of previous block is set in genesis block
  | FirstBlockHasLastCommit
  -- ^ Block at H=1 has last commit field set.
  | BlockMissingLastCommit !Height
  -- ^ Block with H>1 last commit field is @Nothing@
  | InvalidCommit Height !String
  -- ^ Commit embedded into block at height H (it justifies commit of
  --   block H-1) is invalid for some reason
  | InvalidLocalCommit !Height !String
  -- ^ Commit which justified commit of block at height H is invalid
  --   for some reason.
  | BlockInvalidTime !Height
  -- ^ Block contains invalid time in header
  deriving (Eq, Show)


-------------------------------------------------------------------------------
-- Invariant checking for storage and proposed blocks
-------------------------------------------------------------------------------

-- | check storage against all consistency invariants
checkStorage
  :: (MonadReadDB m alg a, Crypto alg, Serialise a)
  => m [BlockchainInconsistency]
checkStorage = queryRO $ execWriterT $ do
  maxH         <- lift $ blockchainHeight
  Just genesis <- lift $ retrieveBlock (Height 0)
  let genesisChainId = headerChainID $ blockHeader genesis
  --
  forM_ [Height 0 .. maxH] $ \case
    Height 0 -> genesisBlockInvariant genesis
    h        -> checkRequire $ do
      -- Block, ID and validator set must be present
      block  <- require [MissingBlock h]        $ retrieveBlock        h
      bid    <- require [MissingBlock h]        $ retrieveBlockID      h
      vset   <- require [MissingValidatorSet h] $ retrieveValidatorSet h
      commit <- require [MissingLocalCommit h]  $ retrieveLocalCommit  h
      -- Data for previous step.
      --
      -- NOTE: We don't show any errors for missing BID for previous
      --       block since we detected when checking for previous height
      mprevVset <- lift $ lift $ retrieveValidatorSet (pred h)
      prevBID   <- require [] $ retrieveBlockID (pred h)
      prevB     <- require [] $ retrieveBlock   (pred h)
      --
      lift $ blockInvariant
        genesisChainId h (headerTime $ blockHeader prevB) prevBID (mprevVset,vset) block
      lift $ commitInvariant (InvalidLocalCommit h)
        h (headerTime $ blockHeader prevB) bid vset commit


-- | Check that block proposed at given height is correct in sense all
--   blockchain invariants hold
checkProposedBlock
  :: (MonadReadDB m alg a, Crypto alg, Serialise a)
  => Height
  -> Block alg a
  -> m [BlockchainInconsistency]
checkProposedBlock h block = queryRO $ do
  Just genesis <- retrieveBlock        (Height 0)
  Just prevB   <- retrieveBlock        (pred h)
  Just prevBID <- retrieveBlockID      (pred h)
  Just vset    <- retrieveValidatorSet  h
  mprevVset    <- retrieveValidatorSet (pred h)
  execWriterT $ blockInvariant
    (headerChainID $ blockHeader genesis)
    h
    (headerTime $ blockHeader prevB)
    prevBID
    (mprevVset,vset)
    block





-- | Check invariants for genesis block
genesisBlockInvariant
  :: Monad m
  => Block alg a
  -> WriterT [BlockchainInconsistency] m ()
genesisBlockInvariant Block{blockHeader = Header{..}, ..} = do
  -- It must have height 0
  (headerHeight == Height 0)
    `orElse` BlockHeightMismatch (Height 0)
  -- Last commit it must be Nothing
  isNothing blockLastCommit
    `orElse` GenesisHasLastCommit
  -- Last block must be Nothing
  isNothing headerLastBlockID
    `orElse` GenesisHasHeaderLastBlockID


-- | Check invariant for block at height > 0
blockInvariant
  :: (Monad m, Crypto alg, Serialise a)
  => BS.ByteString
  -- ^ Blockchain ID
  -> Height
  -- ^ Height of block
  -> Time
  -- ^ Time of previous block
  -> BlockID alg a
  -- ^ Block ID of previous block
  -> (Maybe (ValidatorSet alg), ValidatorSet alg)
  -- ^ Validator set for previous and current height
  -> Block alg a
  -- ^ Block to check
  -> WriterT [BlockchainInconsistency] m ()
blockInvariant _ h _ _ _ _
  | h <= Height 0 = error "blockInvariant called with invalid parameters"
blockInvariant chainID h prevT prevBID (mprevValSet, valSet) Block{blockHeader=Header{..}, ..} = do
  -- All blocks must have same chain ID, i.e. chain ID of
  -- genesis block
  (chainID == headerChainID)
    `orElse` BlockWrongChainID h
  -- Block at height H has H in its header
  (headerHeight == h)
    `orElse` BlockHeightMismatch h
  -- Block time is calculated correctly
  case headerHeight of
    Height 1 -> (headerTime > prevT) `orElse` BlockInvalidTime h
    _        -> do let commitT = do vals <- mprevValSet
                                    commitTime vals prevT =<< blockLastCommit
                   case commitT of
                     Nothing -> tell [BlockInvalidTime h]
                     Just t  -> do (t == headerTime) `orElse` BlockInvalidTime h
                                   (t >= prevT     ) `orElse` BlockInvalidTime h
  -- Previous block ID in header must match actual BID of previous
  -- block
  (headerLastBlockID == Just prevBID)
    `orElse` BlockInvalidPrevBID h
  -- Validators' hash does not match correct one
  (headerValidatorsHash == hash valSet)
    `orElse` BlockValidatorHashMismatch h
  -- Hashes of block fields are correct
  (headerDataHash == hashed blockData)
    `orElse` HeaderHashMismatch h "Data"
  (headerValChangeHash == hashed blockValChange)
    `orElse` HeaderHashMismatch h "Validator change"
  (headerLastCommitHash == hashed blockLastCommit)
    `orElse` HeaderHashMismatch h "Commit hash"
  (headerEvidenceHash == hashed blockEvidence)
    `orElse` HeaderHashMismatch h "Evidence"
  -- Block time must be equal to commit time
  -- Validate commit of previous block
  case (headerHeight, blockLastCommit) of
    -- Last commit at H=1 must be Nothing
    (Height 1, Nothing) -> return ()
    (Height 1, Just _ ) -> tell [FirstBlockHasLastCommit]
    -- Otherwise commit must be present and valid
    (_, Nothing       ) -> tell [BlockMissingLastCommit h]
    (_, Just commit   )
      | Just prevValSet <- mprevValSet
        -> commitInvariant (InvalidCommit h) (pred h) prevT prevBID prevValSet commit
      | otherwise
        -> tell [InvalidCommit h "Cannot validate commit"]

commitInvariant
  :: (Monad m, Crypto alg)
  => (String -> BlockchainInconsistency) -- Error constructor
  -> Height                              -- Height of block for
  -> Time                                -- Time of previous block
  -> BlockID alg a
  -> ValidatorSet alg
  -> Commit alg a
  -> WriterT [BlockchainInconsistency] m ()
commitInvariant mkErr h prevT bid valSet Commit{..} = do
  -- It must justify commit of correct block!
  (commitBlockID == bid)
    `orElse` mkErr "Commit is for wrong block"
  -- All votes should be for previous height
  do let invalid = [ svote
                   | svote <- commitPrecommits
                   , h /= voteHeight (signedValue svote)
                   ]
     null invalid
       `orElse` mkErr "Commit contains votes for invalid height"
  -- All votes must be in same round
  do let rounds = voteRound . signedValue <$> commitPrecommits
     case rounds of
       r:rs | all (==r) rs -> return ()
       _                   -> tell [mkErr "Commit contains votes for different rounds"]
  -- Commit has enough (+2/3) voting power, doesn't have votes
  -- from unknown validators, doesn't have duplicate votes, and
  -- vote goes for correct block
  let verifiedPrecommits = forM commitPrecommits $ \v ->
        verifySignature (fmap validatorPubKey . validatorByAddr valSet) v
  case verifiedPrecommits of
    Nothing   -> tell [mkErr "Commit contains invalid signatures"]
    Just sigs -> do
      let mvoteSet = foldM
            (flip insertSigned)
            (newVoteSet valSet prevT)
            sigs
      case mvoteSet of
        InsertConflict _ -> tell [mkErr "Conflicting votes"]
        InsertDup        -> tell [mkErr "Duplicate votes"]
        InsertUnknown  _ -> tell [mkErr "Votes from unknown source"]
        InsertOK vset    -> case majority23 vset of
          Nothing        -> tell [mkErr "Commit doesn't have 2/3+ majority"]
          Just b
            | b == Just bid -> return ()
            | otherwise
              -> tell [mkErr "2/3+ majority for wrong block"]


checkRequire
  :: Monad m
  => ExceptT [BlockchainInconsistency] (WriterT [BlockchainInconsistency] m) ()
  -> WriterT [BlockchainInconsistency] m ()
checkRequire m = runExceptT m >>= \case
  Left  e  -> tell e
  Right () -> return ()

require :: (Monad m, Monoid w) => e -> m (Maybe a) -> ExceptT e (WriterT w m) a
require e = maybe (throwE e) return <=< lift . lift

orElse :: Monad m => Bool -> e -> WriterT [e] m ()
orElse True  _ = return ()
orElse False e = tell [e]
