{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE MultiWayIf                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE ViewPatterns               #-}
-- |
-- This module provides API for working with persistent (blockchain)
-- and no so persistent (mempool) storage.
module HSChain.Store (
    -- * Working with database
    -- ** Connection
    Connection
  , Access(..)
  , connectionRO
  , MonadReadDB(..)
  , MonadDB(..)
  , queryRO
  , queryRW
  , mustQueryRW
  , queryROT
  , queryRWT
  , mustQueryRWT
    -- ** Opening\/closing database
  , openConnection
  , closeConnection
  , withConnection
  , initDatabase
  , withDatabase
    -- * Querying database
    -- ** Query monads
  , MonadQueryRO(..)
  , MonadQueryRW(..)
  , Query
  , QueryT
    -- ** Standard DB wrapper
  , DBT(..)
  , dbtRO
  , runDBT
  , DatabaseByField(..)
  , DatabaseByType(..)
    -- ** Standard API
  , blockchainHeight
  , retrieveBlock
  , retrieveBlockID
  , retrieveLocalCommit
  , retrieveValidatorSet
  , mustRetrieveBlock
  , mustRetrieveValidatorSet
  , retrieveSavedState
  , storeStateSnapshot
    -- * Blockchain state
  , BChStore(..)
    -- * Blockchain invariants checkers
  , BlockchainInconsistency
  , checkStorage
  , checkProposedBlock
  ) where

import Codec.Serialise           (Serialise)
import Control.Monad.Catch       (MonadMask,MonadThrow,MonadCatch)
import Control.Monad.Morph       (MFunctor(..))
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Control.Monad.Reader
import Control.Monad.Trans.Writer
#if !MIN_VERSION_base(4,13,0)
import Control.Monad.Fail         (MonadFail)
#endif

import Data.Maybe                (isNothing)
import Data.Text                 (Text)
import Data.Generics.Product.Fields (HasField'(..))
import Data.Generics.Product.Typed  (HasType(..))
import qualified Data.List.NonEmpty as NE
import Lens.Micro

import HSChain.Types.Blockchain
import HSChain.Types.Merkle.Types
import HSChain.Blockchain.Internal.Types
import HSChain.Control.Class          (MonadFork)
import HSChain.Crypto
import HSChain.Crypto.Containers
import HSChain.Logger                 (MonadLogger)
import HSChain.Store.Internal.Query
import HSChain.Store.Internal.BlockDB
import HSChain.Types.Validators

----------------------------------------------------------------
-- Monadic API for DB access
----------------------------------------------------------------

-- | Monad transformer which provides 'MonadReadDB' and 'MonadDB'
--   instances.
newtype DBT rw a m x = DBT (ReaderT (Connection rw a) m x)
  deriving ( Functor, Applicative, Monad
           , MonadIO, MonadThrow, MonadCatch, MonadMask
           , MonadFork, MonadLogger, MonadFail
           )

instance MFunctor (DBT rw a) where
  hoist f (DBT m) = DBT $ hoist f m

instance MonadTrans (DBT rw a) where
  lift = DBT . lift

-- | Lift monad which provides read-only access to read-write access.
dbtRO :: DBT 'RO a m x -> DBT rw a m x
dbtRO (DBT m) = DBT (withReaderT connectionRO m)

runDBT :: Connection rw a -> DBT rw a m x -> m x
runDBT c (DBT m) = runReaderT m c

instance MonadIO m => MonadReadDB a (DBT rw a m) where
  askConnectionRO = connectionRO <$> DBT ask
instance MonadIO m => MonadDB a (DBT 'RW a m) where
  askConnectionRW = DBT ask


-- | Helper function which opens database, initializes it and ensures
--   that it's closed on function exit
withDatabase
  :: (MonadIO m, MonadMask m)
  => FilePath         -- ^ Path to the database
  -> (Connection 'RW a -> m x) -> m x
withDatabase path cont
  = withConnection path $ \c -> initDatabase c >> cont c

-- | Initialize all required tables in database.
initDatabase
  :: (MonadIO m)
  => Connection 'RW a  -- ^ Opened connection to database
  -> m ()
initDatabase c = do
  r <- runQueryRW c initializeBlockhainTables
  case r of
    Nothing -> error "Cannot initialize tables!"
    Just () -> return ()






----------------------------------------------------------------
-- Blockchain state storage
----------------------------------------------------------------

-- | Storage for blockchain state.
data BChStore m a = BChStore
  { bchCurrentState  :: m (Maybe Height, MerkleNode Identity (Alg a) (BlockchainState a))
  -- ^ Height of value stored in state
  , bchStoreRetrieve :: Height -> m (Maybe (MerkleNode Identity (Alg a) (BlockchainState a)))
  -- ^ Retrieve state for given height. It's generally not expected that
  , bchStoreStore    :: Height -> MerkleNode Identity (Alg a) (BlockchainState a) -> m ()
  -- ^ Put blockchain state at given height into store
  }

instance HoistDict BChStore where
  hoistDict fun BChStore{..} = BChStore
    { bchCurrentState  = fun   bchCurrentState
    , bchStoreRetrieve = fun . bchStoreRetrieve
    , bchStoreStore    = (fmap . fmap) fun bchStoreStore
    }


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
  :: (MonadReadDB a m, MonadIO m, Crypto (Alg a), Serialise a, CryptoHashable a)
  => m [BlockchainInconsistency]
checkStorage = queryRO $ execWriterT $ do
  maxH         <- lift $ blockchainHeight
  Just genesis <- lift $ retrieveBlock (Height 0)
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
      --
      lift $ blockInvariant h prevBID (mprevVset,vset) block
      lift $ commitInvariant (InvalidLocalCommit h) h bid vset commit


-- | Check that block proposed at given height is correct in sense all
--   blockchain invariants hold
checkProposedBlock
  :: (MonadReadDB a m, MonadIO m, Crypto (Alg a))
  => Height
  -> Block a
  -> m [BlockchainInconsistency]
checkProposedBlock h block = queryRO $ do
  Just prevBID <- retrieveBlockID      (pred h)
  Just vset    <- retrieveValidatorSet  h
  mprevVset    <- retrieveValidatorSet (pred h)
  execWriterT $ blockInvariant
    h
    prevBID
    (mprevVset,vset)
    block




-- | Check invariants for genesis block
genesisBlockInvariant
  :: (Monad m)
  => Block a
  -> WriterT [BlockchainInconsistency] m ()
genesisBlockInvariant Block{..} = do
  -- It must have height 0
  (blockHeight == Height 0)
    `orElse` BlockHeightMismatch (Height 0)
  -- Last commit it must be Nothing
  isNothing blockPrevCommit
    `orElse` GenesisHasLastCommit
  -- Last block must be Nothing
  isNothing blockPrevBlockID
    `orElse` GenesisHasHeaderLastBlockID


-- | Check invariant for block at height > 0
blockInvariant
  :: (Monad m, Crypto (Alg a))
  => Height
  -- ^ Height of block
  -> BlockID a
  -- ^ Block ID of previous block
  -> (Maybe (ValidatorSet (Alg a)), ValidatorSet (Alg a))
  -- ^ Validator set for previous and current height
  -> Block a
  -- ^ Block to check
  -> WriterT [BlockchainInconsistency] m ()
blockInvariant h _ _ _
  | h <= Height 0 = error "blockInvariant called with invalid parameters"
blockInvariant h prevBID (mprevValSet, valSet) Block{..} = do
  -- Block at height H has H in its header
  (blockHeight == h)
    `orElse` BlockHeightMismatch h
  -- Previous block ID in header must match actual BID of previous
  -- block
  (blockPrevBlockID == Just prevBID)
    `orElse` BlockInvalidPrevBID h
  -- Validators' hash does not match correct one
  (blockValidators == hashed valSet)
    `orElse` BlockValidatorHashMismatch h
  -- Validate commit of previous block
  case (blockHeight, blockPrevCommit) of
    -- Last commit at H=1 must be Nothing
    (Height 1, Nothing) -> return ()
    (Height 1, Just _ ) -> tell [FirstBlockHasLastCommit]
    -- Otherwise commit must be present and valid
    (_, Nothing       ) -> tell [BlockMissingLastCommit h]
    (_, Just commit   )
      | Just prevValSet <- mprevValSet
        -> commitInvariant (InvalidCommit h) (pred h) prevBID prevValSet (merkleValue commit)
      | otherwise
        -> tell [InvalidCommit h "Cannot validate commit"]

commitInvariant
  :: (Monad m, Crypto (Alg a))
  => (String -> BlockchainInconsistency) -- Error constructor
  -> Height                              -- Height of block for
  -> BlockID a
  -> ValidatorSet (Alg a)
  -> Commit a
  -> WriterT [BlockchainInconsistency] m ()
commitInvariant mkErr h bid valSet Commit{..} = do
  -- It must justify commit of correct block!
  (commitBlockID == bid)
    `orElse` mkErr "Commit is for wrong block"
  -- All votes should be for previous height
  do let invalid = [ svote
                   | svote <- NE.toList commitPrecommits
                   , h /= voteHeight (signedValue svote)
                   ]
     null invalid
       `orElse` mkErr "Commit contains votes for invalid height"
  -- All votes must be in same round
  do let r NE.:| rs = voteRound . signedValue <$> commitPrecommits
     unless (all (==r) rs)
       $ tell [mkErr "Commit contains votes for different rounds"]
  -- Commit has enough (+2/3) voting power, doesn't have votes
  -- from unknown validators, doesn't have duplicate votes, and
  -- vote goes for correct block
  let verifiedPrecommits = forM commitPrecommits $ verifySignature valSet
  case verifiedPrecommits of
    Nothing   -> tell [mkErr "Commit contains invalid signatures"]
    Just sigs -> do
      let mvoteSet = foldM
            (flip insertSigned)
            (newVoteSet valSet)
            sigs
      case mvoteSet of
        InsertConflict _ -> tell [mkErr "Conflicting votes"]
        InsertDup        -> tell [mkErr "Duplicate votes"]
        InsertUnknown    -> tell [mkErr "Votes from unknown source"]
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



----------------------------------------------------------------
-- DerivingVia
----------------------------------------------------------------

newtype DatabaseByField conn a m x = DatabaseByField (m x)
  deriving newtype (Functor,Applicative,Monad)

instance ( MonadReader r m
         , HasField' conn r (Connection 'RW a)
         , a ~ a'
         ) => MonadReadDB a' (DatabaseByField conn a m) where
  askConnectionRO = DatabaseByField $ connectionRO <$> asks (^. field' @conn)
  {-# INLINE askConnectionRO #-}

instance ( MonadReader r m
         , HasField' conn r (Connection 'RW a)
         , a ~ a'
         ) => MonadDB a' (DatabaseByField conn a m) where
  askConnectionRW = DatabaseByField $ asks (^. field' @conn)
  {-# INLINE askConnectionRW #-}



newtype DatabaseByType a m x = DatabaseByType (m x)
  deriving newtype (Functor,Applicative,Monad)

instance ( MonadReader r m
         , HasType (Connection 'RW a) r
         , a ~ a'
         ) => MonadReadDB a' (DatabaseByType a m) where
  askConnectionRO = DatabaseByType $ connectionRO <$> asks (^. typed @(Connection 'RW a))
  {-# INLINE askConnectionRO #-}

instance ( MonadReader r m
         , HasType (Connection 'RW a) r
         , a ~ a'
         ) => MonadDB a' (DatabaseByType a m) where
  askConnectionRW = DatabaseByType $ asks (^. typed)
  {-# INLINE askConnectionRW #-}
