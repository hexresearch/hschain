{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
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
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}
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
  , MonadCached(..)
  , queryRO
  , queryRW
  , mustQueryRW
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
  , Cached
  , newCached
    -- ** Standard DB wrapper
  , DatabaseByField(..)
  , DatabaseByType(..)
  , DatabaseByReader(..)
  , CachedByField(..)
  , CachedByType(..)
    -- ** Standard API
  , blockchainHeight
  , retrieveBlock
  , retrieveBlockID
  , retrieveLocalCommit
  , retrieveValidatorSet
  , mustRetrieveBlock
  , mustRetrieveValidatorSet
    -- * Blockchain invariants checkers
  , BlockchainInconsistency
  , checkStorage
  , checkProposedBlock
  ) where

import Codec.Serialise           (Serialise)
import Control.Monad.Catch       (MonadMask)
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Control.Monad.Reader
import Control.Monad.Trans.Writer
#if !MIN_VERSION_base(4,13,0)
import Control.Monad.Fail         (MonadFail)
#endif

import Data.Aeson                (FromJSON,ToJSON)
import Data.Maybe                (isNothing)
import Data.Text                 (Text)
import Data.Generics.Product.Fields (HasField'(..))
import Data.Generics.Product.Typed  (HasType(..))
import qualified Data.List.NonEmpty as NE
import Lens.Micro
import GHC.Generics (Generic)

import HSChain.Types.Blockchain
import HSChain.Types.Merkle.Types
import HSChain.Blockchain.Internal.Types
import HSChain.Crypto
import HSChain.Crypto.Containers
import HSChain.Store.Internal.Query
import HSChain.Store.Internal.BlockDB
import HSChain.Store.Query (DatabaseByField(..), DatabaseByType(..), DatabaseByReader(..))
import HSChain.Types.Validators

----------------------------------------------------------------
-- Monadic API for DB access
----------------------------------------------------------------

-- | Helper function which opens database, initializes it and ensures
--   that it's closed on function exit
withDatabase
  :: (MonadIO m, MonadMask m)
  => FilePath         -- ^ Path to the database
  -> (Connection 'RW -> m x) -> m x
withDatabase = withConnection

-- | Initialize all required tables in database.
initDatabase :: (MonadIO m, MonadDB m, MonadCached a m) => m ()
initDatabase = mustQueryRW initializeBlockhainTables


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
  deriving stock    (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-------------------------------------------------------------------------------
-- Invariant checking for storage and proposed blocks
-------------------------------------------------------------------------------

-- | check storage against all consistency invariants
checkStorage
  :: forall m a. (MonadReadDB m, MonadCached a m, MonadIO m, Crypto (Alg a), Serialise a, CryptoHashable a)
  => m [BlockchainInconsistency]
checkStorage = queryRO $ execWriterT $ do
  maxH         <- lift   blockchainHeight
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
  :: (MonadReadDB m, MonadCached a m, MonadIO m, Crypto (Alg a))
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

-- | Newtype wrapper which allows to derive 'MonadReadDB' and
--   'MonadDB' instances using deriving via mechanism by specifying name
--   of field in record carried by reader.
newtype CachedByField conn a m x = CachedByField (m x)
  deriving newtype (Functor,Applicative,Monad)

instance ( MonadReader r m
         , a ~ a'
         , HasField' conn r (Cached a)
         ) => MonadCached a (CachedByField conn a' m) where
  askCached = CachedByField $ asks (^. field' @conn)




-- | Newtype wrapper which allows to derive 'MonadReadDB' and
--   'MonadDB' instances using deriving via mechanism by using type of
--   field in record carried by reader.
newtype CachedByType a m x = CachedByType (m x)
  deriving newtype (Functor,Applicative,Monad)

instance ( MonadReader r m
         , a ~ a'
         , HasType (Cached a) r
         ) => MonadCached a (CachedByType a' m) where
  askCached = CachedByType $ asks (^. typed @(Cached a))
