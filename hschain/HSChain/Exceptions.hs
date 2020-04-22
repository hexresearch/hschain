{-# LANGUAGE DeriveAnyClass            #-}
{-# LANGUAGE DerivingStrategies        #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE StandaloneDeriving        #-}
-- |
module HSChain.Exceptions where

import Control.Exception
import Data.Text       (Text,unpack)
import HSChain.Types.Blockchain
import HSChain.Crypto  (Hash,CryptoHash)

----------------------------------------------------------------
--
----------------------------------------------------------------

-- | Database is corruped
data CorruptedDBError
  = DBMissingValSet       !Height -- ^ Validator set is missing
  | DBMissingGenesis              -- ^ Genesis is missing
  | DBMissingBlock        !Height -- ^ Missing block
  | DBMissingBlockID      !Height -- ^ Missing block ID
  | DBMissingRound        !Height -- ^ Missing commit round
  | DBInvalidValidatorSet !Height -- ^ Invalid validators set at given height
  | DBMissingBlob
  deriving stock    (Show)
  deriving anyclass (Exception)

-- | Trying to access closed database
data DatabaseIsClosed = DatabaseIsClosed
  deriving stock    (Show)
  deriving anyclass (Exception)

-- | Internal error in algorithm. Could only occure either because of
--   bug or due to data corruption.
data InternalError
  = BlockchainStateUnavalable
  -- ^ Program is unsable to retrive blockchain state
  | UnableToCommit
  -- ^ Program fails to write block to disk due to invalid query
  --   (write transaction was rolled back)
  | UnexpectedRollback
  -- ^ Transaction which shouldn't be rolled back was rolled back
  | forall a. Exception a => InvalidBlockInWAL !a
  -- ^ Invalid block is stored in WAL
  | forall a. Exception a => InvalidBlockGenerated !a
  -- ^ Genration of block ended in failure
  | forall a. Exception a => CannotRewindState !a
  -- ^ State transitions already record in blockchain are not valid.
  | forall a. Exception a => TryingToCommitInvalidBlock !a
  -- ^ Engine tries to commit invalid block. That it we have commit
  --   for block that doesn't pass validation. That could happen due
  --   to bug in validation code or due to byzantine behavior of +2\/3
  --   validators
  | forall alg. CryptoHash alg => InconsistenceWhenRewinding !Height !Text !(Mismatch (Hash alg))
  -- ^ Incinsistency encountered when rewinding
deriving stock instance Show InternalError

-- | Data type which is used to display mismatch between expected
--   value and actual value.
data Mismatch a = Mismatch
  { expectedValue :: !a
  , actualValue   :: !a
  }
  deriving stock Show


instance Exception InternalError where
  displayException = \case
    BlockchainStateUnavalable -> "BlockchainStateUnavalable :: InternalError"
    UnableToCommit            -> "UnableToCommit :: InternalError"
    UnexpectedRollback        -> "UnexpectedRollback :: InternalError"
    InvalidBlockInWAL e -> unlines
      [ "InvalidBlockInWAL :: InternalError"
      , displayException e
      ]
    InvalidBlockGenerated e -> unlines
      [ "InvalidBlockGenerated :: InternalError"
      , displayException e
      ]
    CannotRewindState e -> unlines
      [ "CannotRewindState :: InternalError"
      , displayException e
      ]
    TryingToCommitInvalidBlock e -> unlines
      [ "TryingToCommitInvalidBlock :: InternalError"
      , displayException e
      ]
    InconsistenceWhenRewinding h t Mismatch{..} -> unlines
      [ "InconsistenceWhenRewinding :: InternalError"
      , "  H of block that caused problem:"
      , "    " ++ show h
      , "  Problem:"
      , "    " ++ unpack t
      , "  Expected hash (one that stored in block):"
      , "    " ++ show expectedValue
      , "  Hash of computed value:"
      , "    " ++ show actualValue
      ]
