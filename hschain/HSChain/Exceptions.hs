{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveAnyClass            #-}
{-# LANGUAGE DerivingStrategies        #-}
{-# LANGUAGE ExistentialQuantification #-}
-- |
module HSChain.Exceptions where

import Control.Exception
import Data.Text (Text)
import HSChain.Types.Blockchain


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
  | InconsisnceWhenRewinding !Height !Text
  -- ^ Incinsistency encountered when rewinding

deriving stock    instance Show      InternalError
deriving anyclass instance Exception InternalError
