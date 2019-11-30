{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DerivingStrategies #-}
-- |
module HSChain.Exceptions where

import Control.Exception
import Data.Text (Text)
import HSChain.Types.Blockchain


----------------------------------------------------------------
--
----------------------------------------------------------------

-- | Some network error.
data NetworkError
  = ConnectionTimedOut
  | NoAddressAvailable
  | CantReverseLookipHostname
  deriving stock    (Show)
  deriving anyclass (Exception)

-- | Database is corruped
data CorruptedDBError
  = DBMissingValSet       !Height -- ^ Validator set is missing
  | DBMissingGenesis              -- ^ Genesis is missing
  | DBMissingBlock        !Height -- ^ Missing block
  | DBMissingBlockID      !Height -- ^ Missing block ID
  | DBMissingRound        !Height -- ^ Missing commit round
  | DBInvalidValidatorSet !Height -- ^ Invalid validators set at given height
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
  | InvalidBlockInWAL
  -- ^ Invalid block is stored in WAL
  | CannotRewindState
  -- ^ State transitions already record in blockchain are not valid.
  | InconsisnceWhenRewinding !Height !Text
  -- ^ Incinsistency encountered when rewinding 
  deriving stock    (Show)
  deriving anyclass (Exception)

-- | Error generated due to user provided callback which violated some
--   invariant
data CallbackError
  = InvalidBlockGenerated
  -- ^ Invalid block was generated.
  deriving stock    (Show)
  deriving anyclass (Exception)
