{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DerivingStrategies #-}
-- |
module Thundermint.Exceptions where

import Control.Exception
import Thundermint.Types.Blockchain


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
  = DBMissingValSet  !Height     -- ^ Validator set is missing
  | DBMissingGenesis             -- ^ Genesis is missing
  | DBMissingBlock   !Height     -- ^ Missing block
  | DBMissingBlockID !Height     -- ^ Missing block ID
  | DBMissingRound   !Height     -- ^ Missing commit round
  deriving stock    (Show)
  deriving anyclass (Exception)

-- | Internal error in algorithm. Should never happen. 
data InternalError
  = BlockchainStateUnavalable
  -- ^ Program is unsable to retrive blockchain state
  | UnableToCommit
  -- ^ Program fails to write block to disk due to invalid query
  --   (write transaction was rolled back)
  deriving stock    (Show)
  deriving anyclass (Exception)

-- | Exception which is thrown when impossible situation is reached
data ImpossibleError = ImpossibleError
  deriving stock    (Show)
  deriving anyclass (Exception)
