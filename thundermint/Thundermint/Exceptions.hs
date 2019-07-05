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

-- | Exception which is thrown when impossible situation is reached
data ImpossibleError = ImpossibleError
  deriving stock    (Show)
  deriving anyclass (Exception)
