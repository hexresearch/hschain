-- |
module Thundermint.Exceptions where

import Control.Exception


----------------------------------------------------------------
--
----------------------------------------------------------------

-- | Some network error.
data NetworkError
  = ConnectionTimedOut
  | NoAddressAvailable
  deriving (Show)

instance Exception NetworkError
