-- |HSCian.POW
--
-- A wrapper for C bits of HSChain proof-of-wor puzzle, both
-- solution search code and solution checking code.
--
-- Copyright (C) ...

{-# LANGUAGE FFI, CApiFFI #-}

module HSChain.POW
  ( solve
  , check
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as B

import Data.Word

import Foreign

foreign import ccall "evpow_solve"
        evpow_solve :: Ptr Word8    -- ^Header prefix, base for puzzle construction.
                    -> CSize        -- ^Header prefix size.
                    -> Ptr Word8    -- ^(writeable) Answer's buffer.
                    -> Ptr Word8    -- ^(writeable) Full puzzle solution.
                    -> CInt         -- ^Clauses count.
                    -> CInt         -- ^Complexity shift.
                    -> CUShort      -- ^Complexity mantissa.
                    -> CInt         -- ^Milliseconds to search for answer.
                    -> CInt         -- ^Attempts allowed to search for an answer.
                    -> CInt         -- ^Attempts between restarts in local search.
                    -> CInt         -- ^Fixed bits count.
                    -> CULong       -- ^Fixed bits.
                    -> Ptr CDouble  -- ^(writeable) Milliseconds to find first solution.
                    -> CInt         -- ^Returns C boolean (0 - failure).

-- |Configuration of POW
data POWConfig = POWConfig
  { powCfgClausesCount             :: !Word32
  }
  deriving (Show)

solve :: [ByteString] -- ^Parts of header to concatenate
      -> POWConfig -- ^Configuration of POW algorithm
      -> IO (Maybe (ByteString, ByteString)) -- ^Returns a "puzzle answer" part of the header and final hash, if found.
solve headerParts POWConfog{..} = B.useAsCStringLen completeHeader $ \(ptr, len) -> do
  answer <- return nullPtr
  completeHash <- return nullPtr
  r <- evpow_solve
         ptr len
         answer completeHash
         powCfgClausesCount
         powCfgComplexityShift
         powCfgComplexityMantissa
         powCfgMillisecondsToSearch
         powCfgAttemptsToSearch
         powCfgAttemptsBetweenRestarts
         0 0 -- fixed bits
         nullPtr
  if r /= 0 then return (Just ("answer", "hash")) else return Nothing
  where
    completeHeader = B.concat headerParts
