-- |HSCian.POW
--
-- A wrapper for C bits of HSChain proof-of-wor puzzle, both
-- solution search code and solution checking code.
--
-- Copyright (C) ...

{-# LANGUAGE ForeignFunctionInterface, CApiFFI, RecordWildCards #-}

module HSChain.POW
  ( solve
  , check
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as B

import Data.Word

import Foreign
import Foreign.C.Types

foreign import ccall "evpow_solve"
        evpow_solve :: Ptr Word8    -- ^Header prefix, base for puzzle construction.
                    -> CSize        -- ^Header prefix size.
                    -> Ptr Word8    -- ^(writeable) Answer's buffer.
                    -> Ptr Word8    -- ^(writeable) Full puzzle solution.
                    -> Int          -- ^Clauses count.
                    -> Int          -- ^Complexity shift.
                    -> Word16       -- ^Complexity mantissa.
                    -> Int          -- ^Milliseconds to search for answer.
                    -> Int          -- ^Attempts allowed to search for an answer.
                    -> Int          -- ^Attempts between restarts in local search.
                    -> Int          -- ^Fixed bits count.
                    -> Word64       -- ^Fixed bits.
                    -> Ptr Double   -- ^(writeable) Milliseconds to find first solution.
                    -> IO Int       -- ^Returns C boolean (0 - failure).

foreign import ccall "evpow_check"
        evpow_check :: Ptr Word8
                    -> CSize
                    -> Ptr Word8
                    -> Ptr Word8
                    -> Int
                    -> Int
                    -> Word16
                    -> IO Int

-- |Configuration of POW
data POWConfig = POWConfig
  { powCfgClausesCount             :: !Int
  , powCfgAttemptsBetweenRestarts  :: !Int
  , powCfgAttemptsToSearch         :: !Int
  , powCfgMillisecondsToSearch     :: !Int
  , powCfgComplexityMantissa       :: !Word16
  , powCfgComplexityShift          :: !Int
  }
  deriving (Show)

solve :: [ByteString] -- ^Parts of header to concatenate
      -> POWConfig -- ^Configuration of POW algorithm
      -> IO (Maybe (ByteString, ByteString)) -- ^Returns a "puzzle answer" part of the header and final hash, if found.
solve headerParts POWConfig{..} = B.useAsCStringLen completeHeader $ \(ptr', len) -> do
  answer <- return nullPtr
  completeHash <- return nullPtr
  let ptr = castPtr ptr'
  r <- evpow_solve
         ptr (fromIntegral len)
         answer completeHash
         powCfgClausesCount
         powCfgComplexityShift
         powCfgComplexityMantissa
         powCfgMillisecondsToSearch
         powCfgAttemptsToSearch
         powCfgAttemptsBetweenRestarts
         0 0 -- fixed bits
         nullPtr
  if r /= 0 then return (Just (error "answer", error "hash")) else return Nothing
  where
    completeHeader = B.concat headerParts

check :: ByteString -> ByteString -> POWConfig -> IO Bool
check headerWithAnswer hashOfHeader POWConfig{..} = error "check!"
