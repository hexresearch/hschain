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
  , MainPOWConfig(..)
  , POWConfig(..)
  , defaultPOWConfig
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as B

import Data.Word

import Foreign
import Foreign.C.Types
import Foreign.Marshal.Alloc (allocaBytes)

foreign import capi "evpow.h value EVPOW_ANSWER_BYTES" answerSize :: Int
foreign import capi "evpow.h value EVPOW_HASH_BYTES" hashSize :: Int

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

-- |Main configuration.
data MainPOWConfig = MainPOWConfig
  { powMainClausesCount            :: !Int
  , powMainBlocksBetweenAdjustment :: !Int
  , powMainSecondsForBlock         :: !Int
  }
  deriving (Show)
-- |Configuration of POW
data POWConfig = POWConfig
  { powCfgMain                     :: !MainPOWConfig
  , powCfgAttemptsBetweenRestarts  :: !Int
  , powCfgAttemptsToSearch         :: !Int
  , powCfgMillisecondsToSearch     :: !Int
  , powCfgComplexityMantissa       :: !Word16
  , powCfgComplexityShift          :: !Int
  }
  deriving (Show)

-- |Main configuration: complexity of search, time between blocks to attain and
-- number of blocks to wait between adjustments.
defaultMainPOWConfig :: MainPOWConfig
defaultMainPOWConfig = MainPOWConfig
  { powMainClausesCount            = 5250
  , powMainBlocksBetweenAdjustment = 1024
  , powMainSecondsForBlock         = 120
  }

-- |Configuration of POW - default values.
defaultPOWConfig = POWConfig
  { powCfgMain                     = defaultMainPOWConfig
  , powCfgAttemptsBetweenRestarts  = 10000
  , powCfgAttemptsToSearch         = 2500000
  , powCfgMillisecondsToSearch     = 2000
  , powCfgComplexityMantissa       = 0xbed8 -- 0xbed8 * 2^(-12) should give block rate slightly above 2 minutes per block
  , powCfgComplexityShift          = 12
  }

-- |Solve the puzzle.
solve :: [ByteString] -- ^Parts of header to concatenate
      -> POWConfig -- ^Configuration of POW algorithm
      -> IO (Maybe (ByteString, ByteString)) -- ^Returns a "puzzle answer" part of the header and final hash, if found.
solve headerParts POWConfig{..} = B.useAsCStringLen completeHeader $ \(ptr', len) -> do
  allocaBytes (answerSize + hashSize) $ \answerAndHash -> do
    let answer = answerAndHash
        completeHash = plusPtr answerAndHash answerSize
    let ptr = castPtr ptr'
    r <- evpow_solve
           ptr (fromIntegral len)
           answer completeHash
           powMainClausesCount
           powCfgComplexityShift
           powCfgComplexityMantissa
           powCfgMillisecondsToSearch
           powCfgAttemptsToSearch
           powCfgAttemptsBetweenRestarts
           0 0 -- fixed bits
           nullPtr
    if r /= 0
      then do
             answerBS <- B.packCStringLen (castPtr answer, answerSize)
             hashBS <- B.packCStringLen (castPtr completeHash, hashSize)
             return (Just (answerBS, hashBS))
      else return Nothing
  where
    completeHeader = B.concat headerParts
    MainPOWConfig{..} = powCfgMain

check :: ByteString -> ByteString -> POWConfig -> IO Bool
check headerWithAnswer hashOfHeader POWConfig{..} =
  B.useAsCStringLen headerWithAnswer $ \(hdr, hdrLen) ->
    if hdrLen < answerSize
      then return False
      else B.useAsCStringLen hashOfHeader $ \(hash,hashLen) -> do
        if hashLen /= hashSize
          then return False
          else do
            let prefixSize = hdrLen - answerSize
                answer = plusPtr hdr prefixSize
            fmap (/= 0) $ evpow_check
              (castPtr hdr) (fromIntegral prefixSize) (castPtr answer) (castPtr hash)
              powMainClausesCount powCfgComplexityShift powCfgComplexityMantissa
  where
    MainPOWConfig{..} = powCfgMain

