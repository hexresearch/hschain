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
  , POWSearchConfig(..)
  , POWComplexity(..)
  , POWConfig(..)
  , defaultPOWConfig
  , defaultSearchConfig
  , defaultComplexityConfig
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as B

import Data.Word

import Foreign
import Foreign.C.Types
import Foreign.Marshal.Alloc (allocaBytes)

foreign import capi "evpow.h value EVPOW_ANSWER_BYTES" answerSize :: Int
foreign import capi "evpow.h value EVPOW_HASH_BYTES" hashSize :: Int

foreign import ccall unsafe "evpow_solve"
        evpow_solve :: Ptr Word8    -- ^Header prefix, base for puzzle construction.
                    -> CSize        -- ^Header prefix size.
                    -> Ptr Word8    -- ^(writeable) Answer's buffer. Always have some result.
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
                    -> IO Int       -- ^Returns C boolean (not zero means solution has been found).
                                    --  The solution puzzle field will have minimum of solutions found either way, to facilitate the computation of statistics.

foreign import ccall unsafe "evpow_check"
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

data POWSearchConfig = POWSearchConfig
  { searchCfgAttemptsBetweenRestarts  :: !Int
  , searchCfgAttemptsToSearch         :: !Int
  , searchCfgMillisecondsToSearch     :: !Int
  }
  deriving (Eq, Ord, Show)

data POWComplexity = POWComplexity
  { powComplexityShift          :: !Int
  , powComplexityMantissa       :: !Word16
  }
  deriving (Show)

-- |Configuration of POW
data POWConfig = POWConfig
  { powCfgMain                     :: !MainPOWConfig
  , powCfgSearch                   :: !POWSearchConfig
  , powCfgComplexity               :: !POWComplexity
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

defaultSearchConfig :: POWSearchConfig
defaultSearchConfig = POWSearchConfig
  { searchCfgAttemptsBetweenRestarts  = 10000
  , searchCfgAttemptsToSearch         = 2500000
  , searchCfgMillisecondsToSearch     = 2000
  }

-- |Default complexity - should give block rate slightly below 1 block/2 minutes on average i7.
defaultComplexityConfig :: POWComplexity
defaultComplexityConfig = POWComplexity
  { powComplexityShift      = 8 -- 12
  , powComplexityMantissa   = 0xbed8
  }

-- |Configuration of POW - default values.
defaultPOWConfig = POWConfig
  { powCfgMain                     = defaultMainPOWConfig
  , powCfgSearch                   = defaultSearchConfig
  , powCfgComplexity               = defaultComplexityConfig
  }

-- |Solve the puzzle.
solve :: [ByteString] -- ^Parts of header to concatenate
      -> POWConfig -- ^Configuration of POW algorithm
      -> IO (ByteString, Maybe (ByteString, ByteString))
      -- ^First tuple argument is for statistics - it returns the hash found.
      --
      -- The second part is the search result: a "puzzle answer" part of the header
      -- and final hash.
      --
      -- Please note that both minimal hash found (first argument of tuple) and final
      -- hash are treated as little-endian (least significant byte first) integers.
      --
      -- You may be confused how hash starting with, say, byte 0x9b pass under complexity
      -- threshold that requires 4 bytes of leading zeroes (complexity shift 32). If so,
      -- look at bytes at the end and they will be zeroes as required.
      --
      -- As an example (we require 8 zero bits):
      --
      -- $ cabal new-build hschain-pow-func-cli && cabal new-exec -- hschain-pow-func-cli find -s 8 -m 0x8000 -T --print-min-hash --prefix-text '"n"'
      --   minimum hash found: 23 84 1b 5d 86 72 2e fe 15 3f 6a f3 00 0c 72 d0 26 39 b1 95 7e d6 c6 c4 35 f6 a0 32 33 7f 4c 00
      --   complete header: "n\STX\241\236\203u}\217\163\247\EOT\133o\185\192\DC3\208>\STX\EM\238\206\143\DC1\DC4*\STXb\212i\r\135j"

solve headerParts POWConfig{..} = B.useAsCStringLen completeHeader $ \(ptr', len) -> do
  allocaBytes (answerSize + hashSize) $ \answerAndHash -> do
    let answer = answerAndHash
        completeHash = plusPtr answerAndHash answerSize
    let ptr = castPtr ptr'
    r <- evpow_solve
           ptr (fromIntegral len)
           answer completeHash
           powMainClausesCount
           powComplexityShift
           powComplexityMantissa
           searchCfgMillisecondsToSearch
           searchCfgAttemptsToSearch
           searchCfgAttemptsBetweenRestarts
           0 0 -- fixed bits
           nullPtr
    putStrLn $ "returned"
    hashBS <- B.packCStringLen (castPtr completeHash, hashSize)
    if r /= 0
      then do
             answerBS <- B.packCStringLen (castPtr answer, answerSize)
             return (hashBS, Just (answerBS, hashBS))
      else return (hashBS, Nothing)
  where
    completeHeader = B.concat headerParts
    MainPOWConfig{..} = powCfgMain
    POWSearchConfig{..} = powCfgSearch
    POWComplexity{..} = powCfgComplexity

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
              powMainClausesCount powComplexityShift powComplexityMantissa
  where
    MainPOWConfig{..} = powCfgMain
    POWSearchConfig{..} = powCfgSearch
    POWComplexity{..} = powCfgComplexity

