-- |HSCian.POW
--
-- A wrapper for C bits of HSChain proof-of-wor puzzle, both
-- solution search code and solution checking code.
--
-- Copyright (C) ...

{-# LANGUAGE CApiFFI                  #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE MultiWayIf               #-}
{-# LANGUAGE RecordWildCards          #-}
module HSChain.POW
  ( solve
  , check
  , POWConfig(..)
  , defaultPOWConfig
  ) where

import Control.Monad.IO.Class
import Control.Monad.Trans.Cont
import Data.ByteString (ByteString)
import qualified Data.ByteString as B

import Data.Word

import Foreign
import Foreign.C.Types

foreign import capi "evpow.h value EVPOW_ANSWER_BYTES" answerSize :: Int
foreign import capi "evpow.h value EVPOW_HASH_BYTES" hashSize :: Int

foreign import ccall "evpow_solve"
        evpow_solve :: Ptr Word8    -- ^Header suffix, base for puzzle construction.
                    -> CSize        -- ^Header suffix size.
                    -> Ptr Word8    -- ^(writeable) Answer's buffer.
                    -> Ptr Word8    -- ^(writeable) Full puzzle solution.
                    -> Int          -- ^Clauses count.
                    -> Ptr Word8    -- ^Little-endian target.
                    -> Int          -- ^Milliseconds to search for answer.
                    -> Int          -- ^Attempts allowed to search for an answer.
                    -> Int          -- ^Attempts between restarts in local search.
                    -> Int          -- ^Fixed bits count.
                    -> Word64       -- ^Fixed bits.
                    -> Ptr Double   -- ^(writeable) Milliseconds to find first solution.
                    -> IO Int       -- ^Returns C boolean (0 - failure).

foreign import ccall "evpow_check"
        evpow_check :: Ptr Word8    -- ^Suffix buffer
                    -> CSize        --
                    -> Ptr Word8    --
                    -> Ptr Word8    --
                    -> Int          --
                    -> Ptr Word8    --
                    -> IO Int       -- ^C boolean.

-- |Parameters of search algorithm.
data POWSearchConfig = POWSearchConfig
  { powSearchAttemptsBetweenRestarts
  , powSearchAttemptsToSearch
  , powSearchMillisecondsToSearch     :: !Int
  }
  deriving (Show)

-- |Configuration of POW
data POWConfig = POWConfig
  { powCfgClausesCount             :: !Int
  , powCfgSearchConfig             :: !POWSearchConfig
  , powCfgTarget                   :: !Integer
  }
  deriving (Show)

-- |Default configuration for search algorithm.
defaultSearchConfig :: POWSearchConfig
defaultSearchConfig = POWSearchConfig
  { powSearchAttemptsBetweenRestarts  = 10000
  , powSearchAttemptsToSearch         = 2500000
  , powSearchMillisecondsToSearch     = 2000
  }

-- |Configuration of POW - default values.
defaultPOWConfig :: POWConfig
defaultPOWConfig = POWConfig
  { powCfgClausesCount             = 5250
  , powCfgSearchConfig             = defaultSearchConfig
  , powCfgTarget                   = shiftL 1 256 - 1 -- Easiest target to meet.
  }

-- |Encode an integer as least-significant-byte first (little endian).
-- As we use this for specifying targets, we cut transformation at
-- @answerSize@ bytes. This means that Integer must be less that
-- @2^(answerSize * 8)@.
encodeIntegerLSB :: Integer -> ByteString
encodeIntegerLSB i
  | i >= shiftL 1 (8 * answerSize) = error "integer is too big to be encoded as target bytestring"
  | i < 0 = error "negative integer cannot be a target"
  | otherwise = B.pack $ take answerSize $
                         map (fromIntegral . (`mod` 256)) $ iterate (`div` 256) i

-- |Solve the puzzle.
solve :: [ByteString] -- ^Parts of header to concatenate
      -> POWConfig -- ^Configuration of POW algorithm
      -> IO (Maybe ByteString, ByteString) -- ^Returns a "puzzle answer" part of the header and best hash found.
solve headerParts POWConfig{..} = B.useAsCStringLen completeHeader $ \(ptr', len) -> do
  allocaBytes (answerSize + hashSize) $ \answerAndHash -> do
    let answer = answerAndHash
        completeHash = plusPtr answerAndHash answerSize
    let ptr = castPtr ptr'
        encodedTarget = encodeIntegerLSB powCfgTarget
    r <- B.useAsCString encodedTarget $ \target -> evpow_solve
           ptr (fromIntegral len)
           answer completeHash
           powCfgClausesCount
           (castPtr target)
           powSearchMillisecondsToSearch
           powSearchAttemptsToSearch
           powSearchAttemptsBetweenRestarts
           0 0 -- fixed bits
           nullPtr
    hashBS <- B.packCStringLen (castPtr completeHash, hashSize)
    if r /= 0
      then do
             answerBS <- B.packCStringLen (castPtr answer, answerSize)
             return (Just answerBS, hashBS)
      else return (Nothing, hashBS)
  where
    POWSearchConfig{..} = powCfgSearchConfig
    completeHeader = B.concat headerParts

-- | Check whether cryptographic puzzle is correct
check
  :: ByteString -- ^ Serialized header without nonce
  -> ByteString -- ^ Answer to puzzle
  -> ByteString -- ^ ???
  -> POWConfig
  -> IO Bool
check headerWithoutAnswer answer hashOfAnswerHeader POWConfig{..} = evalContT $ do
  (hdr,       hdrLen)    <- ContT $ B.useAsCStringLen headerWithoutAnswer
  (answerPtr, answerLen) <- ContT $ B.useAsCStringLen answer
  abortIf $ answerLen /= answerSize
  (hash,hashLen) <- ContT $ B.useAsCStringLen hashOfAnswerHeader
  abortIf $ hashLen /= hashSize
  target <- ContT $ B.useAsCString encodedTarget
  r <- liftIO $ evpow_check
         (castPtr hdr) (fromIntegral hdrLen)
         (castPtr answerPtr)
         (castPtr hash)
         powCfgClausesCount
         (castPtr target)
  pure $! r /= 0
  where
    encodedTarget = encodeIntegerLSB powCfgTarget
    --
    abortIf :: Bool -> ContT Bool IO ()
    abortIf True  = return ()
    abortIf False = ContT $ \_ -> return False
