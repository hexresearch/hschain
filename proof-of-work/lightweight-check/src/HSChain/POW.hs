-- |HSCian.POW
--
-- A wrapper for C bits of HSChain proof-of-wor puzzle, both
-- solution search code and solution checking code.
--
-- Copyright (C) ...

{-# LANGUAGE BangPatterns, RecordWildCards #-}

module HSChain.POW
  ( check
  , POWConfig(..)
  , defaultPOWConfig
  ) where

import Data.Bits

import Data.ByteString (ByteString)
import qualified Data.ByteString as B

import Data.Word

import HSChain.Crypto.SHA
import HSChain.Crypto.Classes.Hash

answerSize :: Int
answerSize = 32 -- Size of SHA256.

-- |Configuration of POW
data POWConfig = POWConfig
  { powCfgClausesCount             :: !Int
  , powCfgTarget                   :: !Integer
  }
  deriving (Show)

-- |Configuration of POW - default values.
defaultPOWConfig :: POWConfig
defaultPOWConfig = POWConfig
  { powCfgClausesCount             = 5250
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

-- |Compare two bytestrings as little-endian encoded integers.
compareLSB :: ByteString -> ByteString -> Ordering
compareLSB b1 b2 = cmp EQ (B.unpack b1) (B.unpack b2)
  where
    cmp !order  []         []        = order
    cmp  order (by1:by1s) (by2:by2s) = case compare by1 by2 of
      EQ     -> cmp order  by1s by2s -- equality at higher byte does not change ordering of lower bytes.
      order' -> cmp order' by1s by2s -- non-equality at higher byte overrides ordering.
    cmp  _      _          _         = error "bytestring must be of same length."

-- |Check whether answer solves puzzle for header.
solvesPuzzle :: ByteString -> ByteString -> Bool
solvesPuzzle answer header = False

check :: ByteString -> ByteString -> ByteString -> POWConfig -> IO Bool
check headerWithoutAnswer answer hashOfAnswerHeader POWConfig{..}
  | B.length hashOfAnswerHeader /= answerSize = return False -- wrong data size.
  | targetCompareResult == LT = return False -- final hash not under target complexity.
  | computedHash /= hashOfAnswerHeader = return False -- hashes do not match.
  | otherwise = return $ solvesPuzzle answer headerWithoutAnswer
  where
    encodedTarget = encodeIntegerLSB powCfgTarget
    targetCompareResult = compareLSB encodedTarget hashOfAnswerHeader
    Hash computedHash = hashBlob $ B.concat [answer, headerWithoutAnswer] :: Hash SHA256
    
--  B.useAsCStringLen headerWithoutAnswer $ \(hdr, hdrLen) -> 
--    B.useAsCStringLen answer $ \(answerPtr, answerLen) -> do
--      let encodedTarget = encodeIntegerLSB powCfgTarget
--      if answerLen /= answerSize
--        then return False
--        else B.useAsCStringLen hashOfAnswerHeader $ \(hash,hashLen) ->
--          if hashLen /= hashSize
--            then return False
--            else fmap (/= 0) $ B.useAsCString encodedTarget $ \target -> evpow_check
--                (castPtr hdr) (fromIntegral hdrLen) (castPtr answerPtr) (castPtr hash)
--                powCfgClausesCount (castPtr target)

