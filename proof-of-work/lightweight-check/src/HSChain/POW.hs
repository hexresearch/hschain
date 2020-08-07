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

-- |Count of literals in a clause. Corresponds to EVPOW_K definition in C code.
clauseLiteralsCount_K :: Int
clauseLiteralsCount_K = 5

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

-- |Decode clause from the hash.
--
-- We are using here the same encoding as in the C code, which is
-- variable index must be in 1..256 (instead of 0..255) and literal
-- polarity is encoded in the sign.
--
-- This way it is slightly easier to validate against C code.
--
-- Also
createClause :: ByteString -> [Int]
createClause currentHash = take clauseLiteralsCount_K $ decodeClause [] $ B.unpack currentHash
  where
    decodeClause acc [] = []
    decodeClause acc (bLo:bHi:bs) = literal : decodeClause (variable : acc) bs
      where
        w16 = fromIntegral bLo + fromIntegral bHi * 256 :: Word16
        polarity = w16 .&. 1
        assignTrue = polarity == 1
        modulo = answerSize * 8
        randomIndex = ((fromIntegral $ shiftR w16 1) `mod` modulo) + 1 -- 1..256
        variable = skipUsed acc randomIndex
        skipUsed ps index
          | elem index ps = skipUsed ps (mod (index + 1) modulo + 1)
          | otherwise = index
        literal = if assignTrue then variable else negate variable

-- |Check whether answer solves puzzle for header.
solvesPuzzle :: Int -> ByteString -> ByteString -> Bool
solvesPuzzle numClauses answer header = all clauseSatisfied clauses
  where
    hashByStrToByStr byStr = toByStr
      where
        Hash toByStr = hashBlob byStr :: Hash SHA256
    clauses = take numClauses $ map createClause $ tail $ iterate hashByStrToByStr header
    clauseSatisfied literals = any literalSatisfied literals
      where
        literalSatisfied l = (l > 0) == answerBitTrue
          where
            v = abs l
            bit = v - 1
            (byte, bitInByte) = divMod bit 8
            answerByte = B.index answer byte
            answerBitTrue = (answerByte .&. shiftL 1 bitInByte) /= 0

check :: ByteString -> ByteString -> ByteString -> POWConfig -> IO Bool
check headerWithoutAnswer answer hashOfAnswerHeader POWConfig{..}
  | B.length hashOfAnswerHeader /= answerSize = return False -- wrong data size.
  | targetCompareResult == LT = return False -- final hash not under target complexity.
  | computedHash /= hashOfAnswerHeader = return False -- hashes do not match.
  | otherwise = return $ solvesPuzzle powCfgClausesCount answer headerWithoutAnswer
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

