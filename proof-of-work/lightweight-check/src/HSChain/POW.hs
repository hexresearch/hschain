-- |HSCian.POW
--
-- A wrapper for C bits of HSChain proof-of-wor puzzle, both
-- solution search code and solution checking code.
--
-- Copyright (C) ...

{-# LANGUAGE ForeignFunctionInterface, CApiFFI, RecordWildCards #-}

module HSChain.POW
  ( check
  , POWConfig(..)
  , defaultPOWConfig
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as B

import Data.Word

import Foreign
import Foreign.C.Types


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

check :: ByteString -> ByteString -> ByteString -> POWConfig -> IO Bool
check headerWithoutAnswer answer hashOfAnswerHeader POWConfig{..} =
  B.useAsCStringLen headerWithoutAnswer $ \(hdr, hdrLen) -> 
    B.useAsCStringLen answer $ \(answerPtr, answerLen) -> do
      let encodedTarget = encodeIntegerLSB powCfgTarget
      if answerLen /= answerSize
        then return False
        else B.useAsCStringLen hashOfAnswerHeader $ \(hash,hashLen) ->
          if hashLen /= hashSize
            then return False
            else fmap (/= 0) $ B.useAsCString encodedTarget $ \target -> evpow_check
                (castPtr hdr) (fromIntegral hdrLen) (castPtr answerPtr) (castPtr hash)
                powCfgClausesCount (castPtr target)

