{-# LANGUAGE RecordWildCards #-}
module TM.Playground
( getInt8
, getInt16be
, getInt16le
, getInt32be
, getInt32le
, putInt8
, putInt16be
, putInt16le
, putInt32be
, putInt32le
) where

import           Data.Binary
import qualified Data.Binary.Get as BG
import qualified Data.Binary.Put as BP

import Data.Int
import Data.Word

import qualified Data.ByteString.Lazy as B

getInt8 :: Get Int8
getInt8 = do a <- getWord8
             return $ fromIntegral a
getInt16be :: Get Int16
getInt16be = do a <- BG.getWord16be
                return $ fromIntegral a
getInt16le :: Get Int16
getInt16le = do a <- BG.getWord16le
                return $ fromIntegral a
getInt32be :: Get Int32
getInt32be = do a <- BG.getWord32be
                return $ fromIntegral a
getInt32le :: Get Int32
getInt32le = do a <- BG.getWord32le
                return $ fromIntegral a

putInt8 :: Int8 -> Put
putInt8 i = putWord8 ((fromIntegral i) :: Word8)
putInt16be :: Int16 -> Put
putInt16be i = BP.putWord16be ((fromIntegral i) :: Word16)
putInt16le :: Int16 -> Put
putInt16le i = BP.putWord16le ((fromIntegral i) :: Word16)
putInt32be :: Int32 -> Put
putInt32be i = BP.putWord32be ((fromIntegral i) :: Word32)
putInt32le :: Int32 -> Put
putInt32le i = BP.putWord32le ((fromIntegral i) :: Word32)

data TestType = TestType
    { a :: Int16
    , b :: Int16
    } deriving (Show, Eq)

instance Binary TestType where
    put TestType{..} =
      do putInt16be a
         putInt16le b
    get = do a <- getInt16be
             b <- getInt16le
             return TestType{..}

main :: IO ()
main = do
    putStrLn "Supplies Get and Put support to Int8, Int16 etc. types as Data.Binary.Get and Data.Binary.Push do for Word8, Word 16 etc."
    putStrLn ""
    putStrLn "Test data in bytes:"
    print bytes
    putStrLn ""
    putStrLn "As TestType:"
    print (decode bytes :: TestType)
    print (decode bytes1 :: TestType)
    putStrLn ""
    putStrLn "Back to bytes:"
    print $ (encode ((decode bytes) :: TestType))
  where
    bytes = B.pack $ concat $ replicate 2 [0xCD,0xEF]
    bytes1 = B.pack $ concat $ replicate 2 [0x12,0x00]



{-     header <- recvAll sock 2
     let (BE len) = decodeWord16BE header
     r <- recvAll sock len
     print r
     header1 <- recvAll sock 2
     let (BE len1) = decodeWord16BE header1
     print len1
     r1 <- recvAll sock len1
     return $ emptyBs2Maybe $ ( r <> r1)
-}
--
    -- emptyBs2Maybe <$> recvAll' sock



{--    header <- recvAll sock 2
    let (BE len) = decodeWord16BE header
    r <- recvAll sock len
    return $ emptyBs2Maybe r
-}
      -- FIXME: packet length should be added before message 2 octets in network order BE
--      emptyBs2Maybe <$> NetLBS.recv sock 4096
--      emptyBs2Maybe <$> recvAll sock 4096

{-
      header <- recvAll sock 2
      let (BE len) = decodeWord16BE header
      print len
      r <- recvAll sock len
      header1 <- recvAll sock 2
      let (BE len1) = decodeWord16BE header1
      print len1
      r1 <- recvAll sock len1
      header <- recvAll sock 2
      let (BE len2) = decodeWord16BE header
      print len2
      r2 <- recvAll sock len2
      return $ emptyBs2Maybe $ ( r <> r1 <> r2)
-}
--      return $ emptyBs2Maybe $ ( foldl LBS.append "" [r, r1])
--      emptyBs2Maybe <$> ( LBS.concat <$> r <*> r1)


--recvAll' :: Net.Socket -> IO LBS.ByteString
{-
recvAll' sock = loop -- LBS.concat `fmap` loop
  where
    loop = do
        header <- recvAll sock 2

        let n = if LBS.null header
                then  0
                else let (BE len) = decodeWord16BE header
                     in len
        r <- recvAll sock n
        print r
        if n == 0
        then return ""
        else fmap (r<>) loop -- loop (acc <> r)


--        fmap (<>) loop
-}
{-
-- | receive loop which will apply function on each frame
recvAllLoop :: (LBS.ByteString -> IO ()) -> Net.Socket -> IO (Maybe LBS.ByteString)
recvAllLoop f sock = loop
  where
    loop = do
      mFrame <- recvFrame sock
      case mFrame of
        Nothing -> return Nothing
        Just frame -> do
          f frame
          loop

-}
