{-# LANGUAGE NumDecimals       #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
-- |
module TM.RealNetwork (tests) where

import Control.Concurrent.Async
import Control.Exception
import Data.Binary
import Data.Int
import System.Random
import Test.Tasty
import Test.Tasty.HUnit
import Thundermint.P2P.Network

import Control.Concurrent (threadDelay)
import Data.Monoid        ((<>))

import qualified Data.Binary.Get      as BG
import qualified Data.Binary.Put      as BP
import qualified Data.ByteString.Lazy as LBS
import qualified Network.Socket       as Net

----------------------------------------------------------------

-- | used run from ghci
run :: IO ()
run = defaultMain $ testGroup "test suite"
  [ tests
  ]


tests :: TestTree
tests = testGroup "real network"
  [
    testCase "hex to dec" hexTest
  , testCase "dec to hex" intToHexStrTest
  , testCase "ping-pong"  pingPong
--  , testCase "framing" framing
  ]


-- | Simple test on real network
pingPong :: IO ()
pingPong = do
  -- | there was a delay while socket freeing the port
  -- and when you run test some time you will get "resource busy (Address already in use) exception
  -- the random port resolve this
  n <- randomRIO (0, 100 :: Int)
  let port = "300" ++ show  n
      host = "127.0.0.1"
      server = realNetwork port
      hints = Net.defaultHints  { Net.addrSocketType = Net.Stream }
  addr:_ <- Net.getAddrInfo (Just hints) (Just host) (Just port)
  let sockAddr = Net.addrAddress addr
  --
  let runServer NetworkAPI{..} = do
        bracket listenOn fst $ \(_,accept) ->
          bracket (accept) (close . fst) $ \(conn,_) -> do
            Just bs <- recv conn
            send conn ("PONG_" <> bs)
  let runClient NetworkAPI{..}  = do
        threadDelay 10e3
        bracket (connect sockAddr) close $ \conn -> do
          send conn "PING"
          bs <- recv conn
          assertEqual "Ping-pong" (Just "PONG_PING") bs
  ((),()) <- concurrently (runServer server) (runClient server)
  return ()



-- | test on framing
framing :: IO ()
framing = do
  n <- randomRIO (101, 200 :: Int)
  let port = "300"  ++ show n
      host = "127.0.0.1"
      server = realNetwork port
      hints = Net.defaultHints  { Net.addrSocketType = Net.Stream }
  addr:_ <- Net.getAddrInfo (Just hints) (Just host) (Just port)
  let sockAddr = Net.addrAddress addr
  print sockAddr
  --
  let runServer NetworkAPI{..} = do
        bracket listenOn fst $ \(_,accept) ->
          bracket (accept) (close . fst) $ \(conn,_) -> do
            Just bs <- recv conn
            send conn "\NUL\ACKSERVER"
--            send conn "\NUL\ACK123456\NUL\ACK123456"
            assertEqual "Ping-pong" (Just "PONG_PING") (Just bs)
--            send conn ("\NUL\ENQPONG_" <> "\NUL\ENQ" <> bs)
  let runClient NetworkAPI{..}  = do
        threadDelay 10e3
        bracket (connect sockAddr) close $ \conn -> do
          send conn "\NUL\ENQxPING\NUL\ENQ12345"
          send conn "\NUL\ACKssssss\NUL\ACKqq3456"
          threadDelay 10e6
          send conn "\NUL\ACKxxssss\NUL\ACKxx3456"
--          send conn "PING_asdasdasdasdasdasdasdasdasdasdasdasdasdasdasd asdasdasdasdasdasdasdasdasdasdasdasdasdasdasdasdasdasdadasdasdasd\nPING_asdasdasdasdasdasdasdasdasdasdasdasdasdasdasd asdasdasdasdasdasdasdasdasdasdasdasdasdasdasdasdasdasdadasdasdasd"
          (Just bs) <- recv conn
    --      print $ "from client" <> bs
          assertEqual "Ping-pong" (Just "PONG_PING") (Just bs)
  ((),()) <- concurrently (runServer server) (runClient server)
  return ()




-------------------------------------------------------------------------------
-- binary to decimal
-------------------------------------------------------------------------------


data  BeLe = BeLe
    { be :: Int16
    , le :: Int16
    } deriving (Show, Eq)


instance Binary BeLe where
    put BeLe{..} =
      do putInt16be be
         putInt16le le
    get = do be <- getInt16be
             le <- getInt16le
             return BeLe{..}

getInt16be :: Get Int16
getInt16be = do a <- BG.getWord16be
                return $ fromIntegral a

getInt16le :: Get Int16
getInt16le = do a <- BG.getWord16le
                return $ fromIntegral a

putInt16be :: Int16 -> Put
putInt16be i = BP.putWord16be ((fromIntegral i) :: Word16)
putInt16le :: Int16 -> Put
putInt16le i = BP.putWord16le ((fromIntegral i) :: Word16)

-- | test case for Big Endian and Little Endian converting

-- INT16 - Big Endian (AB) | Little Endian (BA)
-- FD EE -> -530  (FD EE) BE | -4355  (EE FD) LE
-- 1234  -> 4660  (1234)  BE | 13330  (34 12) LE
-- 12 AB -> 4779  (12AB)  BE | -21742 (AB 12) LE
-- 12    -> 4608  (1200)  BE | 18     (00 12) LE
hexTest :: IO ()
hexTest =
    let hex1 = LBS.pack [0xFD,0xEE,0xFD,0xEE]
        hex2 = LBS.pack [0x12,0x34,0x12,0x34]
        hex3 = LBS.pack [0x12,0xAB,0x12,0xAB]
        hex4 = LBS.pack [0x12,0x00,0x12,0x00]
     in assertEqual "0xFDEE" (BeLe {be = -530, le = -4355}) (decode hex1 :: BeLe) >>
        assertEqual "0x1234" (BeLe {be = 4660, le = 13330}) (decode hex2 :: BeLe) >>
        assertEqual "0x12AB" (BeLe {be = 4779, le = -21742})(decode hex3 :: BeLe) >>
        assertEqual "0x1200" (BeLe {be = 4608, le = 18})    (decode hex4 :: BeLe)

intToHexStrTest :: IO ()
intToHexStrTest =
    let n1 = 5  :: Word16
        n2 = 10 :: Word16
        n3 = 1  :: Word16
        n4 = 4  :: Word16
        n5 = 62000  :: Word16
     in assertEqual "5" ("\NUL\ENQ") (encode n1) >>
        assertEqual "10" ("\NUL\n") (encode n2) >>
        assertEqual "1" ("\NUL\SOH") (encode n3) >>
        assertEqual "4" ("\NUL\EOT") (encode n4) >>
        assertEqual "62000" ("\242\&0") (encode n5)
