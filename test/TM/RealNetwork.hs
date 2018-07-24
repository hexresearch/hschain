{-# LANGUAGE NumDecimals       #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
-- |
module TM.RealNetwork (tests) where

import Control.Concurrent.Async
import Control.Exception
import Data.Int
import System.Random
import Test.Tasty
import Test.Tasty.HUnit
import Thundermint.P2P.Network

import Control.Concurrent (threadDelay)
import Data.Monoid        ((<>))


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
   testCase "ping-pong"  pingPong
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
