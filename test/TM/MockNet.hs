{-# LANGUAGE NumDecimals #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
-- |
module TM.MockNet (tests) where

import Control.Exception
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async
import Data.Monoid ((<>))
import Test.Tasty
import Test.Tasty.HUnit
import Thundermint.P2P.Network


----------------------------------------------------------------

tests :: TestTree
tests = testGroup "mock network"
  [ testCase "ping-pong"     pingPong
  , testCase "delayed write" delayedWrite
  ]

-- | Simple test to ensure that mock network works at all
pingPong :: IO ()
pingPong = do
  network <- newMockNet
  let serverAddr = 1 :: Int
      clientAddr = 2 :: Int
      server     = createMockNode network "3000" serverAddr
      client     = createMockNode network "3000" clientAddr
  --
  let runServer NetworkAPI{..} = do
        bracket listenOn fst $ \(_,accept) ->
          bracket accept (close . fst) $ \(conn,_) -> do
            Just bs <- recv conn
            send conn ("PONG_" <> bs)
  let runClient NetworkAPI{..} = do
        threadDelay 10e3
        bracket (connect (serverAddr,"3000")) close $ \conn -> do
          send conn "PING"
          bs <- recv conn
          assertEqual "Ping-pong" (Just "PONG_PING") bs
  ((),()) <- concurrently (runServer server) (runClient client)
  return ()



-- | Simple test to ensure that mock network works at all
delayedWrite :: NetworkAPI addr -> IO ()
delayedWrite network = do
  network <- newMockNet
  let serverAddr = 1 :: Int
      clientAddr = 2 :: Int
      server     = createMockNode network "3000" serverAddr
      client     = createMockNode network "3000" clientAddr
  --
  let runServer NetworkAPI{..} =
        bracket listenOn fst $ \(_,accept) ->
          bracket accept (close . fst) $ \(conn,_) -> do
            Just "A1" <- recv conn
            Just "A2" <- recv conn
            Just "A3" <- recv conn
            return ()
  let runClient NetworkAPI{..} = do
        threadDelay 10e3
        bracket (connect (serverAddr,"3000")) close $ \conn -> do
          send conn "A1"
          threadDelay 30e3
          send conn "A2"
          threadDelay 30e3
          send conn "A3"
  ((),()) <- concurrently (runServer server) (runClient client)
  return ()
