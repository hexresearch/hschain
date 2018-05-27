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
          bracket accept (close . fst) $ \(s,_) -> do
            Just bs <- recvBS s 4096
            sendBS s ("PONG_" <> bs)
  let runClient NetworkAPI{..} = do
        threadDelay 10e3
        bracket (connect (serverAddr,"3000")) close $ \s -> do
          sendBS s "PING"
          bs <- recvBS s 4096
          assertEqual "Ping-pong" (Just "PONG_PING") bs
  ((),()) <- concurrently (runServer server) (runClient client)
  return ()



-- | Simple test to ensure that mock network works at all
delayedWrite :: IO ()
delayedWrite = do
  network <- newMockNet
  let serverAddr = 1 :: Int
      clientAddr = 2 :: Int
      server     = createMockNode network "3000" serverAddr
      client     = createMockNode network "3000" clientAddr
  --
  let runServer NetworkAPI{..} = do
        bracket listenOn fst $ \(_,accept) ->
          bracket accept (close . fst) $ \(s,_) -> do
            Just "A1" <- recvBS s 4096
            Just "A2" <- recvBS s 4096
            Just "A3" <- recvBS s 4096
            return ()
  let runClient NetworkAPI{..} = do
        threadDelay 10e3
        bracket (connect (serverAddr,"3000")) close $ \s -> do
          sendBS s "A1"
          threadDelay 30e3
          sendBS s "A2"
          threadDelay 30e3
          sendBS s "A3"
  ((),()) <- concurrently (runServer server) (runClient client)
  return ()

