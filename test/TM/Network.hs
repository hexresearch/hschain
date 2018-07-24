{-# LANGUAGE NumDecimals       #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

-- |
module TM.Network (tests) where

import Control.Concurrent.Async
import Control.Exception
import Test.Tasty
import Test.Tasty.HUnit
import Thundermint.P2P.Network
import TM.MockNet
import TM.RealNetwork

import Control.Concurrent (threadDelay)
import Data.Monoid        ((<>))


tests :: TestTree
tests =  testGroup "network tests"
             [ testCase "mock ping-pong" $ mockNetPair >>= \(server, client) -> pingPong server client
             , testCase "mock delayed write" $ mockNetPair >>= \(server, client) -> delayedWrite server client
             , testCase "real ping-pong" $ realNetPair >>= \(server, client) -> pingPong server client
             , testCase "real delayed write " $ realNetPair >>= \(server, client) -> delayedWrite server client
             ]



-- | used to run from ghci
run :: IO ()
run = do
    (server, client) <- realNetPair
    pingPong server client

-- | Simple test to ensure that mock network works at all
pingPong :: (addr, NetworkAPI addr)
         -> (addr, NetworkAPI addr)
         -> IO ()
pingPong (serverAddr, server) (_, client) = do
  let runServer NetworkAPI{..} = do
        bracket listenOn fst $ \(_,accept) ->
          bracket accept (close . fst) $ \(conn,_) -> do
            Just bs <- recv conn
            send conn ("PONG_" <> bs)
  let runClient NetworkAPI{..} = do
        threadDelay 10e3
        bracket (connect serverAddr) close $ \conn -> do
          send conn "PING"
          bs <- recv conn
          assertEqual "Ping-pong" (Just "PONG_PING") bs
  ((),()) <- concurrently (runServer server) (runClient client)
  return ()



-- | Simple test to ensure that mock network works at all
delayedWrite :: (addr, NetworkAPI addr)
         -> (addr, NetworkAPI addr)
         -> IO ()
delayedWrite (serverAddr, server) (_, client) = do
  let runServer NetworkAPI{..} =
        bracket listenOn fst $ \(_,accept) ->
          bracket accept (close . fst) $ \(conn,_) -> do
            Just "A1" <- recv conn
            Just "A2" <- recv conn
            Just "A3" <- recv conn
            return ()
  let runClient NetworkAPI{..} = do
        threadDelay 10e3
        bracket (connect serverAddr) close $ \conn -> do
          send conn "A1"
          threadDelay 30e3
          send conn "A2"
          threadDelay 30e3
          send conn "A3"
  ((),()) <- concurrently (runServer server) (runClient client)
  return ()
