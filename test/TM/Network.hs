{-# LANGUAGE NumDecimals       #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-} {- allow useful top functions for GHCi -}

-- |
module TM.Network (tests) where

import Control.Concurrent.Async
import Control.Exception
import Data.Default.Class        (def)
import Data.Monoid               ((<>))
import qualified Network.Socket  as Net

import Thundermint.P2P.Network
import TM.MockNet
import TM.RealNetwork

import Control.Concurrent (threadDelay)
import Data.Monoid        ((<>))


tests :: TestTree
tests =
    let opts = def { allowConnectFromLocal = True } in
    testGroup "network test"
                  [ testGroup "mock"
                    [ testCase "ping-pong" $ mockNetPair >>= \(server, client) -> pingPong server client
                    , testCase "delayed write" $ mockNetPair >>= \(server, client) -> delayedWrite server client
                    ]
                  , testGroup "real"
                    [ testGroup "IPv4"
                          [ testCase "ping-pong" $ realNetPair opts "127.0.0.1" >>= \(server, client) -> pingPong server client
                          , testCase "delayed write" $ realNetPair opts "127.0.0.1" >>= \(server, client) -> delayedWrite server client
                          ]
                    , testGroup "IPv6"
                          [ testCase "ping-pong" $ realNetPair opts "::1" >>= \(server, client) -> pingPong server client
                          , testCase "delayed write" $ realNetPair opts "::1" >>= \(server, client) -> delayedWrite server client
                          ]
                    ]
                  ]



-- | used to run from ghci
run :: RealNetworkConnectOptions -> IO ()
run opts = do
    (server, client) <- realNetPair opts "localhost"
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
