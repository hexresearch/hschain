{-# LANGUAGE NumDecimals       #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

-- |
module TM.NetworkTls (tests) where

import Control.Concurrent         (threadDelay)
import Control.Concurrent.Async
import Control.Exception
import Data.ByteString.Lazy.Char8 as LBC
import Data.Monoid                ((<>))
import Test.Tasty
import Test.Tasty.HUnit
import Thundermint.P2P.Network
import TM.MockNet
import TM.RealNetwork

import qualified Data.ByteString.Lazy as LBS

tests :: TestTree
tests =
    testGroup "Tls network test"
                  [ testGroup "IPv4"
                          [ testCase "tls ping-pong" $ realTlsNetPair "127.0.0.1" >>= \(server, client) -> pingPong server client
                          , testCase "tls delayed write" $ realTlsNetPair "127.0.0.1" >>= \(server, client) -> delayedWrite server client
                          ]
                    , testGroup "IPv6"
                          [ testCase "tls ping-pong" $ realTlsNetPair "::1" >>= \(server, client) -> pingPong server client
                          , testCase "tls delayed write" $ realTlsNetPair "::1" >>= \(server, client) -> delayedWrite server client
                          ]
                  ]



-- | used to run from ghci
run :: IO ()
run = do
    (server, client) <- realTlsNetPair "localhost"
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
            send conn (LBS.fromStrict $  "PONG_" <> LBC.toStrict bs)
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
