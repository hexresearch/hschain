{-# LANGUAGE NumDecimals       #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-} {- allow useful top functions for GHCi -}

-- |
module TM.Network (tests) where

import Control.Concurrent        (threadDelay)
import Control.Concurrent.Async
import Control.Exception
import Data.Monoid               ((<>))
import qualified Network.Socket  as Net

import Thundermint.P2P.Network

import TM.MockNet
import TM.RealNetwork
import Test.Tasty
import Test.Tasty.HUnit


tests :: TestTree
tests =
    testGroup "network test"
                  [ testGroup "mock"
                    [ testCase "ping-pong" $ mockNetPair >>= \(server, client) -> pingPong server client
                    , testCase "delayed write" $ mockNetPair >>= \(server, client) -> delayedWrite server client
                    ]
                  , testGroup "real"
                    [ testGroup "IPv4"
                          [ testCase "ping-pong" $ realNetPair "127.0.0.1" >>= \(server, client) -> pingPong server client
                          , testCase "delayed write" $ realNetPair "127.0.0.1" >>= \(server, client) -> delayedWrite server client
                          ]
                    , testGroup "IPv6"
                          [ testCase "ping-pong" $ realNetPair "::1" >>= \(server, client) -> pingPong server client
                          , testCase "delayed write" $ realNetPair "::1" >>= \(server, client) -> delayedWrite server client
                          ]
                    ]
                  , testGroup "local addresses detection"
                    [ testCase "all locals must be local" $ getLocalAddresses >>= (fmap and . mapM isLocalAddress) >>= (@? "Must be local")
                    , testCase "loopback is local" $ (and <$> mapM isLocalAddress [loopbackIpv4, loopbackIpv6]) >>= (@? "Must be local")
                    -- TODO: Randomly generate addresses and check it is not isLocalAddress

                    ]
                  ]


loopbackIpv4, loopbackIpv6 :: Net.SockAddr
loopbackIpv4 = Net.SockAddrInet  50000 0x100007f
loopbackIpv6 = Net.SockAddrInet6 50000 0 (0,0,0,1) 0

-- | used to run from ghci
run :: IO ()
run = do
    (server, client) <- realNetPair "localhost"
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
