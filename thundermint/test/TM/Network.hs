{-# LANGUAGE NumDecimals         #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-} {- allow useful top functions for GHCi -}


-- |
module TM.Network (tests) where


import Control.Concurrent.Async

import Control.Concurrent     (threadDelay)
import Control.Monad          (forM_)

import Data.Monoid            ((<>))
import Data.String            (fromString)

import           Control.Exception as E
import qualified Network.Socket    as Net

import Thundermint.P2P
import Thundermint.P2P.Network
import Thundermint.Types.Network

import Test.Tasty
import Test.Tasty.HUnit
import Test.QuickCheck
import TM.MockNet
import TM.Util.Network

tests :: TestTree
tests =
    testGroup "network test"
                  [ testGroup "mock"
                    [ testCase "ping-pong" $ mockNetPair >>= uncurry pingPong
                    , testCase "delayed write" $ mockNetPair >>= uncurry delayedWrite
                    ]
                  , testGroup "NetAddr"
                    [ testCase "tupleToHostAddress" $ do
                        a <- generate arbitrary
                        Net.tupleToHostAddress a @=? tupleToHostAddress a
                    , testCase "hostAddressToTuple" $ do
                        a <- generate arbitrary
                        Net.hostAddressToTuple a @=? hostAddressToTuple a
                    , testCase "tupleToHostAddress6" $ do
                        a <- generate arbitrary
                        Net.tupleToHostAddress6 a @=? tupleToHostAddress6 a
                    , testCase "hostAddress6ToTuple" $ do
                        a <- generate arbitrary
                        Net.hostAddress6ToTuple a @=? hostAddress6ToTuple a
                    ]
                  , testGroup "real"
                    [ testGroup "IPv4"
                         [ testCase "ping-pong" $ withRetry pingPong "127.0.0.1"
                         , testCase "delayed write" $ withRetry delayedWrite "127.0.0.1"
                         ]
                    , testGroup "IPv6"
                          [ testCase "ping-pong" $ withRetry pingPong "::1"
                          , testCase "delayed write" $ withRetry delayedWrite "::1"
                          ]
                    ]
                  , testGroup "real-udp"
                    [ testGroup group $
                         [ testCase "ping-pong" $ withRetry' (Just Nothing) pingPong address
                         , testCase "delayed write" $ withRetry' (Just Nothing) delayedWrite address
                         , testCase "sized ping pongs" $ withRetry' (Just $ Just $ 123 + v6) (sizedPingPong 8 11) address
                         ]
                    | (group, address, v6) <- [("IPv4", "127.0.0.1", 0)]--, ("IPv6", "::1", 1)]
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


-- | Simple test to ensure that mock network works at all
pingPong :: (NetAddr, NetworkAPI)
         -> (NetAddr, NetworkAPI)
         -> IO ()
pingPong (serverAddr, server) (_clientAddr, client) = do
  let runServer NetworkAPI{..} = do
        bracket listenOn fst $ \(_,accept) ->
          bracket accept (close . fst) $ \(conn,_) -> do
            bs <- skipNothings "ping pong server" recv conn
            send conn ("PONG_" <> bs)
  let runClient NetworkAPI{..} = do
        threadDelay 10e3
        bracket (connect serverAddr) close $ \conn -> do
          send conn "PING"
          bs <- skipNothings "ping pong client" recv conn
          assertEqual "Ping-pong" ("PONG_PING") bs
  ((),()) <- concurrently (runServer server) (runClient client)
  return ()

-- | Ping pong test parametrized by message size.
sizedPingPong :: Int
         -> Int
         -> (NetAddr, NetworkAPI)
         -> (NetAddr, NetworkAPI)
         -> IO ()
sizedPingPong startPower endPower (serverAddr, server) (_clientAddr, client) = do
  let powers = [startPower..endPower]
      runServer NetworkAPI{..} = do
        bracket listenOn fst $ \(_,accept) ->
          bracket accept (close . fst) $ \(conn,_) -> do
            forM_ powers $ \_ -> do
              bs <- skipNothings "server" recv conn
              send conn ("PONG_" <> bs)
      runClient NetworkAPI{..} = do
        threadDelay 10e3
        bracket (connect serverAddr) close $ \conn -> do
          forM_ powers $ \power -> do
            let messageSize = 2 ^ power
                block = fromString [ toEnum $ fromEnum ' ' + mod i 64 | i <- [1..messageSize]]
                msg = "PING" <> block
            send conn msg
            bs <- skipNothings "client" recv conn
            assertEqual ("Ping-pong power " ++ show power) ("PONG_" <> msg) bs
  ((),()) <- concurrently (runServer server) (runClient client)
  return ()
