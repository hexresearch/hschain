{-# LANGUAGE NumDecimals         #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-} {- allow useful top functions for GHCi -}


-- |
module TM.Network (tests) where


import Control.Concurrent.Async

import Control.Concurrent (threadDelay)
import Control.Monad      (forM_)
import Data.Monoid        ((<>))
import Data.String        (fromString)

import Control.Exception as E

import qualified Network.Socket as Net

import HSChain.P2P.Network
import HSChain.Types.Network
import HSChain.P2P.Network.IpAddresses (isLocalAddress)

import Test.QuickCheck
import Test.Tasty
import Test.Tasty.HUnit
import TM.RealNetwork
import TM.Util.Network

tests :: TestTree
tests = testGroup "network test"
  [ testGroup "NetAddr"
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
  , testGroup "local addresses detection"
    [ testCase "all locals must be local"
    $ getLocalAddresses >>= (fmap and . mapM isLocalAddress) >>= (@? "Must be local")
    , testCase "loopback is local"
    $ (and <$> mapM isLocalAddress [loopbackIpv4, loopbackIpv6]) >>= (@? "Must be local")
      -- TODO: Randomly generate addresses and check it is not isLocalAddress
    ]
  , testGroup "NetworkAPI"
    [ testGroup "mock"
      [ testCase "ping-pong"     $ mockNetPair >>= pingPong
      , testCase "delayed write" $ mockNetPair >>= delayedWrite
      ]
    , testGroup "real"
      [ testGroup group
        [ testCase "ping-pong" $ withRetryTCP address pingPong
        , testCase "delayed write" $ withRetryTCP address delayedWrite
        ]
      | (group, address) <- [("IPv4", "127.0.0.1"), ("IPv6", "::1")]
      ]
    , testGroup "real-udp"
      [ testGroup group $
        [ testCase "ping-pong"        $ withTimeoutRetry "ping-pong"        10e6 (newNetPair (Just Nothing)) pingPong
        , testCase "delayed write"    $ withTimeoutRetry "delayed wrote"    10e6 (newNetPair (Just Nothing)) delayedWrite
        , testCase "sized ping pongs" $ withTimeoutRetry "sized ping pongs" 10e6 (newNetPair (Just $ Just $ 123 + v6)) (sizedPingPong 8 11)
        ]
      | (group, newNetPair, v6) <- [ ("IPv4", (`realNetPair` "127.0.0.1"), 0)
                                   , ("IPv6", (`realNetPair`  "::1"), 1)]
      ]
    ]
  ]



loopbackIpv4, loopbackIpv6 :: NetAddr
loopbackIpv4 = NetAddrV4 0x100007f 50000
loopbackIpv6 = NetAddrV6 (0,0,0,1) 50000


-- | Simple test to ensure that mock network works at all
pingPong :: NetPair
         -> IO ()
pingPong ((serverAddr, server), (_clientAddr, client)) = do
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
              -> NetPair
              -> IO ()
sizedPingPong startPower endPower ((serverAddr, server), (_clientAddr, client)) = do
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


----------------------------------------------------------------
-- Create network API
----------------------------------------------------------------

mockNetPair :: IO ( (NetAddr, NetworkAPI)
                  , (NetAddr, NetworkAPI))
mockNetPair = do
  network <- newMockNet
  return ( (a1, createMockNode network a1)
         , (a2, createMockNode network a2)
         )
  where
    a1 = NetAddrV4 40001 2222
    a2 = NetAddrV4 10004 2222
