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

import HSChain.P2P.Network
import HSChain.Network.Types
import HSChain.P2P.Network.IpAddresses (isLocalAddress,getLocalAddresses)

import Test.Tasty
import Test.Tasty.HUnit
import TM.RealNetwork


----------------------------------------------------------------
-- Test tree
----------------------------------------------------------------

tests :: TestTree
tests = testGroup "network test"
  [ testGroup "local addresses detection"
    [ testCase "all locals must be local"
    $ getLocalAddresses >>= (fmap and . mapM isLocalAddress) >>= (@? "Must be local")
    , testCase "loopback is local"
    $ (and <$> mapM isLocalAddress [loopbackIpv4, loopbackIpv6]) >>= (@? "Must be local")
      -- TODO: Randomly generate addresses and check it is not isLocalAddress
    ]
  , testGroup "NetworkAPI"
    [ testGroup "MockNet"
      [ testCase "ping-pong"     $ mockNetPair >>= pingPong
      , testCase "delayed write" $ mockNetPair >>= delayedWrite
      ]
    , testGroup "TCP"
    $ let withRetryTCP addr fun = withTimeOut 10e6 $ withRetry $ realNetPair Nothing addr >>= fun
      in [ testGroup group
           [ testCase "ping-pong"     $ withRetryTCP address pingPong
           , testCase "delayed write" $ withRetryTCP address delayedWrite
           ]
         | (group, address) <- [("IPv4", "127.0.0.1"), ("IPv6", "::1")]
         ]
    , testGroup "UDP"
    $ let withRetryUDP = withTimeOut 10e6 . withRetry
      in [ testGroup group $
           [ testCase "ping-pong"
           $ withRetryUDP $ newNetPair (Just Nothing) >>= pingPong
           , testCase "delayed write"
           $ withRetryUDP $ newNetPair (Just Nothing) >>= delayedWrite
           , testCase "sized ping pongs"
           $ withRetryUDP $ newNetPair (Just $ Just $ 123 + v6) >>= sizedPingPong 8 11
           ]
         | (group, newNetPair, v6) <- [ ("IPv4", (`realNetPair` "127.0.0.1"), 0)
                                      , ("IPv6", (`realNetPair`  "::1"), 1)]
         ]
    , testGroup "TLS"
    $ let withRetryTLS addr fun = withRetry $ realTlsNetPair addr >>= fun
      in [ testGroup group
           [ testCase "ping-pong"        $ withRetryTLS address pingPong
           , testCase "delayed write"    $ withRetryTLS address delayedWrite
           , testCase "sized ping pongs" $ withRetryTLS address (sizedPingPong 8 11)
           ]
         | (group, address) <- [("IPv4", "127.0.0.1"), ("IPv6", "::1")]
         ]
    ]
  ]


----------------------------------------------------------------
-- Tests implementations
----------------------------------------------------------------

-- | Simple test to ensure that mock network works at all
delayedWrite :: NetPair -> IO ()
delayedWrite ((serverAddr, server), (_, client)) = do
  let runServer NetworkAPI{..} =
        bracket listenOn fst $ \(_,accept) ->
          bracket accept (close . fst) $ \(conn,_) -> do
            "A1" <- recv conn
            "A2" <- recv conn
            "A3" <- recv conn
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


-- | Simple test to ensure that mock network works at all
pingPong :: NetPair
         -> IO ()
pingPong ((serverAddr, server), (_clientAddr, client)) = do
  let runServer NetworkAPI{..} = do
        bracket listenOn fst $ \(_,accept) ->
          bracket accept (close . fst) $ \(conn,_) -> do
            bs <- recv conn
            send conn ("PONG_" <> bs)
  let runClient NetworkAPI{..} = do
        threadDelay 10e3
        bracket (connect serverAddr) close $ \conn -> do
          send conn "PING"
          bs <- recv conn
          assertEqual "Ping-pong" ("PONG_PING") bs
  ((),()) <- concurrently (runServer server) (runClient client)
  return ()


-- | Ping pong test parametrized by message size.
sizedPingPong :: Int
              -> Int
              -> NetPair
              -> IO ()
sizedPingPong startPower endPower ((serverAddr, server), (_clientAddr, client)) = do
  let powers = [startPower .. endPower]
      runServer NetworkAPI{..} = do
        bracket listenOn fst $ \(_,accept) ->
          bracket accept (close . fst) $ \(conn,_) -> do
            forM_ powers $ \_ -> do
              bs <- recv conn
              send conn ("PONG_" <> bs)
      runClient NetworkAPI{..} = do
        threadDelay 10e3
        bracket (connect serverAddr) close $ \conn -> do
          forM_ powers $ \power -> do
            let messageSize = 2 ^ power
                block = fromString [ toEnum $ fromEnum ' ' + mod i 64 | i <- [1..messageSize]]
                msg = "PING" <> block
            send conn msg
            bs <- recv conn
            assertEqual ("Ping-pong power " ++ show power) ("PONG_" <> msg) bs
  ((),()) <- concurrently (runServer server) (runClient client)
  return ()


----------------------------------------------------------------
-- Create network API
----------------------------------------------------------------

loopbackIpv4, loopbackIpv6 :: NetAddr
loopbackIpv4 = NetAddrV4 0x100007f 50000
loopbackIpv6 = NetAddrV6 (0,0,0,1) 50000

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
