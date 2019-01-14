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
import qualified Data.ByteString.Lazy as LBS

import           Control.Exception as E
import qualified Network.Socket    as Net

import Thundermint.P2P.Network

import Test.Tasty
import Test.Tasty.HUnit
import TM.MockNet
import TM.Util.Network

tests :: TestTree
tests =
    testGroup "network test"
                  [ testGroup "mock"
                    [ testCase "ping-pong" $ mockNetPair >>= uncurry pingPong
                    , testCase "delayed write" $ mockNetPair >>= uncurry delayedWrite
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
                         [ {-testCase "ping-pong" $ withRetry' (Just Nothing) pingPong address
                         , testCase "delayed write" $ withRetry' (Just Nothing) delayedWrite address
                         , -}testCase "sized ping pongs" $ withRetry' (Just $ Just $ 123 + v6) (sizedPingPong 1 20) address
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
pingPong :: (addr, NetworkAPI addr)
         -> (addr, NetworkAPI addr)
         -> IO ()
pingPong (serverAddr, server) (clientAddr, client) = do
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

-- | Ping pong test parametrized by message size.
sizedPingPong :: Show addr => Int
         -> Int
         -> (addr, NetworkAPI addr)
         -> (addr, NetworkAPI addr)
         -> IO ()
sizedPingPong startPower endPower (serverAddr, server) (clientAddr, client) = do
  let powers = [startPower..endPower]
      skipNothings lbl recv conn = do
        dumpStrLn $ "at skipNothings for "++show lbl
        mbMsg <- recv conn
        dumpStrLn $ "skip nothings result "++show (fmap LBS.length mbMsg)
        case mbMsg of
          Just msg -> return msg
          Nothing -> skipNothings lbl recv conn
      runServer NetworkAPI{..} = do
        bracket listenOn fst $ \(_,accept) ->
          bracket accept (close . fst) $ \(conn,_) -> do
            forM_ powers $ \_ -> do
              bs <- skipNothings "server" recv conn
              send conn ("PONG_" <> bs)
      runClient NetworkAPI{..} = do
        threadDelay 10e3
        dumpStrLn $ "sending from "++show clientAddr
        bracket (connect serverAddr) close $ \conn -> do
          forM_ powers $ \power -> do
            let messageSize = 2 ^ power
                block = fromString [ toEnum $ fromEnum ' ' + mod i 64 | i <- [1..messageSize]]
                msg = "PING" <> block
            send conn msg
            dumpStrLn $ "sent "++show (LBS.length msg)
            bs <- skipNothings "client" recv conn
            dumpStrLn $ "sent "++show (LBS.length msg)++", received "++show (LBS.length bs)
            assertEqual ("Ping-pong power " ++ show power) ("PONG_" <> msg) bs
  dumpStrLn $ "client address "++show (clientAddr)
  ((),()) <- concurrently (runServer server) (runClient client)
  return ()
