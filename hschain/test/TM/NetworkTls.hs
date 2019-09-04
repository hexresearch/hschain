{-# LANGUAGE NumDecimals         #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-} {- allow useful top functions for GHCi -}

-- |
module TM.NetworkTls (tests) where

import Control.Concurrent.Async
import Test.Tasty
import Test.Tasty.HUnit

import Control.Concurrent     (threadDelay)
import Data.Monoid            ((<>))

import           Control.Exception          as E
import qualified Data.ByteString.Lazy       as LBS
import qualified Data.ByteString.Lazy.Char8 as LBC

import HSChain.P2P.Network

import TM.Util.Network



tests :: TestTree
tests =
    testGroup "Tls network test"
                  [ testGroup group
                          [ testCase "tls ping-pong" $ withRetryTLS address pingPong
                          , testCase "tls delayed write" $ withRetryTLS address delayedWrite
                          , testCase "tls framing: send big data" $ withRetryTLS address bigDataSend
                          ]
                  | (group, address) <- [("IPv4", "127.0.0.1"), ("IPv6", "::1")]
                  ]


-- | Simple test to ensure that real network works at all
pingPong :: NetPair
         -> IO ()
pingPong ((serverAddr, server), (_, client)) = do
  let runServer NetworkAPI{..} =
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


-- | Simple test to ensure that framing works
bigDataSend :: NetPair
            -> IO ()
bigDataSend ((serverAddr, server), (_, client)) = do
  let runServer NetworkAPI{..} =
        bracket listenOn fst $ \(_,accept) ->
          bracket accept (close . fst) $ \(conn,_) -> do
            Just bs <- recv conn
            send conn (LBS.fromStrict $  "PONG_" <> LBC.toStrict bs)
  let runClient NetworkAPI{..} = do
        threadDelay 10e3
        bracket (connect serverAddr) close $ \conn -> do
          let sbuf0 = LBC.take 21000 $ LBC.repeat 'A'
          let sbuf1 = LBC.take 10000 $ LBC.repeat 'B'
          let sbuf2 = LBC.take 30000 $ LBC.repeat 'C'
          let sbuf = sbuf0 <> sbuf1 <> sbuf2
          send conn sbuf
          bs <- recv conn
          assertEqual "Ping-pong" (Just ("PONG_" <> sbuf)) bs
  ((),()) <- concurrently (runServer server) (runClient client)
  return ()
