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
import Control.Monad.Catch    (throwM)
import Control.Monad.IO.Class (liftIO)
import Control.Retry          (RetryPolicy, RetryStatus(..), recoverAll)
import Data.Monoid            ((<>))

import           Control.Exception          as E
import qualified Data.ByteString.Lazy       as LBS
import           Data.ByteString.Lazy.Char8 as LBC
import qualified Network.Socket             as Net

import Thundermint.P2P.Network

import TM.RealNetwork
import TM.Util.Network



tests :: TestTree
tests =
    testGroup "Tls network test"
                  [ testGroup "IPv4"
                          [ testCase "tls ping-pong" $ pingPong  "127.0.0.1"
                          , testCase "tls delayed write" $ delayedWrite "127.0.0.1"
                          , testCase "tls framing: send big data" $ bigDataSend "127.0.0.1"
                          ]
                    , testGroup "IPv6"
                          [ testCase "tls ping-pong" $ pingPong "::1"
                          , testCase "tls delayed write" $ delayedWrite  "::1"
                          , testCase "tls framing: send big data" $ bigDataSend "::1"
                          ]
                  ]



-- add retry logic  to find free port to listen
pingPong :: Net.HostName -> IO ()
pingPong host = do
  liftIO $ recoverAll retryPolicy $ \RetryStatus{..} -> realNetPair host >>= \(server, client) -> pingPong' server client
                                                      `E.catch` (\(ex :: E.IOException) -> do
                                                                   throwM ex
                                                                )
-- add retry logic  to find free port to listen
delayedWrite :: Net.HostName -> IO ()
delayedWrite host = do
  liftIO $ recoverAll retryPolicy $ \RetryStatus{..} -> realNetPair host >>= \(server, client) -> delayedWrite' server client
                                                      `E.catch` (\(ex :: E.IOException) -> do
                                                                   throwM ex
                                                                )
-- add retry logic  to find free port to listen
bigDataSend :: Net.HostName -> IO ()
bigDataSend host = do
  liftIO $ recoverAll retryPolicy $ \RetryStatus{..} -> realNetPair host >>= \(server, client) -> bigDataSend' server client
                                                      `E.catch` (\(ex :: E.IOException) -> do
                                                                   throwM ex
                                                                )



-- | Simple test to ensure that real network works at all
pingPong' :: (addr, NetworkAPI addr)
         -> (addr, NetworkAPI addr)
         -> IO ()
pingPong' (serverAddr, server) (_, client) = do
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
bigDataSend' :: (addr, NetworkAPI addr)
         -> (addr, NetworkAPI addr)
         -> IO ()
bigDataSend' (serverAddr, server) (_, client) = do
  let runServer NetworkAPI{..} =
        bracket listenOn fst $ \(_,accept) ->
          bracket accept (close . fst) $ \(conn,_) -> do
            Just bs <- recv conn
            send conn (LBS.fromStrict $  "PONG_" <> LBC.toStrict bs)
  let runClient NetworkAPI{..} = do
        threadDelay 10e3
        bracket (connect serverAddr) close $ \conn -> do
          let sbuf0 = LBC.take 2100 $ LBC.repeat 'A'
          let sbuf1 = LBC.take 1000 $ LBC.repeat 'B'
          let sbuf2 = LBC.take 3000 $ LBC.repeat 'C'
          let sbuf = sbuf0 <> sbuf1 <> sbuf2
          send conn sbuf
          bs <- recv conn
          assertEqual "Ping-pong" (Just ("PONG_" <> sbuf)) bs
  ((),()) <- concurrently (runServer server) (runClient client)
  return ()
