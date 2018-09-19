{-# LANGUAGE NumDecimals       #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-} {- allow useful top functions for GHCi -}

-- |
module TM.NetworkTls (tests) where


import Control.Concurrent.Async
import Control.Exception
import Test.Tasty
import Test.Tasty.HUnit
import Thundermint.P2P.Network
import TM.RealNetwork

import Control.Concurrent (threadDelay)
import Data.Monoid        ((<>))

import qualified Data.ByteString.Lazy       as LBS
import           Data.ByteString.Lazy.Char8 as LBC

tests :: TestTree
tests =
    testGroup "Tls network test"
                  [ testGroup "IPv4"
                          [ testCase "tls ping-pong" $ realTlsNetPair "127.0.0.1" >>= uncurry pingPong
                          , testCase "tls delayed write" $ realTlsNetPair "127.0.0.1" >>= uncurry delayedWrite
                          , testCase "tls framing: send big data" $ realTlsNetPair "127.0.0.1" >>= uncurry bigDataSend
                          ]
                    , testGroup "IPv6"
                          [ testCase "tls ping-pong" $ realTlsNetPair "::1" >>= \(server, client) -> catch  (pingPong server client) suppressIOException
                          , testCase "tls delayed write" $ realTlsNetPair "::1" >>= \(server, client) -> catch (delayedWrite server client) suppressIOException
                          , testCase "tls framing: send big data" $ realTlsNetPair "::1" >>= \(server, client) -> catch (bigDataSend server client) suppressIOException
                          ]
                  ]



-- | used to run from ghci
run :: IO ()
run = do
    (server, client) <- realTlsNetPair "localhost"
    pingPong server client

-- | Simple test to ensure that real network works at all
pingPong :: (addr, NetworkAPI addr)
         -> (addr, NetworkAPI addr)
         -> IO ()
pingPong (serverAddr, server) (_, client) = do
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
bigDataSend :: (addr, NetworkAPI addr)
         -> (addr, NetworkAPI addr)
         -> IO ()
bigDataSend (serverAddr, server) (_, client) = do
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
