{-# LANGUAGE NumDecimals         #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-} {- allow useful top functions for GHCi -}


-- |
module TM.Network (tests) where


import Control.Concurrent.Async

import Control.Concurrent     (threadDelay)
import Control.Monad.Catch    (bracketOnError, onException, throwM)
import Control.Monad.IO.Class (liftIO)
import Control.Retry          (RetryPolicy, RetryStatus(..), recoverAll)
import Data.Monoid            ((<>))

import           Control.Exception as E
import qualified Network.Socket    as Net

import Thundermint.P2P.Network

import Test.Tasty
import Test.Tasty.HUnit
import TM.MockNet
import TM.RealNetwork
import TM.Util.Network

tests :: TestTree
tests =
    testGroup "network test"
                  [ testGroup "mock"
                    [ testCase "ping-pong" $ mockNetPair >>= \(server, client) -> pingPong' server client
                    , testCase "delayed write" $ mockNetPair >>= \(server, client) -> delayedWrite' server client
                    ]
                  , testGroup "real"
                    [ testGroup "IPv4"
                         [ testCase "ping-pong" $ pingPong "127.0.0.1"
                         , testCase "delayed write" $ delayedWrite "127.0.0.1"
                          ]
                    , testGroup "IPv6"
                          [ testCase "ping-pong" $ pingPong "::1"
                          , testCase "delayed write" $ delayedWrite "::1"
                          ]
                    ]
                  , testGroup "local addresses detection"
                    [ testCase "all locals must be local" $ getLocalAddresses >>= (fmap and . mapM isLocalAddress) >>= (@? "Must be local")
                    , testCase "loopback is local" $ (and <$> mapM isLocalAddress [loopbackIpv4, loopbackIpv6]) >>= (@? "Must be local")
                    -- TODO: Randomly generate addresses and check it is not isLocalAddress

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
                                                      `E.catch` (\(ex :: E.IOException)-> do
                                                                   throwM ex
                                                                )


loopbackIpv4, loopbackIpv6 :: Net.SockAddr
loopbackIpv4 = Net.SockAddrInet  50000 0x100007f
loopbackIpv6 = Net.SockAddrInet6 50000 0 (0,0,0,1) 0



-- | Simple test to ensure that mock network works at all
pingPong' :: (addr, NetworkAPI addr)
         -> (addr, NetworkAPI addr)
         -> IO ()
pingPong' (serverAddr, server) (_, client) = do
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
