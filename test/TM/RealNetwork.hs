{-# LANGUAGE NumDecimals #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
-- |
module TM.RealNetwork (tests) where

import           Control.Exception
import           Control.Concurrent (threadDelay)
import           Control.Concurrent.Async
import           Data.Monoid ((<>))
import           Test.Tasty
import           Test.Tasty.HUnit
import           Thundermint.P2P.Network
import qualified Network.Socket as Net
import qualified Data.ByteString as BS
import qualified Network.Socket.ByteString as NetBS
----------------------------------------------------------------

run :: IO ()
run = defaultMain $ testGroup "test suite"
  [ tests
  ]


tests :: TestTree
tests = testGroup "real network"
  [ testCase "ping-pong"  pingPong
  , testCase "framing" framing
  ]


-- | Simple test on real network
pingPong :: IO ()
pingPong = do
  let port = "3000"
      host = "127.0.0.1"
      server = realNetwork port
      hints = Net.defaultHints  { Net.addrSocketType = Net.Stream }
  addr:_ <- Net.getAddrInfo (Just hints) (Just host) (Just port)
  let sockAddr = Net.addrAddress addr
  --
  let runServer NetworkAPI{..} = do
        bracket listenOn fst $ \(_,accept) ->
          bracket (accept) (close . fst) $ \(conn,_) -> do
            Just bs <- recv conn
            send conn ("PONG_" <> bs)
  let runClient NetworkAPI{..}  = do
        threadDelay 10e3
        bracket (connect sockAddr) close $ \conn -> do
          send conn "PING"
          bs <- recv conn
          assertEqual "Ping-pong" (Just "PONG_PING") bs
  ((),()) <- concurrently (runServer server) (runClient server)
  return ()



-- | test on framing
framing :: IO ()
framing = do
  let port = "3001"
      host = "127.0.0.1"
      server = realNetwork port
      hints = Net.defaultHints  { Net.addrSocketType = Net.Stream }
  addr:_ <- Net.getAddrInfo (Just hints) (Just host) (Just port)
  let sockAddr = Net.addrAddress addr
  print sockAddr
  --
  let runServer NetworkAPI{..} = do
        bracket listenOn fst $ \(_,accept) ->
          bracket (print "start server" >> accept) (close . fst) $ \(conn,_) -> do
            Just bs <- recv conn
            send conn ("PONG_" <> bs)
  let runClient NetworkAPI{..}  = do
        threadDelay 10e3
        bracket (print "start client" >> connect sockAddr) close $ \conn -> do
          send conn "PING"
--          send conn "12345678901234"
--          send conn "PING_asdasdasdasdasdasdasdasdasdasdasdasdasdasdasd asdasdasdasdasdasdasdasdasdasdasdasdasdasdasdasdasdasdadasdasdasd\nPING_asdasdasdasdasdasdasdasdasdasdasdasdasdasdasd asdasdasdasdasdasdasdasdasdasdasdasdasdasdasdasdasdasdadasdasdasd"
          bs <- recv conn
          assertEqual "Ping-pong" (Just "PONG_PING") bs
  ((),()) <- concurrently (runServer server) (runClient server)
  return ()


-- | helper function will be used for framing
recvAll :: Net.Socket -> Int -> IO BS.ByteString
recvAll sock n = BS.concat `fmap` loop n
  where
    loop 0    = return []
    loop left = do
      r <- NetBS.recv sock left
      if BS.null r
        then return []
        else fmap (r:) (loop (left - BS.length r))
