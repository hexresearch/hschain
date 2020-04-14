{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE MultiWayIf          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
-- |
module TM.P2P (tests) where

import Codec.Serialise (serialise,deserialise)
import Control.Concurrent
import Test.Tasty
import Test.Tasty.HUnit

import HSChain.Control.Class
import HSChain.Network.Types
import HSChain.Network.Mock
import HSChain.PoW.Types
import HSChain.PoW.Logger
import HSChain.PoW.Consensus
import HSChain.PoW.P2P
import HSChain.PoW.P2P.Types
import HSChain.Examples.Simple

import TM.Util.InMemory
import TM.Util.Mockchain

tests :: TestTree
tests = testGroup "P2P"
  [ testCase "self-test"   selfTest
  , testCase "normal sync" testNormalSync
  , testCase "catchup"     testCatchup
  ]

-- Test that test harness works
selfTest :: IO ()
selfTest = runNetTest $ \_ _ -> return ()

-- Test sync when we don't have to do catchup
testNormalSync :: IO ()
testNormalSync = runNetTest $ \sendMsg recvMsg -> do
  -- Announce header at H=1
  sendMsg $ GossipAnn $ AnnBestHead header1
  -- Peer connected header and asks for block
  GossipReq (ReqBlock bid1) <- recvMsg
  assertEqual "Block request H=1" bid1 (blockID header1)
  sendMsg $ GossipResp $ RespBlock block1
  GossipAnn (AnnBestHead h1) <- recvMsg
  assertEqual "Announce " h1 header1
  -- Announce header at H=2
  sendMsg $ GossipAnn $ AnnBestHead header2
  -- Peer connected header and asks for block
  GossipReq (ReqBlock bid2) <- recvMsg
  assertEqual "Block request H=2" bid2 (blockID header2)
  sendMsg $ GossipResp $ RespBlock block2
  GossipAnn (AnnBestHead h2) <- recvMsg
  assertEqual "Announce 2" h2 header2

-- Test sync when we have to do catchup
testCatchup :: IO ()
testCatchup = runNetTest $ \sendMsg recvMsg -> do
  -- Announce header at H=2
  sendMsg $ GossipAnn $ AnnBestHead header2
  GossipReq (ReqHeaders loc) <- recvMsg
  assertEqual "Locator" loc (Locator [blockID genesis])
  sendMsg $ GossipResp $ RespHeaders [header1,header2]
  -- Now peer may request either block1 or block2. In test we have to
  -- handle both
  GossipReq (ReqBlock bidA) <- recvMsg
  if | bidA == blockID block1 -> do
         error "Won't happen at the moment (block choice is determinitic"
     | bidA == blockID block2 -> do
         sendMsg $ GossipResp $ RespBlock block2
         GossipReq (ReqBlock bidB) <- recvMsg
         assertEqual "Second BID" bidB (blockID block1)
         sendMsg $ GossipResp $ RespBlock block1
         GossipAnn (AnnBestHead h2) <- recvMsg
         assertEqual "Announce 2" h2 header2
     | otherwise -> assertFailure "Bad block requested"



----------------------------------------------------------------
-- Runnner for tests
----------------------------------------------------------------

-- Run network test
--
-- We use PEX setting which preclude it from sending any messages
runNetTest
  :: (  (GossipMsg KV -> IO ())
     -> (IO (GossipMsg KV))
     -> IO ())
  -> IO ()
runNetTest test = do
  db  <- inMemoryDB @_ @_ @KV
  net <- newMockNet
  let s0 = consensusGenesis (head mockchain) (viewKV (blockID genesis))
  let apiNode        = createMockNode net ipNode
      NetworkAPI{..} = createMockNode net ipOur
  forkLinked (runNoLogsT $ startNode (NetCfg 0 0) apiNode db s0) $ do
    -- Establish connection
    --
    -- FIXME: we need to do something better than fixed delay 
    threadDelay 100000
    P2PConnection{..} <- connect ipNode
    send $ serialise $ HandshakeHello (HandshakeNonce 0) port
    HandshakeAck <- deserialise <$> recv
    -- Run test
    test (send . serialise) (deserialise <$> recv)
  where
    ipNode = NetAddrV4 1 port
    ipOur  = NetAddrV4 2 port
    port   = 1000
