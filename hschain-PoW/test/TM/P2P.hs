{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiWayIf                 #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}
-- |
module TM.P2P (tests) where

import Codec.Serialise (serialise,deserialise)
import Control.Concurrent
import Control.Monad.Trans.Cont
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Class
import Control.Monad.IO.Class
import Test.Tasty
import Test.Tasty.HUnit

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
selfTest = runNetTest $ return ()

-- Test sync when we don't have to do catchup
testNormalSync :: IO ()
testNormalSync = runNetTest $ do
  -- Announce header at H=1
  sendM $ GossipAnn $ AnnBestHead header1
  -- Peer connected header and asks for block
  expectBlockReq "B1" block1
  expectAnnounce "A1" header1
  -- Announce header at H=2
  sendM $ GossipAnn $ AnnBestHead header2
  -- Peer connected header and asks for block
  expectBlockReq "B2" block2
  expectAnnounce "A2" header2


-- Test sync when we have to do catchup
testCatchup :: IO ()
testCatchup = runNetTest $ do
  -- Announce header at H=2
  sendM $ GossipAnn $ AnnBestHead header2
  GossipReq (ReqHeaders loc) <- recvM
  liftIO $ assertEqual "Locator" loc (Locator [blockID genesis])
  sendM $ GossipResp $ RespHeaders [header1,header2]
  -- Now peer may request either block1 or block2. In test we have to
  -- handle both
  bidA <- blockReq "K"
  if | bidA == blockID block1 -> do
         error "Won't happen at the moment (block choice is determinitic"
     | bidA == blockID block2 -> do
         sendM $ GossipResp $ RespBlock block2
         expectBlockReq "B2.2" block1
         expectAnnounce "A2.1" header2
     | otherwise -> liftIO $ assertFailure "Bad block requested"


----------------------------------------------------------------
-- Helpers
----------------------------------------------------------------

expectAnnounce :: String -> Header KV -> TestM ()
expectAnnounce key h0 = do
  GossipAnn (AnnBestHead h) <- recvM
  liftIO $ assertEqual ("expectAnnounce: "++key) h h0

blockReq :: String -> TestM (BlockID KV)
blockReq key = recvM >>= \case
  GossipReq (ReqBlock bid) -> return bid
  m                        -> liftIO $ assertFailure (key ++ " : blockReq, got " ++ show m)

expectBlockReq :: String -> Block KV -> TestM ()
expectBlockReq key block = do
  GossipReq (ReqBlock bid) <- recvM
  liftIO $ assertEqual ("expectBlockReq: "++key) bid (blockID block)
  sendM $ GossipResp $ RespBlock block



----------------------------------------------------------------
-- Runnner for tests
----------------------------------------------------------------

newtype TestM a = TestM
  { unTestM :: ReaderT P2PConnection IO a
  }
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadFail)

runTestM :: P2PConnection -> TestM a -> IO a
runTestM c m = runReaderT (unTestM m) c

sendM :: GossipMsg KV -> TestM ()
sendM m = TestM $ do
  P2PConnection{..} <- ask
  send $ serialise m

recvM :: TestM (GossipMsg KV)
recvM = TestM $ do
  P2PConnection{..} <- ask
  deserialise <$> recv

-- Run network test
--
-- We use PEX setting which preclude it from sending any messages
runNetTest :: TestM () -> IO ()
runNetTest test = do
  db  <- inMemoryDB @_ @_ @KV
  net <- newMockNet
  let s0 = consensusGenesis (head mockchain) (viewKV (blockID genesis))
  let apiNode        = createMockNode net ipNode
      NetworkAPI{..} = createMockNode net ipOur
  runNoLogsT $ evalContT $ do
    _ <- startNode (NetCfg 0 0) apiNode [] db s0
    lift $ lift $ do -- Establish connection
      --
      -- FIXME: we need to do something better than fixed delay
      threadDelay 100000
      conn@P2PConnection{..} <- connect ipNode
      send $ serialise $ HandshakeHello (HandshakeNonce 0) port
      HandshakeAck <- deserialise <$> recv
      -- Run test
      runTestM conn test
  where
    ipNode = NetAddrV4 1 port
    ipOur  = NetAddrV4 2 port
    port   = 1000
