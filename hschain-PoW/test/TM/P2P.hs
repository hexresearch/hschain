{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
-- |
module TM.P2P (tests, test1) where

import Codec.Serialise (serialise,deserialise)
import Control.Concurrent
import Control.Monad.IO.Class
import Control.Monad.State.Strict
import Control.Monad.Except
import Data.IORef
import Data.List (unfoldr)
import Data.Foldable
import qualified Data.Map.Strict as Map
import qualified Data.Set        as Set
import Lens.Micro
import Test.Tasty
import Test.Tasty.HUnit

import HSChain.Control.Class
import HSChain.Network.Types
import HSChain.Network.Mock
import HSChain.PoW.Types
import HSChain.PoW.Consensus
import HSChain.PoW.P2P
import HSChain.PoW.P2P.Types
import HSChain.Types.Merkle.Types
import HSChain.Examples.Simple

import TM.Util.InMemory
import TM.Util.Mockchain

tests :: TestTree
tests = testGroup "P2P"
  [
  ]

test1 :: IO ()
test1 = do
  db  <- inMemoryDB @_ @KV
  net <- newMockNet
  let s0 = consensusGenesis (head mockchain) (viewKV (blockID genesis))
  let apiNode        = createMockNode net ipNode
      NetworkAPI{..} = createMockNode net ipOur
  forkLinked (startNode (NetCfg 0 0) apiNode db s0) $ do
    -- Establish connection
    threadDelay 100000
    P2PConnection{..} <- connect ipNode
    send $ serialise $ HandshakeHello (HandshakeNonce 0) port
    HandshakeAck <- deserialise <$> recv
    -- Send announce
    send $ serialise $ GossipAnn $ AnnBestHead header1
    msg <- deserialise <$> recv
    print (msg :: GossipMsg KV)
    -- WAIT
    -- HandshakeHello 1 port
    -- Handshake
    return ()
  where
    ipNode = NetAddrV4 1 port
    ipOur  = NetAddrV4 2 port
    port   = 1000

