-- | Tests for peer exchange
--
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module TM.P2P (tests) where


import Data.IORef
import           Data.Set          (Set)
import qualified Data.Set as Set
import Control.Monad
import GHC.Conc

import Test.Tasty
import Test.Tasty.HUnit

import Thundermint.Control
import Thundermint.Debug.Trace

import TM.Util.Network
import TM.Util.Misc
import TM.Util.Tests


collectEvents :: IORef (Set TraceEvents) -> TraceEvents -> IO ()
collectEvents events event = atomicModifyIORef events (\s -> (Set.insert event s, ()))


testPeersMustConnect :: IO ()
testPeersMustConnect = do
    [events1, events2, events3] <- replicateM 3 (newIORef Set.empty)
    runConcurrently
        [ createTestNetwork
            [ TestNetLinkDescription 1 [2, 3] (collectEvents events1)
            , TestNetLinkDescription 2 [1, 3] (collectEvents events2)
            , TestNetLinkDescription 3 [1, 2] (collectEvents events3)
            ]
        , waitSec 0.5
        ]
    readIORef events1 >>= ([ TeNodeStarted
                           , TeNodeConnectingTo "(TestAddr 2,\"tst\")"
                           , TeNodeConnectingTo "(TestAddr 3,\"tst\")"
                           ] @~<?)
    readIORef events2 >>= ([ TeNodeStarted
                           , TePeerRegistryChanged (Set.fromList ["(TestAddr 1,\"tst\")","(TestAddr 3,\"tst\")"])
                           ] @~<?)
    readIORef events3 >>= ([ TeNodeStarted
                           , TePeerRegistryChanged (Set.fromList ["(TestAddr 1,\"tst\")","(TestAddr 2,\"tst\")"])
                           ] @~<?)


testMultithread :: IO ()
testMultithread =
    assertBool "Test must be run multithreaded" (numCapabilities > 1)


testPeerRegistryMustBeFilled :: IO ()
testPeerRegistryMustBeFilled = do
    [events1, events2, events3] <- replicateM 3 (newIORef Set.empty)
    runConcurrently
        [ createTestNetwork
            [ TestNetLinkDescription 1 [2, 3] (collectEvents events1)
            , TestNetLinkDescription 2 [1, 3] (collectEvents events2)
            , TestNetLinkDescription 3 [1, 2] (collectEvents events3)
            ]
        , waitSec 0.5
        ]
    readIORef events1 >>= ([TePeerRegistryChanged (Set.fromList ["(TestAddr 2,\"tst\")","(TestAddr 3,\"tst\")"])] @~<?)
    readIORef events2 >>= ([TePeerRegistryChanged (Set.fromList ["(TestAddr 1,\"tst\")","(TestAddr 3,\"tst\")"])] @~<?)
    readIORef events3 >>= ([TePeerRegistryChanged (Set.fromList ["(TestAddr 1,\"tst\")","(TestAddr 2,\"tst\")"])] @~<?)


testPeersMustAckAndGetAddresses :: IO ()
testPeersMustAckAndGetAddresses = do
    [events1, events2, events3, events4] <- replicateM 4 (newIORef Set.empty)
    runConcurrently
        [ createTestNetwork
            --
            -- Test network initial topology:
            --
            --          3
            --        / |
            --  1 -- 2  |
            --        \ |
            --          4
            --
            [ TestNetLinkDescription 2 [1, 3, 4] (collectEvents events2)
            , TestNetLinkDescription 3 [2, 4]    (collectEvents events3)
            , TestNetLinkDescription 4 [2, 3]    (collectEvents events4)
            --
            , TestNetLinkDescription 1 [2]       (collectEvents events1)
            ]
        , waitSec 2
        ]
    readIORef events1 >>= ([TePeerRegistryChanged (Set.fromList ["(TestAddr 2,\"tst\")","(TestAddr 3,\"tst\")","(TestAddr 4,\"tst\")"])] @~<?)
    readIORef events3 >>= ([TePeerRegistryChanged (Set.fromList ["(TestAddr 1,\"tst\")","(TestAddr 2,\"tst\")","(TestAddr 4,\"tst\")"])] @~<?)
    readIORef events4 >>= ([TePeerRegistryChanged (Set.fromList ["(TestAddr 1,\"tst\")","(TestAddr 2,\"tst\")","(TestAddr 3,\"tst\")"])] @~<?)
    return ()



tests :: TestTree
tests = testGroup "p2p"
    [ testGroup "simple tests" [
          testCase "Tests must be multithreaded" testMultithread
        , testCase "Peers must connect" testPeersMustConnect
        , testCase "Peer registry must be filled" testPeerRegistryMustBeFilled
        , testCase "Peers must ack and get addresses" testPeersMustAckAndGetAddresses
        ]
    ]
