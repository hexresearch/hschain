-- | Tests for peer exchange
--
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module TM.P2P (tests) where


import Control.Monad
import Control.Monad.Fix
import Data.IORef
import Data.List
import Data.Set (Set)
import GHC.Conc
import qualified Data.Set as Set

import Test.Tasty
import Test.Tasty.HUnit

import Thundermint.Blockchain.Internal.Engine.Types
import Thundermint.Control
import Thundermint.Debug.Trace
import Thundermint.Mock.Types
import Thundermint.Utils
import Thundermint.Run

import TM.Util.Network
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


mkExpectedRegistry :: [Int] -> Set TraceEvents
mkExpectedRegistry ids =
    Set.fromList [TePeerRegistryChanged $ Set.fromList $ map (\nid -> show (TestAddr nid, testNetworkName)) ids]

hasRegistryInEvent :: [Int] -> IORef (Set TraceEvents) -> IO Bool
hasRegistryInEvent ids events = Set.isSubsetOf (mkExpectedRegistry ids) <$> readIORef events


andM :: Monad m => [m Bool] -> m Bool
andM [] = return True
andM (p:ps) = p >>= \case
        True -> andM ps
        False -> return False


testPeersMustAckAndGetAddresses :: IO ()
testPeersMustAckAndGetAddresses = do
    [events1, events2, events3, events4] <- replicateM 4 (newIORef Set.empty)
    ok <- newIORef False
    runConcurrently
        [ createTestNetwork
            --
            -- Test network with initial topology:
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
        , fix $ \next ->
            andM [  hasRegistryInEvent [2,3,4] events1
                  , hasRegistryInEvent [1,2,4] events3
                  , hasRegistryInEvent [1,2,3] events4
                  ] >>= \case
                  True  -> writeIORef ok True
                  False -> waitSec 0.1 >> next
        , waitSec 5
        ]
    --readIORef events1 >>= (mkExpectedRegistry [2,3,4] @<?)
    --readIORef events3 >>= (mkExpectedRegistry [1,2,4] @<?)
    --readIORef events4 >>= (mkExpectedRegistry [1,2,3] @<?)
    readIORef ok >>= assertBool "All events must occur"


testBigNetMustInterconnect :: IO ()
testBigNetMustInterconnect = do
    let netSize = 20
        cfg0    = defCfg :: Configuration Example
        ownCfg  = cfg0
          { cfgNetwork = (cfgNetwork cfg0)
            { pexMinConnections      = netSize - 1
            , pexMaxConnections      = netSize
            , pexMinKnownConnections = netSize - 1
            , pexMaxKnownConnections = netSize - 1
            }
          }
    events <- replicateM netSize (newIORef Set.empty)
    ok <- newIORef False
    runConcurrently
        [ createTestNetworkWithConfig ownCfg
            --
            -- Test network with initial ring topology:
            --
            --          2 -- 3 -- 4 -- ...
            --        /
            --       1
            --        \
            --          0 -- (n-1) -- (n-2) -- ...
            --
            [ TestNetLinkDescription i [(i - 1) `mod` netSize] (collectEvents e)
            | (i,e) <- zip [0..] events]
        , fix $ \next ->
            andM [  hasRegistryInEvent (i `delete` [0..(netSize - 1)]) e
                 | (i,e) <- zip [0..] events
                 ]
                >>= \case
                  True  -> writeIORef ok True
                  False -> waitSec 0.1 >> next
        , waitSec (fromIntegral netSize)
        ]
    readIORef ok >>= assertBool "All events must occur"


tests :: TestTree
tests = testGroup "p2p"
    [ testGroup "simple tests" [
          testCase "Tests must be multithreaded" testMultithread
        , testCase "Peers must connect" testPeersMustConnect
        , testCase "Peer registry must be filled" testPeerRegistryMustBeFilled
        , testCase "Peers must ack and get addresses" testPeersMustAckAndGetAddresses
        , testCase "Peers in big net must interconnects" testBigNetMustInterconnect
        ]
    ]
