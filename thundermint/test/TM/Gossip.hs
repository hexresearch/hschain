-- | Tests for gossip
--
{-. LANGUAGE DataKinds             #-}
{-. LANGUAGE LambdaCase            #-}
{-. LANGUAGE OverloadedStrings     #-}
{-. LANGUAGE RankNTypes            #-}
{-. LANGUAGE ScopedTypeVariables   #-}

module TM.Gossip (tests) where


import Control.Monad
--import Control.Monad.Fix
import Data.IORef
--import Data.List
--import Data.Set (Set)
--import GHC.Conc
import qualified Data.Set as Set

import Test.Tasty
import Test.Tasty.HUnit

--import Thundermint.Blockchain.Internal.Engine.Types
import Thundermint.Control
import Thundermint.Debug.Trace
--import Thundermint.Mock.Coin
--import Thundermint.Mock.Types
import Thundermint.Utils

-- import Thundermint.P2P

import TM.Util.Network
import TM.Util.Tests



testGossip :: IO ()
testGossip = do
    [events1, events2, events3, events4] <- replicateM 4 (newIORef Set.empty)
    runConcurrently
        [ createTestNetwork
            [ TestNetLinkDescription 1 [2, 3, 4] (collectEvents events1)
            , TestNetLinkDescription 2 [1, 3, 4] (collectEvents events2)
            , TestNetLinkDescription 3 [1, 2, 4] (collectEvents events3)
            , TestNetLinkDescription 4 [1, 2, 3] (collectEvents events4)
            ]
        -- TODO дождаться каждого события; чтобы ускорить проверку
        , waitSec 0.5
        ]
    readIORef events1 >>= ([TePeerGossipVotesStarted] @~<?)
    readIORef events2 >>= ([TePeerGossipVotesStarted] @~<?)
    readIORef events3 >>= ([TePeerGossipVotesStarted] @~<?)
    readIORef events4 >>= ([TePeerGossipVotesStarted] @~<?)


tests :: TestTree
tests = testGroup "eigen-gossip"
    [ testGroup "simple tests" [
            testCase "Test gossip" testGossip
        ]
    ]
