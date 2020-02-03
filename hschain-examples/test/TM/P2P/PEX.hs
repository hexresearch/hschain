{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | Tests for peer exchange
--
module TM.P2P.PEX (tests) where

import Control.Monad
import Control.Monad.Fix
import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Cont
import Control.Exception
import Data.IORef
import Data.List
import Data.Set (Set)
import GHC.Conc
import qualified Data.Set as Set

import Test.Tasty
import Test.Tasty.HUnit

import HSChain.Blockchain.Internal.Engine.Types
import HSChain.Control
import HSChain.Mock.Types
import HSChain.Utils
import qualified HSChain.Mock.KeyVal as Mock
import           HSChain.Mock.KeyVal   (BData)
import HSChain.Blockchain.Internal.Engine.Types
import HSChain.Control
import HSChain.Logger
import HSChain.Mock.Types
import HSChain.Mock.KeyList
import HSChain.Monitoring
import HSChain.P2P
import HSChain.P2P.Network
import HSChain.Types.Blockchain
import HSChain.Run
import HSChain.Store

import TM.Util.Network
import TM.Util.Tests

----------------------------------------------------------------
-- Test tree
----------------------------------------------------------------

tests :: TestTree
tests = testGroup "P2P"
  [ testGroup "simple tests"
    [ testCase "require threaded runtime" testMultithread
    -- , testCase "Peers must connect" testPeersMustConnect
    -- , testCase "Peers must ack and get addresses" testPeersMustAckAndGetAddresses
    -- , testCase "Peers in big net must interconnects" $ testBigNetMustInterconnect 20
    ]
  ]


----------------------------------------------------------------
-- Tests implementation
----------------------------------------------------------------

testMultithread :: IO ()
testMultithread =
    assertBool "Test must be run multithreaded" (numCapabilities > 1)
{-
-- Peers in fully connected network must connect to each other.
--
--   * All other peer should be in peer registry
--   * We should try to connect to seed peers
testPeersMustConnect :: IO ()
testPeersMustConnect = do
    [events1, events2, events3] <- replicateM 3 (newIORef Set.empty)
    runConcurrently
        [ createTestNetwork
            [ mkNodeDescription 1 [2, 3] (collectEvents events1)
            , mkNodeDescription 2 [3]    (collectEvents events2)
            , mkNodeDescription 3 []     (collectEvents events3)
            ]
        , waitSec 0.5
        ]
    readIORef events1 >>= ([ TeNodeStarted
                           , TeNodeConnectingTo "2.0.0.0:1122"
                           , TeNodeConnectingTo "3.0.0.0:1122"
                           , TePeerRegistryChanged (Set.fromList ["2.0.0.0:1122","3.0.0.0:1122"])
                           ] @~<?)
    readIORef events2 >>= ([ TeNodeStarted
                           , TeNodeConnectingTo "3.0.0.0:1122"
                           , TePeerRegistryChanged (Set.fromList ["1.0.0.0:1122","3.0.0.0:1122"])
                           ] @~<?)
    readIORef events3 >>= ([ TeNodeStarted
                           , TePeerRegistryChanged (Set.fromList ["1.0.0.0:1122","2.0.0.0:1122"])
                           ] @~<?)


-- In network with initial star topology all peers must connect to
-- each other. Initially only node 1 knows others' nodes addresses
--
--          3
--        /
--  2 -- 1
--        \
--          4
testPeersMustAckAndGetAddresses :: IO ()
testPeersMustAckAndGetAddresses = do
    [events1, events2, events3, events4] <- replicateM 4 (newIORef Set.empty)
    runConcurrently
        [ createTestNetwork
            [ mkNodeDescription 1 [2,3,4] (collectEvents events1)
            , mkNodeDescription 2 []      (collectEvents events2)
            , mkNodeDescription 3 []      (collectEvents events3)
            , mkNodeDescription 4 []      (collectEvents events4)
            ]
        , fix $ \next ->
            andM [ hasRegistryInEvent [2,3,4] events1
                 , hasRegistryInEvent [1,3,4] events2
                 , hasRegistryInEvent [1,2,4] events3
                 , hasRegistryInEvent [1,2,3] events4
                 ] >>= \case
                          True  -> return ()
                          False -> waitSec 0.1 >> next
        , waitSec 10 >> throwIO AbortTest
        ]

-- Nodes in network with initial ring topology should connect to each
-- other
--
--          2 -- 3 -- 4 -- ...
--        /
--       1
--        \
--          0 -- (n-1) -- (n-2) -- ...
testBigNetMustInterconnect :: Int -> IO ()
testBigNetMustInterconnect netSize = do
    let cfg0   = defCfg :: Configuration Example
        ownCfg = cfg0
          { cfgNetwork = (cfgNetwork cfg0)
            { pexMinConnections      = netSize - 1
            , pexMaxConnections      = netSize
            , pexMinKnownConnections = netSize - 1
            , pexMaxKnownConnections = netSize - 1
            }
          }
    events <- replicateM netSize (newIORef Set.empty)
    runConcurrently
        [ createTestNetworkWithConfig ownCfg
            [ mkNodeDescription i [(i - 1) `mod` netSize] (collectEvents e)
            | (i,e) <- [0..] `zip` events
            ]
        , fix $ \next ->
            andM [  hasRegistryInEvent (i `delete` [0..(netSize - 1)]) e
                 | (i,e) <- zip [0..] events
                 ]
                >>= \case
                  True  -> return ()
                  False -> waitSec 0.1 >> next
        , waitSec (fromIntegral netSize) >> throwIO AbortTest
        ]


----------------------------------------------------------------
-- Helpers
----------------------------------------------------------------

mkExpectedRegistry :: [Int] -> Set TraceEvents
mkExpectedRegistry
  = Set.singleton
  . TePeerRegistryChanged
  . Set.fromList
  . map (show . intToNetAddr)

hasRegistryInEvent :: [Int] -> IORef (Set TraceEvents) -> IO Bool
hasRegistryInEvent ids events = Set.isSubsetOf (mkExpectedRegistry ids) <$> readIORef events


andM :: Monad m => [m Bool] -> m Bool
andM [] = return True
andM (p:ps) = p >>= \case
        True  -> andM ps
        False -> return False
-}


type TestMonad m = DBT 'RW BData (NoLogsT m)

data TestNetLinkDescription = TestNetLinkDescription
  { ncFrom          :: Int
  , ncTo            :: [Int]
  }

createTestNetwork
  :: (MonadMask m, MonadFork m, MonadTMMonitoring m)
  => [TestNetLinkDescription]
  -> m ()
createTestNetwork = createTestNetworkWithConfig defCfg

createTestNetworkWithConfig
    :: (MonadMask m, MonadFork m, MonadTMMonitoring m)
    => Configuration Example
    -> [TestNetLinkDescription]
    -> m ()
createTestNetworkWithConfig = createTestNetworkWithValidatorsSetAndConfig testValidators

createTestNetworkWithValidatorsSetAndConfig
    :: (MonadIO m, MonadMask m, MonadFork m, MonadTMMonitoring m)
    => [PrivValidator (Alg BData)]
    -> Configuration Example
    -> [TestNetLinkDescription]
    -> m ()
createTestNetworkWithValidatorsSetAndConfig validators cfg netDescr = do
    net <- liftIO newMockNet
    evalContT $ do
      acts <- forM (netDescr `zip` vallist) $ \(ndescr, val) -> do
        c <- ContT $ withConnection ":memory:"
        lift $ mkTestNode net (c, ndescr, val)
      lift $ catchAbort $ runConcurrently $ concat acts
  where
    vallist        = map Just validators ++ repeat Nothing
    dbValidatorSet = makeValidatorSetFromPriv validators
    genesis        = Mock.mkGenesisBlock dbValidatorSet
    --
    mkTestNode
      :: (MonadFork m, MonadMask m, MonadTMMonitoring m)
      => MockNet
      -> ( Connection 'RW BData
         , TestNetLinkDescription
         , Maybe (PrivValidator (Alg BData))
         )
      -> m [m ()]
    mkTestNode net (conn, TestNetLinkDescription{..}, validatorPK) = do
        initDatabase conn
        let run = runNoLogsT . runDBT conn
        (_,actions) <- run $ Mock.interpretSpec genesis
          (   BlockchainNet
                { bchNetwork        = createMockNode net (intToNetAddr ncFrom)
                , bchInitialPeers   = map intToNetAddr ncTo
                }
          :*: NodeSpec
                { nspecPrivKey = validatorPK
                , nspecDbName  = Nothing
                , nspecLogFile = []
                }
          :*: cfg
          )
          mempty
        return $ run <$> actions
