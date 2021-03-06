{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedLists            #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE ViewPatterns               #-}
-- | Tests for peer exchange
--
module TM.P2P.PEX (tests) where

import Control.Monad
import Control.Monad.Fix
import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Cont
import Control.Monad.Reader
import Control.Exception
import Data.Aeson (Value(..),Object)
import Data.Bool
import Data.Coerce
import Data.Default.Class
import Data.IORef
import Data.List
import Data.Maybe
import Data.Text.Lazy (Text)
import Data.Text.Lazy.Builder (toLazyText)
import Data.Hashable

import Katip
import GHC.Conc
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet        as HS
import qualified Data.Vector         as V
import qualified Data.Text           as T

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Runners

import HSChain.Blockchain.Internal.Engine.Types
import HSChain.Mock.Types
import HSChain.Control.Delay
import qualified HSChain.Mock.KeyVal as Mock
import           HSChain.Mock.KeyVal   (BData)
import HSChain.Control.Class
import HSChain.Logger
import HSChain.Mock.KeyList
import HSChain.Monitoring
import HSChain.Network.Mock
import HSChain.Types.Blockchain
import HSChain.Run
import HSChain.Store

import TM.Util.Network


----------------------------------------------------------------
-- Test tree
----------------------------------------------------------------

tests :: TestTree
tests = testGroup "P2P"
  [ testGroup "simple tests"
    [ testCase "require threaded runtime" testMultithread
    , localOption (1 :: NumThreads)
    $ testCase "Peers must connect" testPeersMustConnect
    , localOption (1 :: NumThreads)
    $ testCase "Peers must ack and get addresses" testPeersMustAckAndGetAddresses
    , localOption (1 :: NumThreads)
    $ testCase "Peers in ring net must interconnects" $ testBigNetMustInterconnect 4
    ]
  ]


----------------------------------------------------------------
-- Tests implementation
----------------------------------------------------------------

testMultithread :: IO ()
testMultithread =
    assertBool "Test must be run multithreaded" (numCapabilities > 1)

-- Peers in fully connected network must connect to each other.
--
--   * All other peer should be in peer registry
--   * We should try to connect to seed peers
testPeersMustConnect :: IO ()
testPeersMustConnect = do
  [events1, events2, events3] <- replicateM 3 $ newIORef []
  runConcurrently
    [ createTestNetwork
      [ TestNode 1 [2, 3] events1
      , TestNode 2 [3]    events2
      , TestNode 3 []     events3
      ]
    , waitSec 0.5
    ]
  readPEX events1 >>= isSubset "Node 1"
    [[ ("peer registry update",  [("conns", Array [ "2.0.0.0:0", "3.0.0.0:0"])]) ]]
  readPEX events2 >>= isSubset "Node 2"
    [[ ("peer registry update", [("conns",Array [ "1.0.0.0:0", "3.0.0.0:0"])]) ]]
  readPEX events3 >>= isSubset "Node 3"
    [[ ("peer registry update",  [("conns",Array ["1.0.0.0:0", "2.0.0.0:0"])]) ]]


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
  [events1, events2, events3, events4] <- replicateM 4 $ newIORef []
  runConcurrently
    [ createTestNetwork
      [ TestNode 1 [2,3,4] events1
      , TestNode 2 []      events2
      , TestNode 3 []      events3
      , TestNode 4 []      events4
      ]
    , fix $ \next ->
        andM [ hasSubset (mkExpected [2,3,4]) <$> readPEX events1
             , hasSubset (mkExpected [1,3,4]) <$> readPEX events2
             , hasSubset (mkExpected [1,2,4]) <$> readPEX events3
             , hasSubset (mkExpected [1,2,3]) <$> readPEX events4
             ] >>= flip unless (waitSec 0.1 >> next)
    , waitSec 2 >> throwIO AbortTest
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
  let cfg0   = def :: Configuration FastTest
      ownCfg = cfg0
        { cfgNetwork = (cfgNetwork cfg0)
          { pexMinConnections      = netSize - 1
          , pexMaxConnections      = netSize
          , pexMinKnownConnections = netSize - 1
          , pexMaxKnownConnections = netSize - 1
          }
        }
  events <- replicateM netSize $ newIORef []
  runConcurrently
    [ createTestNetworkWithConfig ownCfg
      [ TestNode i [(i - 1) `mod` netSize] e
      | (i,e) <- [0..] `zip` events
      ]
    , fix $ \next ->
        andM [ hasSubset (mkExpected (i `delete` [0 .. netSize - 1])) <$> readPEX e
             | (i,e) <- zip [0..] events
             ] >>= flip unless (waitSec 0.1 >> next)
    , waitSec (fromIntegral netSize) >> throwIO AbortTest
    ]



----------------------------------------------------------------
-- Helpers
----------------------------------------------------------------

andM :: Monad m => [m Bool] -> m Bool
andM = foldr (\p m -> bool (return False) m =<< p) (return True)

-- Create list of expected events for the 
mkExpected :: [Int] -> [[(Text, Object)]]
mkExpected idxs
  = [[ ( "peer registry update"
       , HM.fromList [ ("conns"
                       , Array $ V.fromList $ String . T.pack . show . intToNetAddr <$> idxs
                       )
                     ]
       )
     ]]

-- Read list of networking-related log entries
readPEX :: IORef [(Namespace, Text, Object)] -> IO [(Text, Object)]
readPEX ref
  = mapMaybe (\(ns,msg,o) -> do
                 Namespace ("net":_) <- return ns
                 guard $ msg /= "Killed normally by ThreadKilled"
                 return (msg, o)
             )
  . reverse
 <$> readIORef ref


isSubset
  :: (Hashable a, Eq a, Show a, HasCallStack)
  => String
  -> [[a]]                      -- ^ Expected messages in log
  -> [a]                        -- ^ Actual logs
  -> Assertion
isSubset prefix es xs
  = maybe (return ()) error
  $ checkSubset prefix es xs

hasSubset
  :: (Hashable a, Eq a, Show a, HasCallStack)
  => [[a]]                      -- ^ Expected messages in log
  -> [a]                        -- ^ Actual logs
  -> Bool
hasSubset es xs
  = Nothing == checkSubset "" es xs

checkSubset
  :: (Hashable a, Eq a, Show a, HasCallStack)
  => String
  -> [[a]]                      -- ^ Expected messages in log
  -> [a]                        -- ^ Actual logs
  -> Maybe String
checkSubset prefix es0 xs0 = loop es0 xs0
  where
    failure = Just $ unlines $ "" : prefix : "Unexpected log sequence:" : map show xs0
    loop []     _  = Nothing
    loop (e:es) xs = go es (HS.fromList e) xs
    --
    go es EmptySet xs     = loop es xs
    go _  _        []     = failure
    go es expected (x:xs)
      | x `HS.member` expected = go es (HS.delete x expected) xs
      | otherwise              = go es expected                xs

pattern EmptySet :: HS.HashSet a
pattern EmptySet <- (HS.null -> True)

data TestNode = TestNode
  { ncFrom   :: Int
  , ncTo     :: [Int]
  , ncScribe :: IORef [(Namespace, Text, Object)]
  }

createTestNetwork
  :: (MonadMask m, MonadFork m, MonadTMMonitoring m)
  => [TestNode]
  -> m ()
createTestNetwork = createTestNetworkWithConfig def

createTestNetworkWithConfig
    :: (MonadMask m, MonadFork m, MonadTMMonitoring m)
    => Configuration FastTest
    -> [TestNode]
    -> m ()
createTestNetworkWithConfig = createTestNetworkWithValidatorsSetAndConfig testValidators

createTestNetworkWithValidatorsSetAndConfig
    :: (MonadIO m, MonadMask m, MonadFork m, MonadTMMonitoring m)
    => [PrivValidator (Alg BData)]
    -> Configuration FastTest
    -> [TestNode]
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
      -> ( Connection 'RW
         , TestNode
         , Maybe (PrivValidator (Alg BData))
         )
      -> m [m ()]
    mkTestNode net (conn, TestNode{..}, validatorPK) = do
        let run = runPEXT conn ncScribe
        (_,actions) <- run $ Mock.interpretSpec genesis
          NodeSpec
            { nspecPrivKey     = validatorPK
            , nspecDbName      = Nothing
            , nspecLogFile     = []
            }
          BlockchainNet
            { bchNetwork      = createMockNode net (intToNetAddr ncFrom)
            , bchInitialPeers = intToNetAddr <$> ncTo
            }
          (coerce cfg)
          mempty
        return $ run <$> actions


newtype PEXT a m x = PEXT
  (ReaderT (Namespace, IORef [(Namespace,Text,Object)], Connection 'RW, Cached a) m x)
  deriving newtype ( Functor, Applicative, Monad
                   , MonadIO, MonadThrow, MonadCatch, MonadMask
                   , MonadFork, MonadTMMonitoring
                   , MonadReader (Namespace, IORef [(Namespace,Text,Object)], Connection 'RW, Cached a)
                   )
  deriving (MonadReadDB, MonadDB) via DatabaseByType (PEXT a m)
  deriving (MonadCached a)        via CachedByType a (PEXT a m)

runPEXT :: MonadIO m => Connection 'RW -> IORef [(Namespace,Text,Object)] -> PEXT a m x -> m x
runPEXT conn ref (PEXT m) = do
  c <- newCached
  runReaderT m (mempty,ref,conn,c)

instance MonadTrans (PEXT a) where
  lift = PEXT . lift

instance MonadIO m => MonadLogger (PEXT a m) where
  logger _ msg a = do
    (namespace, ref, _, _) <- PEXT ask
    liftIO $ atomicModifyIORef' ref $ \xs ->
      ( ( namespace
        , toLazyText $ unLogStr msg
        , toObject a) : xs
      , ()
      )
  localNamespace f (PEXT m) = PEXT $ local (\(n,a,b,c) -> (f n,a,b,c)) m

