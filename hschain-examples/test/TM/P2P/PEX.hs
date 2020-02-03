{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DerivingStrategies         #-}
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

import Control.Arrow (first)
import Control.Monad
import Control.Monad.Fix
import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Cont
import Control.Monad.Trans.Reader
import Control.Exception
import Data.Aeson (Value(..),Object)
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

import HSChain.Blockchain.Internal.Engine.Types
import HSChain.Mock.Types
import HSChain.Utils
import qualified HSChain.Mock.KeyVal as Mock
import           HSChain.Mock.KeyVal   (BData)
import HSChain.Control
import HSChain.Logger
import HSChain.Mock.KeyList
import HSChain.Monitoring
import HSChain.P2P.Network
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
    , testCase "Peers must connect" testPeersMustConnect
    , testCase "Peers must ack and get addresses" testPeersMustAckAndGetAddresses
    , testCase "Peers in big net must interconnects" $ testBigNetMustInterconnect 20
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
    [ [ ("Connecting to",  [("addr",  "3.0.0.0:1122")])
      , ("Connecting to",  [("addr",  "2.0.0.0:1122")])
      ]
    , [ ("peer registry update",  [("conns", Array [ "2.0.0.0:1122", "3.0.0.0:1122"])])
      ]
    ]
  readPEX events2 >>= isSubset "Node 2"
    [ [ ("Connecting to"   ,  [("addr", "3.0.0.0:1122")])
      , ("Preacceped peer" ,  [("addr", "1.0.0.0:1122")])
      , ("Accepted peer"   ,  [("addr", "1.0.0.0:1122"),("norm", "1.0.0.0:1122")])
      ]
    , [ ("peer registry update", [("conns",Array [ "1.0.0.0:1122", "3.0.0.0:1122"])])
      ]
    ]
  readPEX events3 >>= isSubset "Node 3"
    [ [ ("Preacceped peer" ,  [("addr", "2.0.0.0:1122")])
      , ("Accepted peer"   ,  [("addr", "2.0.0.0:1122"),("norm", "2.0.0.0:1122")])
      , ("Preacceped peer" ,  [("addr", "1.0.0.0:1122")])
      , ("Accepted peer"   ,  [("addr", "1.0.0.0:1122"),("norm", "1.0.0.0:1122")])
      ]
    , [ ("peer registry update",  [("conns",Array ["1.0.0.0:1122", "2.0.0.0:1122"])])
      ]
    ]

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
  let cfg0   = defCfg :: Configuration FastTest
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
andM [] = return True
andM (p:ps) = p >>= \case
        True  -> andM ps
        False -> return False

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
createTestNetwork = createTestNetworkWithConfig defCfg

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
      -> ( Connection 'RW BData
         , TestNode
         , Maybe (PrivValidator (Alg BData))
         )
      -> m [m ()]
    mkTestNode net (conn, TestNode{..}, validatorPK) = do
        initDatabase conn
        let run = runIORefLogT ncScribe . runDBT conn
        (_,actions) <- run $ Mock.interpretSpec genesis
          (   BlockchainNet
                { bchNetwork      = createMockNode net (intToNetAddr ncFrom)
                , bchInitialPeers = intToNetAddr <$> ncTo
                }
          :*: NodeSpec
                { nspecPrivKey = validatorPK
                , nspecDbName  = Nothing
                , nspecLogFile = []
                }
              -- 
          :*: (let Configuration{..} = cfg in Configuration{..})
          )
          mempty
        return $ run <$> actions


newtype IORefLogT m a = IORefLogT { unIORefLogT :: ReaderT (Namespace, IORef [(Namespace,Text,Object)]) m a }
  deriving newtype ( Functor, Applicative, Monad
                   , MonadIO, MonadThrow, MonadCatch, MonadMask
                   , MonadFork, MonadTMMonitoring
                   )

runIORefLogT :: IORef [(Namespace,Text,Object)] -> IORefLogT m a -> m a
runIORefLogT ref = flip runReaderT (mempty,ref) . unIORefLogT

instance MonadTrans IORefLogT where
  lift = IORefLogT . lift

instance MonadIO m => MonadLogger (IORefLogT m) where
  logger _ msg a = do
    (namespace, ref) <- IORefLogT ask
    liftIO $ modifyIORef' ref (( namespace
                               , toLazyText $ unLogStr msg
                               , toObject a) :)
    -- liftIO $ putStr $ T.unpack $ case chunks of
    --   [] -> ""
    --   _  -> T.intercalate "." chunks
    -- liftIO $ putStrLn $ TL.unpack $ toLazyText $ unLogStr msg
    -- liftIO $ forM_ (HM.toList $ toObject a) $ \(k,v) -> do
    --   putStr $ "  " ++ T.unpack k ++ " = "
    --   print v
  localNamespace f = IORefLogT . local (first f) . unIORefLogT
