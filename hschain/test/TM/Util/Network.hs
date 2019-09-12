{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NumDecimals         #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeOperators       #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Contains helpers for network UTs
module TM.Util.Network
  ( NetPair
  , TestAlg
  -- * Timeouts
  , withRetry
  , withTimeOut
  -- *
  , mkNodeDescription
  , createGossipTestNetwork
  , createTestNetworkWithConfig
  , createTestNetwork
  , testValidators
  , intToNetAddr
  ) where

import qualified Control.Exception        as E

import Control.Monad
import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Cont
import Control.Retry  (constantDelay, limitRetries, recovering)

import Data.Monoid    ((<>))
import System.Timeout (timeout)

import qualified HSChain.Mock.KeyVal as Mock
import HSChain.Blockchain.Internal.Engine.Types
import HSChain.Control
import HSChain.Crypto                           ((:&))
import HSChain.Crypto.Ed25519                   (Ed25519)
import HSChain.Crypto.SHA                       (SHA512)
import HSChain.Debug.Trace
import HSChain.Logger
import HSChain.Mock.Types
import HSChain.Mock.KeyList
import HSChain.Monitoring
import HSChain.P2P
import HSChain.P2P.Network
import HSChain.Run
import HSChain.Store

import TM.RealNetwork


----------------------------------------------------------------
-- Timeouts
----------------------------------------------------------------

withRetry :: MonadIO m => IO a -> m a
withRetry
  = liftIO
  . recovering (constantDelay 500 <> limitRetries 20) hs
  . const
  where
    -- exceptions list to trigger the recovery logic
    hs :: [a -> Handler IO Bool]
    hs = [const $ Handler (\(_::E.IOException) -> return True)]

-- | Exception for aborting the execution of test
data AbortTest = AbortTest
                 deriving Show

instance Exception AbortTest

withTimeOut :: Int -> IO a -> IO a
withTimeOut t act = timeout t act >>= \case
  Just n  -> pure n
  Nothing -> E.throwIO AbortTest


----------------------------------------------------------------
-- Test networks
----------------------------------------------------------------

-- TODO объединить в один список, а лучше сделать бесконечный
testValidators :: [PrivValidator TestAlg]
testValidators = take 4 $ map PrivValidator $ makePrivKeyStream 1337

type TestAlg = Ed25519 :& SHA512

type TestMonad m = DBT 'RW TestAlg Mock.BData (NoLogsT (TracerT m))

type TestAppByzantine m = AppByzantine (TestMonad m) TestAlg Mock.BData


data TestNetLinkDescription m = TestNetLinkDescription
    { ncFrom          :: Int
    , ncTo            :: [Int]
    , ncByzantine     :: TestAppByzantine m
    , ncTraceCallback :: TraceEvents -> m ()
    , ncAppCallbacks  :: AppCallbacks (TestMonad m) TestAlg Mock.BData
    }


mkNodeDescription :: (Monad m) => Int -> [Int] -> (TraceEvents -> m ()) -> TestNetLinkDescription m
mkNodeDescription ncFrom ncTo ncTraceCallback = TestNetLinkDescription
  { ncByzantine    = mempty
  , ncAppCallbacks = mempty
  , ..
  }


createTestNetwork
  :: (MonadMask m, MonadFork m, MonadTMMonitoring m)
  => [TestNetLinkDescription m]
  -> m ()
createTestNetwork = createTestNetworkWithConfig defCfg


-- | Create fully connected network with byzantine behaviour
--
createGossipTestNetwork :: (MonadMask m, MonadFork m, MonadTMMonitoring m)
                        => [(TestAppByzantine m, AppCallbacks (TestMonad m) TestAlg Mock.BData)]
                        -> m ()
createGossipTestNetwork byzs =
    let maxN = length byzs - 1
    in createTestNetworkWithValidatorsSetAndConfig
        testValidators
        (defCfg :: Configuration Example)
        [ (mkNodeDescription i [(i+1)..maxN] (\_ -> return ())) { ncByzantine = byz, ncAppCallbacks = appc }
        | (i, (byz, appc)) <- zip [0..] byzs
        ]


createTestNetworkWithConfig
    :: (MonadMask m, MonadFork m, MonadTMMonitoring m)
    => Configuration Example
    -> [TestNetLinkDescription m]
    -> m ()
createTestNetworkWithConfig = createTestNetworkWithValidatorsSetAndConfig testValidators


createTestNetworkWithValidatorsSetAndConfig
    :: (MonadIO m, MonadMask m, MonadFork m, MonadTMMonitoring m)
    => [PrivValidator TestAlg]
    -> Configuration Example
    -> [TestNetLinkDescription m]
    -> m ()
createTestNetworkWithValidatorsSetAndConfig validators cfg netDescr = do
    net <- liftIO newMockNet
    let vallist = map Just validators ++ repeat Nothing
    evalContT $ do
      acts <- forM (netDescr `zip` vallist) $ \(ndescr, val) -> do
        c <- ContT $ withConnection ":memory:"
        lift $ mkTestNode net (c, ndescr, val)
      lift $ catchAbort $ runConcurrently $ concat acts
  where
    dbValidatorSet = makeValidatorSetFromPriv validators
    mkTestNode
      :: (MonadFork m, MonadMask m, MonadTMMonitoring m)
      => MockNet
      -> ( Connection 'RW TestAlg Mock.BData
         , TestNetLinkDescription m
         , Maybe (PrivValidator TestAlg))
      -> m [m ()]
    mkTestNode net (conn, TestNetLinkDescription{..}, validatorPK) = do
        initDatabase conn (Mock.genesisBlock dbValidatorSet)
        --
        let run = runTracerT ncTraceCallback . runNoLogsT . runDBT conn
        (_,actions) <- run $ Mock.interpretSpec
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
          ( ncAppCallbacks
         <> mempty { appByzantine = ncByzantine }
          )
        return $ run <$> actions

intToNetAddr :: Int -> NetAddr
intToNetAddr i = NetAddrV4 (fromIntegral i) 1122
