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
import Control.Monad.Fail     (MonadFail)
import Control.Monad.IO.Class
import Control.Retry  (constantDelay, limitRetries, recovering)

import qualified Data.Map.Strict      as Map

import Data.Foldable  (toList)
import Data.Monoid    ((<>))
import System.Timeout (timeout)

import qualified HSChain.Mock.KeyVal as Mock
import HSChain.Blockchain.Internal.Engine.Types
import HSChain.Control
import HSChain.Crypto                           ((:&), Fingerprint, (:<<<))
import HSChain.Crypto.Ed25519                   (Ed25519)
import HSChain.Crypto.SHA                       (SHA512,SHA256)
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
--
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
mkNodeDescription ncFrom ncTo ncTraceCallback =
    let ncByzantine   = mempty
        ncAppCallbacks = mempty
    in TestNetLinkDescription {..}

type TestNetDescription m = [TestNetLinkDescription m]



createTestNetwork :: (MonadMask m, MonadFork m, MonadTMMonitoring m, MonadFail m)
                  => TestNetDescription m
                  -> m ()
createTestNetwork descr = createTestNetworkWithConfig (defCfg :: Configuration Example) descr


-- | Create fully connected network with byzantine behaviour
--
createGossipTestNetwork :: (MonadMask m, MonadFork m, MonadTMMonitoring m, MonadFail m)
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
    :: (MonadMask m, MonadFork m, MonadTMMonitoring m, MonadFail m)
    => Configuration Example
    -> TestNetDescription m
    -> m ()
createTestNetworkWithConfig = createTestNetworkWithValidatorsSetAndConfig testValidators


createTestNetworkWithValidatorsSetAndConfig
    :: forall m . (MonadIO m, MonadMask m, MonadFork m, MonadTMMonitoring m, MonadFail m)
    => [PrivValidator TestAlg]
    -> Configuration Example
    -> TestNetDescription m
    -> m ()
createTestNetworkWithValidatorsSetAndConfig validators cfg netDescr = do
    net <- liftIO newMockNet
    let vallist = map Just validators ++ repeat Nothing
    withMany (\(ndescr, val) cont -> withConnection ":memory:" (\c -> cont (c,ndescr,val))) (zip netDescr vallist) $ \lst -> do
      acts <- mapM (mkTestNode net) lst
      catchAbort $ runConcurrently $ join acts
  where
    dbValidatorSet = makeValidatorSetFromPriv validators
    mkTestNode
      :: MockNet
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
