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
  ( module TM.Util.Network
  , NetPair
  ) where


import Control.Concurrent (threadDelay)

import qualified Control.Concurrent.Async as Async
import qualified Control.Exception        as E

import Control.Monad
import Control.Monad.Catch
import Control.Monad.Fail     (MonadFail)
import Control.Monad.IO.Class
import Control.Retry
    (RetryPolicy, constantDelay, limitRetries, recovering, skipAsyncExceptions)

import qualified Data.ByteString.Lazy as LBS
import qualified Data.Map.Strict      as Map

import Data.Monoid    ((<>))
import System.Timeout (timeout)

import Katip

import qualified Network.Socket as Net

import qualified HSChain.Mock.KeyVal as Mock

import HSChain.Blockchain.Internal.Engine.Types
import HSChain.Control
import HSChain.Crypto                           ((:&), Fingerprint, (:<<<))
import HSChain.Crypto.Ed25519                   (Ed25519)
import HSChain.Crypto.SHA                       (SHA512,SHA256)
import HSChain.Debug.Trace
import HSChain.Logger
import HSChain.Mock.Types
import HSChain.Monitoring
import HSChain.P2P
import HSChain.P2P.Network
import HSChain.Run
import HSChain.Store

import TM.RealNetwork


instance MonadLogger IO where
  logger severity str extra = putStrLn $ "LOG: " ++ show severity ++ ": " ++ show str ++ "(" ++ show (toObject extra) ++ ")"
  localNamespace _ act = act

shouldRetry :: Bool
shouldRetry = True
retryPolicy :: RetryPolicy
retryPolicy = constantDelay 500 <> limitRetries 20

withRetryTCP :: MonadIO m
             => Net.HostName
             -> (NetPair -> IO a)
             -> m a
withRetryTCP = withRetry . realNetPair Nothing 

withRetryUDP :: MonadIO m
             => Maybe (Maybe Int)
             -> Net.HostName
             -> (NetPair -> IO a)
             -> m a
withRetryUDP useUDP = withRetry . (realNetPair useUDP)

withRetryTLS :: MonadIO m
             => Net.HostName
             -> (NetPair -> IO a)
             -> m a
withRetryTLS = withRetry . realTlsNetPair

withRetry :: MonadIO m => (IO NetPair) -> (NetPair -> IO a) -> m a
withRetry newNetPair fun = do
  liftIO $ recovering retryPolicy hs
    (const $ newNetPair >>= fun)
    where
      -- | exceptions list to trigger the recovery logic
      hs :: [a -> Handler IO Bool]
      hs = [const $ Handler (\(_::E.IOException) -> return shouldRetry)]


withTimeoutRetry
  :: MonadIO m
  => String
  -> Int
  -> (IO NetPair)
  -> (NetPair -> IO a)
  -> m a
withTimeoutRetry msg t newNetPair fun = do
  liftIO $ recovering retryPolicy (skipAsyncExceptions ++ hs)
    (const action)
    where
      action = withTimeOut msg t (newNetPair >>= fun)
      -- | exceptions list to trigger the recovery logic
      hs :: [a -> Handler IO Bool]
      hs = [const $ Handler (\(_::E.IOException) -> return shouldRetry)]

-- | Exception for aborting the execution of test
data AbortTest = AbortTest String
                 deriving Show

instance Exception AbortTest

withTimeOut :: String -> Int -> IO a -> IO a
withTimeOut abortMsg t act = timeout t act >>= \case
    Just n  -> pure n
    Nothing -> E.throwIO $ AbortTest $ abortMsg <> " due to timeout"

-- TODO объединить в один список, а лучше сделать бесконечный
testValidators, extraTestValidators :: Map.Map (Fingerprint (SHA256 :<<< SHA512) TestAlg) (PrivValidator TestAlg)
testValidators = makePrivateValidators
  [ "2K7bFuJXxKf5LqogvVRQjms2W26ZrjpvUjo5LdvPFa5Y"
  , "4NSWtMsEPgfTK25tCPWqNzVVze1dgMwcUFwS5WkSpjJL"
  , "3Fj8bZjKc53F2a87sQaFkrDas2d9gjzK57FmQwnNnSHS"
  , "D2fpHM1JA8trshiUW8XPvspsapUvPqVzSofaK1MGRySd"
  ]


extraTestValidators = makePrivateValidators
  [ "EiG2NbUV9ofWeyXcoCkJYvuyLEEQhyeWtGh5n9fcobQi"
  , "CPSbRfUkV6QcUSBucJ6YN1zVm2h8ZMBNdBhm4Fwd46km"
  , "GGy5NQVwQxuLBLewmgiDziMAbEN3tfNHetCFrF44n5uL"
  , "ENf7S1yqKQyeBC59RJWgGB5qHA3GYzwaxHGx1MTXiyts"
  , "EbG1Bo6NBPuYVT5u44epdjfbYiSaqLzhTWg7NghwDstp"
  , "BtrJnk73h4WYxfiYJYuw85bhFUi2y6mYpxac2ChoUyHK"
  , "4eWJ5jSzaBUjkP5kG7gJa3Ad9btjbXMVBxiMQq59M774"
  , "E8Su8SBtP6F3UjwjSTmZbccD3U8B1amP5syyLyLUY732"
  , "9ygyNr894jDvMoymQhdEsrnrxotXKfyKETUkH8qM2LCc"
  , "9Yjk2NmuirUtf8b15jRSzHM5H7L2G1hoN6HTsRAJ8pCF"
  ]


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


type TestNetNode = ()


type TestNet = Map.Map Int TestNetNode


toPair :: TestNetLinkDescription m -> (Int, [Int])
toPair TestNetLinkDescription{..} = (ncFrom, ncTo)


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


{-
createTestNetworkWithValidatorsSetAndConfig
  :: forall m app . (MonadIO m, MonadMask m, MonadFork m, MonadTMMonitoring m, MonadFail m)
  => Map.Map (Fingerprint (SHA256 :<<< SHA512) (Ed25519 :& SHA512)) (PrivValidator (Ed25519 :& SHA512))
  -> Configuration app -> TestNetDescription m -> m ()
createTestNetworkWithValidatorsSetAndConfig validatorsSet cfg desc = do
    net  <- liftIO newMockNet
    withMany (\descr cont -> withConnection ":memory:" (\c -> cont (c,descr))) desc $ \descrList -> do
      acts <- mapM (mkTestNode net) descrList
      runConcurrently $ join acts
  where
    mkTestNode
      :: MockNet
      -> (Connection 'RW (Ed25519 :& SHA512) [(String, NetAddr)], TestNetLinkDescription m)
      -> m [m ()]
-}


createTestNetworkWithValidatorsSetAndConfig
    :: forall m . (MonadIO m, MonadMask m, MonadFork m, MonadTMMonitoring m, MonadFail m)
    => Map.Map (Fingerprint (SHA256 :<<< SHA512) TestAlg) (PrivValidator TestAlg)
    -> Configuration Example
    -> TestNetDescription m
    -> m ()
createTestNetworkWithValidatorsSetAndConfig validators cfg netDescr = do
    net <- liftIO newMockNet
    let vallist = map (Just . snd) (Map.toList validators) ++ repeat Nothing
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
            -- store <- newSTMBchStorage $ initialState Mock.transitions
            -- logic <- makeAppLogic store Mock.transitions Mock.runner
            -- runNode cfg NodeDescription
            --   { nodeValidationKey = validatorPK
            --   , nodeCallbacks     = ncAppCallbacks
            --                      <> mempty { appByzantine = ncByzantine }
            --   , nodeLogic         = logic
            --   , nodeNetwork       = BlockchainNet
            --     { bchNetwork        = createMockNode net (intToNetAddr ncFrom)
            --     , bchInitialPeers   = map intToNetAddr ncTo
            --     }
            --   }


-- | UDP may return Nothings for the message receive operation.
skipNothings :: String -> (a -> IO (Maybe LBS.ByteString)) -> a -> IO LBS.ByteString
skipNothings _lbl recv conn = do
  mbMsg <- recv conn
  case mbMsg of
    Just msg -> return msg
    Nothing  -> skipNothings _lbl recv conn


-- | Simple test to ensure that mock network works at all
delayedWrite :: NetPair -> IO ()
delayedWrite ((serverAddr, server), (_, client)) = do
  let runServer NetworkAPI{..} =
        bracket listenOn fst $ \(_,accept) ->
          bracket accept (close . fst) $ \(conn,_) -> do
            "A1" <- skipNothings "A1" recv conn
            "A2" <- skipNothings "A2" recv conn
            "A3" <- skipNothings "A3" recv conn
            return ()
  let runClient NetworkAPI{..} = do
        threadDelay 10e3
        bracket (connect serverAddr) close $ \conn -> do
          send conn "A1"
          threadDelay 30e3
          send conn "A2"
          threadDelay 30e3
          send conn "A3"
  ((),()) <- Async.concurrently (runServer server) (runClient client)
  return ()

intToNetAddr :: Int -> NetAddr
intToNetAddr i = NetAddrV4 (fromIntegral i) 1122