{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE NumDecimals         #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}

-- |
module TM.Util.Network where


import Codec.Serialise
import Control.Monad
import Control.Monad.Catch
import Control.Monad.IO.Class

import Control.Concurrent (threadDelay)
import Control.Retry      (RetryPolicy, constantDelay, limitRetries, recovering)
import Data.Monoid        ((<>))
import Data.Proxy         (Proxy(..))
import GHC.Generics       (Generic)

import qualified Control.Concurrent.Async as Async
import qualified Control.Exception        as E
import qualified Data.Map                 as Map
import qualified Network.Socket           as Net

import Thundermint.Blockchain.Internal.Engine.Types
import Thundermint.Control
import Thundermint.Crypto
import Thundermint.Crypto.Ed25519
import Thundermint.Debug.Trace
import Thundermint.Logger
import Thundermint.Run
import Thundermint.Mock.KeyVal
import Thundermint.Mock.Types
import Thundermint.P2P.Network
import Thundermint.Store
import Thundermint.Monitoring
import TM.RealNetwork

testNetworkName :: String
testNetworkName = "tst"

shouldRetry :: Bool
shouldRetry = True
retryPolicy :: RetryPolicy
retryPolicy = constantDelay 500 <> limitRetries 20

withRetry :: MonadIO m => ( (Net.SockAddr, NetworkAPI Net.SockAddr)
                        -> (Net.SockAddr, NetworkAPI Net.SockAddr) -> IO a)
         -> Net.HostName -> m a
withRetry fun host = do
  liftIO $ recovering retryPolicy hs (const $ realNetPair host >>= uncurry fun)
    where
      -- | exceptions list to trigger the recovery logic
      hs :: [a -> Handler IO Bool]
      hs = [const $ Handler (\(_::E.IOException) -> return shouldRetry)]


testValidators :: Map.Map (Address Ed25519_SHA512) (PrivValidator Ed25519_SHA512)
testValidators = makePrivateValidators
  [ "2K7bFuJXxKf5LqogvVRQjms2W26ZrjpvUjo5LdvPFa5Y"
  , "4NSWtMsEPgfTK25tCPWqNzVVze1dgMwcUFwS5WkSpjJL"
  , "3Fj8bZjKc53F2a87sQaFkrDas2d9gjzK57FmQwnNnSHS"
  , "D2fpHM1JA8trshiUW8XPvspsapUvPqVzSofaK1MGRySd"
  ]


data TestNetLinkDescription m = TestNetLinkDescription
    { ncFrom     :: Int
    , ncTo       :: [Int]
    , ncCallback :: TraceEvents -> m ()
    }


type TestNetDescription m = [TestNetLinkDescription m]


type TestNetNode = ()


type TestNet = Map.Map Int TestNetNode


newtype TestAddr = TestAddr Int deriving (Show, Ord, Eq, Generic)

instance Serialise TestAddr


toPair :: TestNetLinkDescription m -> (Int, [Int])
toPair TestNetLinkDescription{..} = (ncFrom, ncTo)


createTestNetwork :: (MonadIO m, MonadMask m, MonadFork m, MonadTMMonitoring m)
                  => TestNetDescription m -> m ()
createTestNetwork = createTestNetworkWithConfig (defCfg :: Configuration Example)


createTestNetworkWithConfig
  :: forall m app . (MonadIO m, MonadMask m, MonadFork m, MonadTMMonitoring m)
  => Configuration app -> TestNetDescription m -> m ()
createTestNetworkWithConfig cfg desc = do
    net  <- liftIO newMockNet
    withMany (\descr cont -> withConnection ":memory:" (\c -> cont (c,descr))) desc $ \descrList -> do
      acts <- mapM (mkTestNode net) descrList
      runConcurrently $ join acts
  where
    mkTestNode
      :: (MonadIO m, MonadMask m, MonadFork m)
      => MockNet TestAddr
      -> (Connection 'RW Ed25519_SHA512 [(String,Int)], TestNetLinkDescription m)
      -> m [m ()]
    mkTestNode net (conn, TestNetLinkDescription{..}) = do
        let validatorSet = makeValidatorSetFromPriv testValidators
        initDatabase conn Proxy (genesisBlock validatorSet) validatorSet
        --
        let run = runTracerT ncCallback . runNoLogsT . runDBT conn
        fmap (map run) $ run $ do
            (_,logic) <- logicFromFold transitions
            runNode cfg
              BlockchainNet
                { bchNetwork          = createMockNode net testNetworkName (TestAddr ncFrom)
                , bchLocalAddr        = (TestAddr ncFrom, testNetworkName)
                , bchInitialPeers     = map ((,testNetworkName) . TestAddr) ncTo
                }
              NodeDescription
                { nodeCommitCallback   = \_ -> return ()
                , nodeValidationKey    = Nothing
                , nodeReadyCreateBlock = \_ _ -> return True
                }
              logic

-- | Simple test to ensure that mock network works at all
delayedWrite :: (addr, NetworkAPI addr)
         -> (addr, NetworkAPI addr)
         -> IO ()
delayedWrite (serverAddr, server) (_, client) = do
  let runServer NetworkAPI{..} =
        bracket listenOn fst $ \(_,accept) ->
          bracket accept (close . fst) $ \(conn,_) -> do
            Just "A1" <- recv conn
            Just "A2" <- recv conn
            Just "A3" <- recv conn
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
