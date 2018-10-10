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
import GHC.Generics       (Generic)

import qualified Control.Concurrent.Async as Async
import qualified Control.Exception        as E
import qualified Data.Map                 as Map
import qualified Network.Socket           as Net

import Thundermint.Blockchain.Interpretation
import Thundermint.Blockchain.Types
import Thundermint.Consensus.Types
import Thundermint.Control
import Thundermint.Crypto
import Thundermint.Crypto.Ed25519
import Thundermint.Debug.Trace
import Thundermint.Logger
import Thundermint.Mock
import Thundermint.Mock.KeyVal
import Thundermint.P2P.Network
import Thundermint.Store
import Thundermint.Store.SQLite
import Thundermint.Store.STM
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


createTestNetwork :: forall m . (MonadIO m, MonadMask m, MonadFork m) => TestNetDescription m -> m ()
createTestNetwork = createTestNetworkWithConfig defCfg


createTestNetworkWithConfig :: forall m . (MonadIO m, MonadMask m, MonadFork m) => Configuration -> TestNetDescription m -> m ()
createTestNetworkWithConfig cfg desc = do
    net <- liftIO newMockNet
    acts <- mapM (mkTestNode net) desc
    runConcurrently $ join acts
  where
    mkTestNode :: (MonadIO m, MonadMask m, MonadFork m) => MockNet TestAddr -> TestNetLinkDescription m -> m [m ()]
    mkTestNode net TestNetLinkDescription{..} = do
        let validatorSet = makeValidatorSetFromPriv testValidators
        blockStorage <- liftIO $ newSQLiteBlockStorage ":memory:" (genesisBlock validatorSet) validatorSet
        hChain       <- liftIO $ blockchainHeight blockStorage
        let run = runTracerT ncCallback . runNoLogsT
        fmap (map run) $ run $ do
            bchState     <- newBChState transitions
                          $ makeReadOnly (hoistBlockStorageRW liftIO blockStorage)
            _            <- stateAtH bchState (succ hChain)
            runNode cfg NodeDescription
                { nodeStorage         = hoistBlockStorageRW liftIO blockStorage
                , nodeBlockChainLogic = transitions
                , nodeNetwork         = createMockNode net testNetworkName (TestAddr ncFrom)
                , nodeAddr            = (TestAddr ncFrom, testNetworkName)
                , nodeInitialPeers    = map ((,testNetworkName) . TestAddr) ncTo
                , nodeValidationKey   = Nothing
                , nodeCommitCallback  = \_ -> return ()
                , nodeBchState        = bchState
                , nodeMempool         = nullMempoolAny
                }




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
