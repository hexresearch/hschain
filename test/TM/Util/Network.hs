{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}

-- |
module TM.Util.Network where


import Control.Monad
import Control.Monad.Catch
import Control.Monad.IO.Class
import Thundermint.Crypto.Ed25519
import qualified Data.Map               as Map
import Codec.Serialise
import GHC.Generics (Generic)

import Thundermint.Blockchain.Interpretation
import Thundermint.Blockchain.Types
import Thundermint.Consensus.Types
import Thundermint.Control
import Thundermint.Crypto
import Thundermint.Debug.Trace
import Thundermint.Logger
import Thundermint.Mock
import Thundermint.Mock.KeyVal
import Thundermint.P2P.Network
import Thundermint.Store
import Thundermint.Store.STM


testNetworkName :: String
testNetworkName = "tst"


testValidators :: Map.Map (Address Ed25519_SHA512) (PrivValidator Ed25519_SHA512)
testValidators = makePrivateValidators
  [ "2K7bFuJXxKf5LqogvVRQjms2W26ZrjpvUjo5LdvPFa5Y"
  , "4NSWtMsEPgfTK25tCPWqNzVVze1dgMwcUFwS5WkSpjJL"
  , "3Fj8bZjKc53F2a87sQaFkrDas2d9gjzK57FmQwnNnSHS"
  , "D2fpHM1JA8trshiUW8XPvspsapUvPqVzSofaK1MGRySd"
  ]


data TestNetLinkDescription m = TestNetLinkDescription
    { ncFrom   :: Int
    , ncTo     :: [Int]
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
        blockStorage <- liftIO $ newSTMBlockStorage genesisBlock validatorSet
        hChain       <- liftIO $ blockchainHeight blockStorage
        let run = runTracerT ncCallback . runNoLogsT
        fmap (map run) $ run $ do
            bchState     <- newBChState transitions
                          $ makeReadOnly (hoistBlockStorageRW liftIO blockStorage)
            _            <- stateAtH bchState (next hChain)
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

