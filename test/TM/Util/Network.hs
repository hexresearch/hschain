{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}

-- |
module TM.Util.Network where


import Data.List
import Control.Monad.Catch
import Control.Monad.IO.Class
import Thundermint.Crypto.Ed25519
import qualified Data.Map               as Map
import Codec.Serialise
import GHC.Generics (Generic)
import qualified Data.Set as Set

import Thundermint.Blockchain.Types
import Thundermint.Control
import Thundermint.Crypto
import Thundermint.Debug.Trace
import Thundermint.Logger
import Thundermint.Mock
import Thundermint.Mock.KeyVal
import Thundermint.P2P.Network
import Thundermint.Store
import Thundermint.Store.STM


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


removeMutualConnections :: TestNetDescription m -> TestNetDescription m
removeMutualConnections =
    reverse .
    snd .
    foldl' (\(pairs, result) td@TestNetLinkDescription{..} ->
                    let ncTo' = filter (\t -> (ncFrom,t) `Set.notMember` pairs) ncTo
                    in ( pairs  `Set.union` Set.fromList (map (,ncFrom) ncTo' ++ map (ncFrom,) ncTo')
                       , td { ncTo = ncTo' } : result
                       )
           )
           (Set.empty, [])


createTestNetwork :: forall m . (MonadIO m, MonadMask m, MonadFork m) => TestNetDescription m -> m ()
createTestNetwork desc' = do
    let desc = removeMutualConnections desc' -- omit bug ...
    net <- liftIO newMockNet
    runConcurrently $ map (mkTestNode net) desc
  where
    mkTestNode :: (MonadIO m, MonadMask m, MonadFork m) => MockNet TestAddr -> TestNetLinkDescription m -> m ()
    mkTestNode net TestNetLinkDescription{..} = do
        let validatorSet = makeValidatorSetFromPriv testValidators
        blockStorage <- liftIO $ newSTMBlockStorage genesisBlock validatorSet
        runTracerT ncCallback $ runNoLogsT $ runNode NodeDescription
            { nodeStorage         = hoistBlockStorageRW liftIO blockStorage
            , nodeBlockChainLogic = transitions
            , nodeNetworks        = createMockNode net "tst" (TestAddr ncFrom)
            , nodeAddr            = (TestAddr ncFrom, "tst")
            , nodeInitialPeers    = map ((,"tst") . TestAddr) ncTo
            , nodeValidationKey   = Nothing
            , nodeAction          = Nothing
            , nodeCommitCallback  = \_ -> return ()
            }

