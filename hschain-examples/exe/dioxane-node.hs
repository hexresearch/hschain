{-# LANGUAGE ApplicativeDo              #-}
{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
import Control.Arrow      ((&&&))
import Control.Concurrent (forkIO)
import Control.Monad
import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Cont
import Control.Monad.Reader
import Data.Aeson             (FromJSON)
import Data.Word
import qualified Data.Vector as V
import Data.Yaml.Config       (loadYamlSettings, requireEnv)
import Options.Applicative

import Katip (Namespace,LogEnv)
import Network.Wai.Middleware.Prometheus
import Prometheus   (register)
import Prometheus.Metric.GHC
import qualified Network.Wai.Handler.Warp as Warp

import GHC.Generics (Generic)

import HSChain.Config
import HSChain.Control.Class
import HSChain.Crypto         (publicKey)
import HSChain.Logger
import HSChain.Mock
import HSChain.Mock.Dioxane
import HSChain.Mock.KeyList
import HSChain.Mock.Types
import HSChain.Monitoring
import HSChain.Network.TCP (newNetworkTcp)
import HSChain.Run
import HSChain.Store
import HSChain.Types
import HSChain.Network.Types (NetAddr)


----------------------------------------------------------------
--
----------------------------------------------------------------

data DioTag

instance Dio DioTag where
  dioDict = DioDict
    { dioUserKeys       = V.fromList
                        $ take 5000
                        $ map (id &&& publicKey)
                        $ makePrivKeyStream 1337
    , dioInitialBalance = 1000000
    , dioValidators     = 4
    }


data AppDict = AppDict
  { dictGauges    :: !PrometheusGauges
  , dictNamespace :: !Namespace
  , dictLogEnv    :: !LogEnv
  , dictConn      :: !(Connection 'RW)
  , dictCached    :: !(Cached (BData DioTag))
  }
  deriving stock (Generic)

newtype AppT m a = AppT { _unAppT :: ReaderT AppDict m a }
  deriving newtype (Functor,Applicative,Monad,MonadIO)
  deriving newtype (MonadThrow,MonadCatch,MonadMask,MonadFork)
  deriving MonadLogger                  via LoggerByTypes    (ReaderT AppDict m)
  deriving MonadTMMonitoring            via MonitoringByType (ReaderT AppDict m)
  deriving (MonadReadDB, MonadDB)       via DatabaseByType   (ReaderT AppDict m)
  deriving (MonadCached (BData DioTag)) via CachedByType (BData DioTag) (ReaderT AppDict m)

runAppT :: MonadIO m => LogEnv -> PrometheusGauges -> Connection 'RW -> AppT m a -> m a
runAppT lenv g conn (AppT act) = do
  cached <- newCached
  runReaderT act AppDict { dictGauges    = g
                         , dictNamespace = mempty
                         , dictLogEnv    = lenv
                         , dictConn      = conn
                         , dictCached    = cached
                         }


data Opts = Opts
  { cmdConfigPath :: [FilePath]
  , optMaxH       :: Maybe Height
  }

data NodeConfig = NodeConfig
  { nodeSpec      :: NodeSpec (BData DioTag)
  , nodeDelays    :: Configuration Example
  , nodePort      :: Word16
  , nodeSeeds     :: [NetAddr]
  , nodeMaxH      :: Maybe Height
  , nodeIdx       :: Int
  }
  deriving (Generic)
  deriving FromJSON via TopConfig (SnakeCase (DropSmart (Config NodeConfig)))

main :: IO ()
main = do
  -- Parse CLI
  Opts{..} <- customExecParser (prefs showHelpOnError)
            $ info (helper <*> parser)
              (  fullDesc
              <> header   "Coin node settings"
              <> progDesc ""
              )
  -- Read config.
  --
  -- NOTE: later files take precedence
  NodeConfig{..} <- loadYamlSettings (reverse cmdConfigPath) [] requireEnv
  startWebMonitoring $ fromIntegral nodePort + 1000
  -- Start node
  evalContT $ do
    -- Create network
    let bnet     = BlockchainNet { bchNetwork      = newNetworkTcp nodePort
                                 , bchInitialPeers = nodeSeeds
                                 }
    --
    (conn, logenv) <- allocNode nodeSpec
    gauges         <- standardMonitoring
    lift $ runAppT logenv gauges conn $ do
      (_,acts) <- interpretSpec @_ @DioTag
        nodeIdx
        bnet
        nodeDelays
        (maybe mempty callbackAbortAtH (optMaxH <|> nodeMaxH))
      logOnException $ runConcurrently acts

    
parser :: Parser Opts
parser = do
  optMaxH <- optional $ Height <$> option auto
    (  metavar "N"
    <> long "max-h"
    <> help "Maximum height"
    )
  cmdConfigPath <- some $ strArgument
    (  metavar "PATH"
    <> help  "Path to configuration"
    <> showDefault
    )
  return Opts{..}

startWebMonitoring :: Warp.Port -> IO ()
startWebMonitoring port = do
    void $ register ghcMetrics
    void $ forkIO
         $ Warp.run port
         $ prometheus def { prometheusInstrumentPrometheus = False }
                      metricsApp
