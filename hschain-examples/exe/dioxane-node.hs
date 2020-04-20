{-# LANGUAGE ApplicativeDo              #-}
{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
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
import Control.Monad.Trans.Reader
import Data.Aeson             (FromJSON)
import Data.Word
import qualified Data.Vector as V
import Data.Yaml.Config       (loadYamlSettings, requireEnv)
import Options.Applicative

import Network.Wai.Middleware.Prometheus
import Prometheus   (register)
import Prometheus.Metric.GHC
import qualified Network.Wai.Handler.Warp as Warp

import GHC.Generics (Generic)

import HSChain.Blockchain.Internal.Engine.Types
import HSChain.Control
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

newtype MonitorT m a = MonitorT (ReaderT PrometheusGauges m a)
  deriving ( Functor,Applicative,Monad
           , MonadIO,MonadMask,MonadThrow,MonadCatch
           , MonadLogger,MonadFork)

instance MonadIO m =>  MonadTMMonitoring (MonitorT m) where
  usingCounter getter n = MonitorT $ flip addCounterNow n =<< asks getter
  usingGauge   getter x = MonitorT $ flip setTGaugeNow x =<< asks getter
  usingVector  getter l = MonitorT $ do
    g <- asks getter
    incTCVectorNow g l

runMonitorT :: PrometheusGauges -> MonitorT m a -> m a
runMonitorT g (MonitorT m) = runReaderT m g


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


data Opts = Opts
  { cmdConfigPath :: [FilePath]
  , optMaxH       :: Maybe Height
  }

data NodeCfg = NodeCfg
  { nodePort      :: Word16
  , nodeSeeds     :: [NetAddr]
  , nodeMaxH      :: Maybe Height
  , nodeIdx       :: Int
  }
  deriving (Show,Generic)
instance FromJSON NodeCfg

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
  nspec@NodeSpec{} :*: NodeCfg{..} :*: (cfg :: Configuration Example)
    <- loadYamlSettings (reverse cmdConfigPath) [] requireEnv
  startWebMonitoring $ fromIntegral nodePort + 1000
  -- Start node
  evalContT $ do
    -- Create network
    let bnet     = BlockchainNet { bchNetwork      = newNetworkTcp nodePort
                                 , bchInitialPeers = nodeSeeds
                                 }
    --
    (conn, logenv) <- allocNode nspec
    gauges         <- standardMonitoring
    lift $ runMonitorT gauges . runLoggerT logenv . runDBT conn $ do
      (RunningNode{..},acts) <- interpretSpec @_ @_ @DioTag
        (nspec :*: cfg :*: bnet)
        nodeIdx
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
