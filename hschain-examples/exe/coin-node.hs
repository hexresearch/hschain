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
import Control.Concurrent
import Control.Monad
import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Cont
import Data.Aeson             (FromJSON)
import Data.Word
import Data.Yaml.Config       (loadYamlSettings, requireEnv)
import Options.Applicative

import Network.Wai.Middleware.Prometheus
import Prometheus   (register)
import Prometheus.Metric.GHC
import GHC.Generics (Generic)
import qualified Network.Wai.Handler.Warp as Warp

import HSChain.Blockchain.Internal.Engine.Types
import HSChain.Logger
import HSChain.Mock.Coin
import HSChain.Mock.Types
import HSChain.Run

import HSChain.Control
import HSChain.Control.Class
import HSChain.Store
import HSChain.Monitoring
import HSChain.Mock
import HSChain.Crypto         (PublicKey)
import HSChain.Network.TCP    (newNetworkTcp)
import HSChain.Types
import HSChain.Types.Merkle.Types
import HSChain.Network.Types  (NetAddr)


----------------------------------------------------------------
--
----------------------------------------------------------------

newtype MonitorT m a = MonitorT (ReaderT PrometheusGauges m a)
  deriving ( Functor,Applicative,Monad
           , MonadIO,MonadMask,MonadThrow,MonadCatch
           , MonadLogger,MonadFork )

instance MonadIO m =>  MonadTMMonitoring (MonitorT m) where
  usingCounter getter n = MonitorT $ flip addCounterNow n =<< asks getter
  usingGauge   getter x = MonitorT $ flip setTGaugeNow x =<< asks getter
  usingVector  getter l = MonitorT $ do
    g <- asks getter
    incTCVectorNow g l

runMonitorT :: PrometheusGauges -> MonitorT m a -> m a
runMonitorT g (MonitorT m) = runReaderT m g

data Opts = Opts
  { cmdConfigPath :: [FilePath]
  , optMaxH       :: Maybe Height
  }

data NodeCfg = NodeCfg
  { validatorKeys :: [PublicKey (Alg BData)]
  , nodePort      :: Word16
  , nodeSeeds     :: [NetAddr]
  , nodeMaxH      :: Maybe Height
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
  coin :*: nspec@NodeSpec{} :*: NodeCfg{..} :*: (cfg :: Configuration Example)
    <- loadYamlSettings (reverse cmdConfigPath) [] requireEnv
  startWebMonitoring $ fromIntegral nodePort + 1000
  -- Start node
  evalContT $ do
    let (mtxGen, genesis) = mintMockCoin [ Validator v 1 | v <- validatorKeys] coin
    -- Create network
    let bnet     = BlockchainNet { bchNetwork      = newNetworkTcp nodePort
                                 , bchInitialPeers = nodeSeeds
                                 }
    --- Allocate resources
    (conn, logenv) <- allocNode nspec
    gauges         <- standardMonitoring
    let run = runMonitorT gauges . runLoggerT logenv . runDBT conn
    -- Actually run node
    lift $ run $ do
      (RunningNode{..},acts) <- interpretSpec
        genesis
        cfg
        bnet
        nspec
        (maybe mempty callbackAbortAtH (optMaxH <|> nodeMaxH))
      txGen <- case mtxGen of
        Nothing  -> return []
        Just txG -> do
          cursor <- getMempoolCursor rnodeMempool
          return [transactionGenerator txG
                    rnodeMempool
                    (merkleValue . snd <$> bchCurrentState rnodeState)
                    (void . pushTransaction cursor)]
      logOnException $ runConcurrently $ txGen ++ acts

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

----------------------------------------------------------------
--
----------------------------------------------------------------

startWebMonitoring :: Warp.Port -> IO ()
startWebMonitoring port = do
    void $ register ghcMetrics
    void $ forkIO
         $ Warp.run port
         $ prometheus def
                      { prometheusInstrumentPrometheus = False }
                      metricsApp
