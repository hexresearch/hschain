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
import Data.Monoid            ((<>))
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
import HSChain.Store
import HSChain.Monitoring
import HSChain.Crypto         (PublicKey)
import HSChain.P2P            (generatePeerId)
import HSChain.P2P.Network    (newNetworkTcp)
import HSChain.Types
import HSChain.Debug.Trace

import qualified HSChain.P2P.Types as P2PT


----------------------------------------------------------------
--
----------------------------------------------------------------

newtype MonitorT m a = MonitorT (ReaderT PrometheusGauges m a)
  deriving ( Functor,Applicative,Monad
           , MonadIO,MonadMask,MonadThrow,MonadCatch
           , MonadLogger,MonadFork,MonadTrace )

instance MonadIO m =>  MonadTMMonitoring (MonitorT m) where
  usingCounter getter n   = MonitorT $ flip addCounterNow n =<< asks getter
  usingGauge   getter x   = MonitorT $ flip setTGaugeNow x =<< asks getter
  usingVector  getter l x = MonitorT $ do
    g <- asks getter
    setTGVectorNow g l x

runMonitorT :: PrometheusGauges -> MonitorT m a -> m a
runMonitorT g (MonitorT m) = runReaderT m g

data Opts = Opts
  { cmdConfigPath :: [FilePath]
  , optMaxH       :: Maybe Height
  }

data NodeCfg = NodeCfg
  { validatorKeys :: [PublicKey Alg]
  , nodePort      :: Word16
  , nodeSeeds     :: [P2PT.NetAddr]
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
    --- Allocate resources
    (conn, logenv) <- allocNode genesis nspec
    gauges         <- standardMonitoring
    let run = runMonitorT gauges . runLoggerT logenv . runDBT conn
    -- Create network
    peerId <- generatePeerId
    let peerInfo = P2PT.PeerInfo peerId nodePort 0
        bnet     = BlockchainNet { bchNetwork      = newNetworkTcp peerInfo
                                 , bchInitialPeers = nodeSeeds
                                 }
    -- Actually run node
    lift $ run $ do
      (RunningNode{..},acts) <- interpretSpec
        (nspec :*: cfg :*: bnet)
        (maybe mempty callbackAbortAtH (optMaxH <|> nodeMaxH))
      txGen <- case mtxGen of
        Nothing  -> return []
        Just txG -> do
          cursor <- getMempoolCursor rnodeMempool
          return [transactionGenerator txG
                    rnodeMempool
                    (snd <$> bchCurrentState rnodeState)
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
