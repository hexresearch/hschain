{-# LANGUAGE ApplicativeDo       #-}
{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
import Control.Concurrent
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Cont
import Data.Aeson             (FromJSON)
import Data.Monoid            ((<>))
import Data.Word
import Data.Yaml.Config       (loadYamlSettings, requireEnv)
import Options.Applicative

import Network.Wai.Middleware.Prometheus
import Prometheus
import Prometheus.Metric.GHC
import GHC.Generics (Generic)
import qualified Network.Wai.Handler.Warp as Warp

import Thundermint.Blockchain.Internal.Engine.Types
import Thundermint.Logger
import Thundermint.Mock.Coin
import Thundermint.Mock.Types
import Thundermint.Run

import Thundermint.Control
import Thundermint.Blockchain.Interpretation
import Thundermint.Store
import Thundermint.Crypto         (PublicKey)
import Thundermint.P2P            (generatePeerId)
import Thundermint.P2P.Network    (newNetworkTcp)
import Thundermint.Types

import qualified Thundermint.P2P.Types as P2PT


----------------------------------------------------------------
--
----------------------------------------------------------------

data Opts = Opts
  { cmdConfigPath :: [FilePath]
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
    (_, conn, logenv) <- allocNode genesis nspec
    let run = runLoggerT logenv . runDBT conn
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
        (maybe mempty callbackAbortAtH nodeMaxH)
      txGen <- case mtxGen of
        Nothing  -> return []
        Just txG -> do
          cursor <- getMempoolCursor rnodeMempool
          return [transactionGenerator txG (currentState rnodeState) (void . pushTransaction cursor)]
      logOnException $ runConcurrently $ txGen ++ acts

parser :: Parser Opts
parser = do
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
