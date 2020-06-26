{-# LANGUAGE ApplicativeDo              #-}
{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
import Control.Concurrent
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Cont
import Data.Yaml.Config       (loadYamlSettings, requireEnv)
import Options.Applicative

import Network.Wai.Middleware.Prometheus
import Prometheus   (register)
import Prometheus.Metric.GHC
import qualified Network.Wai.Handler.Warp as Warp

import HSChain.Blockchain.Internal.Engine.Types
import HSChain.Logger
import HSChain.Mock.Coin
import HSChain.Mock.Types
import HSChain.Run

import HSChain.Control.Class
import HSChain.Monitoring
import HSChain.Mock
import HSChain.Network.TCP    (newNetworkTcp)
import HSChain.Types


----------------------------------------------------------------
--
----------------------------------------------------------------

data Opts = Opts
  { cmdConfigPath :: [FilePath]
  , optMaxH       :: Maybe Height
  }
    

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
  SingleNodeConfig{..} :: SingleNodeConfig BData CoinSpecification <-
    loadYamlSettings (reverse cmdConfigPath) [] requireEnv
  startWebMonitoring $ fromIntegral (realnetPort snodeNet) + 1000
  -- Start node
  evalContT $ do
    --- Allocate resources
    (dictConn, dictLogEnv) <- allocNode snodeSpec
    dictGauges             <- standardMonitoring
    -- Create network
    let bnet     = BlockchainNet { bchNetwork      = newNetworkTcp $ realnetPort snodeNet
                                 , bchInitialPeers = realnetSeeds snodeNet
                                 }
    -- Dictionatry with paremeters
    let dict = CoinDictM { dictNamespace = mempty
                         , ..
                         }
    lift $ runCoinT dict $ do
      (_,threads) <- interpretSpec
        snodeCfg
        bnet
        snodeSpec
        [ Validator v 1 | v <- snodeValidators]
        snodeBchData
        (maybe mempty callbackAbortAtH optMaxH)
      logOnException $ runConcurrently $ threads


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
