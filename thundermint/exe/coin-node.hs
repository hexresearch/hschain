{-# LANGUAGE ApplicativeDo       #-}
{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE DataKinds           #-}
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
import Control.Monad.Catch
import Control.Monad.IO.Class
import Data.Monoid            ((<>))
import Options.Applicative
import System.Directory       (createDirectoryIfMissing)
import System.FilePath        (splitFileName)

import Katip.Core                        (showLS)
import Network.Wai.Middleware.Prometheus
import Prometheus
import Prometheus.Metric.GHC

import qualified Network.Wai.Handler.Warp as Warp

import Thundermint.Blockchain.Internal.Engine.Types
import Thundermint.Logger
import Thundermint.Mock.Coin
import Thundermint.Mock.Types
import Thundermint.Run

import Thundermint.Crypto         ((:&))
import Thundermint.Crypto.Ed25519 (Ed25519)
import Thundermint.Crypto.SHA     (SHA512)
import Thundermint.P2P            (generatePeerId)
import Thundermint.P2P.Network    (getLocalAddress, realNetwork)

import qualified Thundermint.P2P.Types as P2PT

import App.Settings
import App.Yaml

----------------------------------------------------------------
--
----------------------------------------------------------------

data Opts = Opts
    { cmdConfigPath :: FilePath
    }


main :: IO ()
main = do
    Opts{..} <- customExecParser (prefs showHelpOnError)
              $ info (helper <*> parser)
                (  fullDesc
                <> header   "Coin node settings"
                <> progDesc ""
                )
    CoinSettings{settings'node = CoinNodeSpec{..}
                  , settings'coin = CoinSpec{..}
                  , settings'logs = LogSpec{..}} <- readYaml cmdConfigPath
    let loggers = [ makeScribe s { scribe'path = scribe'path s }
                  | s <- logSpec'logFiles
                  ]
    let netCfg = let change cfg = cfg { cfgNetwork = (cfgNetwork cfg) { pexMinKnownConnections = nspec'count - 1} }
                 in change (defCfg :: Configuration Example) :: Configuration Example

    let port = 1000 + fromIntegral nspec'port
    startWebMonitoring port

    liftIO $ createDirectoryIfMissing True $ fst $ splitFileName nspec'dbName
    withLogEnv "TM" "DEV" loggers $ \logenv -> runLoggerT logenv $ do
      let !validatorSet = makeValidatorSetFromPriv
                        $ (id @[PrivValidator (Ed25519 :& SHA512)])
                        $ nspec'validators
      logger InfoS "Listening for bootstrap adresses" ()
      logger InfoS ("net Addresses: " <> showLS nspec'seeds) ()
      nodeAddr     <- liftIO getLocalAddress
      logger InfoS ("local Address: " <> showLS nodeAddr) ()
      peerId <- generatePeerId
      logger InfoS ("peer Id generated: " <> showLS peerId) ()
      let peerInfo = P2PT.PeerInfo peerId (fromIntegral nspec'port) 0
      logger InfoS ("peer Info generated: " <> showLS peerInfo) ()
      let netAPI = realNetwork peerInfo
      logger InfoS ("network API created") ()
      let net = BlockchainNet
                   { bchNetwork          = netAPI
                   , bchInitialPeers     = nspec'seeds
                   }
          genSpec = restrictGenerator nspec'number nspec'count
                  $ defaultGenerator coin'keys coin'deposit coin'delay
          genesis = genesisFromGenerator validatorSet genSpec
      catch (do
        (_,act) <- interpretSpec
          (Just coin'maxH)
          genesis
          netCfg
          net
          (NodeSpec nspec'privKey (Just nspec'dbName) logSpec'logFiles coin'walletKeys)
          genSpec
        logger InfoS ("spec has been interpreted") ()
        act `catch` (\e -> logger InfoS ("Exiting due to "<> (showLS (e :: SomeException))) ())
        ) (\e -> logger InfoS ("Exiting interpretSpec sequence due to "<> (showLS (e :: SomeException))) ())
      logger InfoS "Normal exit" ()
      liftIO $ print $ "Node number " <> show nspec'number <> " Normal exit"


parser :: Parser Opts
parser = do
  cmdConfigPath <- strArgument
    (  metavar "PATH"
    <> help "Path to configuration"
    <> value "config/coin-node1.yaml"
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

