{-# LANGUAGE ApplicativeDo       #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
import Control.Concurrent
import Control.Monad
import Control.Monad.Catch
import Control.Monad.IO.Class
import Data.Int
import Options.Applicative

import Katip.Core           (showLS)
import Network.Socket       (SockAddr(..), PortNumber)
import Network.Wai.Middleware.Prometheus
import Prometheus
import Prometheus.Metric.GHC
import System.Environment   (getEnv,lookupEnv)
import System.FilePath      ((</>))
import qualified Network.Wai.Handler.Warp as Warp

import Thundermint.Blockchain.Internal.Engine.Types
import Thundermint.Crypto.Ed25519            (Ed25519_SHA512)
import Thundermint.Logger
import Thundermint.Run
import Thundermint.Mock.Coin
import Thundermint.Mock.Types
import Thundermint.P2P.Consts
import Thundermint.P2P.Instances ()
import Thundermint.P2P.Network               ( getLocalAddress, realNetwork
                                             , getCredentialFromBuffer, realNetworkTls)
import qualified Data.Aeson            as JSON
import qualified Data.ByteString.Char8 as BC8


----------------------------------------------------------------
-- Cmd line specification
----------------------------------------------------------------
data Opts = Opts
  { maxH              :: Maybe Int64
  , listenPort        :: PortNumber
  , prefix            :: FilePath
  , delay             :: Int
  , doValidate        :: Bool
  , netInitialDeposit :: Integer
  , netInitialKeys    :: Int
  , nodeNumber        :: Int
  , totalNodes        :: Int
  , optTls            :: Bool
  , netAddresses      :: [SockAddr]
  }

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

----------------------------------------------------------------
--
----------------------------------------------------------------

main :: IO ()
main = do
    Opts{..} <- customExecParser (prefs showHelpOnError)
              $ info (helper <*> parser)
                (  fullDesc
                <> header   "Coin test program"
                <> progDesc ""
                )
    nodeSpecStr <- BC8.pack <$> getEnv    "THUNDERMINT_NODE_SPEC"
    netParamStr <- lookupEnv "THUNDERMINT_NET_PARAM"
    ipMapPath   <- BC8.pack <$> getEnv    "THUNDERMINT_KEYS"
    let netCfg = case netParamStr of
          Nothing -> defCfg :: Configuration Example
          Just s  -> either error id $ JSON.eitherDecodeStrict' $ BC8.pack s
    let nodeSpec@NodeSpec{..} = either error id $ JSON.eitherDecodeStrict' nodeSpecStr
        loggers = [ makeScribe s { scribe'path = fmap (prefix </>) (scribe'path s) }
                  | s <- nspecLogFile
                  ]
    let port = 1000 + fromIntegral listenPort
    startWebMonitoring port
    withLogEnv "TM" "DEV" loggers $ \logenv -> runLoggerT "Coin" logenv $ do
      let !validatorSet = makeValidatorSetFromPriv
                        $ (id @[PrivValidator Ed25519_SHA512])
                        $ either error (fmap PrivValidator)
                        $ JSON.eitherDecodeStrict' ipMapPath
      logger InfoS "Listening for bootstrap adresses" ()
      nodeAddr     <- liftIO getLocalAddress
      logger InfoS ("net Addresses: " <> showLS netAddresses) ()
      netAPI <- case optTls of
        False -> return $ realNetwork (show listenPort)
        True  -> liftIO $ do
          keyPem  <- BC8.pack <$> getEnv "KEY_PEM"
          certPem <- BC8.pack <$> getEnv "CERT_PEM"
          let credential = getCredentialFromBuffer certPem keyPem
          return $ realNetworkTls credential (show thundermintPort)
      let net = BlockchainNet
                   { bchNetwork          = netAPI
                   , bchLocalAddr        = nodeAddr
                   , bchInitialPeers     = netAddresses
                   }
          genSpec = restrictGenerator nodeNumber totalNodes
                  $ defaultGenerator netInitialKeys netInitialDeposit delay
      (_,act) <- interpretSpec maxH genSpec validatorSet net netCfg nodeSpec
      act `catch` (\Abort -> return ())
  where
    parser :: Parser Opts
    parser = do
      maxH <- optional $ option auto
        (  long    "max-h"
        <> metavar "N"
        <> help    "Maximum height"
        )
      listenPort <- option auto
        (  long    "listen-port"
        <> value thundermintPort
        <> metavar "PORT"
        <> help    ("listening port (default " <> show thundermintPort <> ")")
        )
      prefix <- option str
        (  long    "prefix"
        <> value   ""
        <> metavar "PATH"
        <> help    "prefix for db & logs"
        )
      delay <- option auto
        (  long    "delay"
        <> metavar "N"
        <> help    "delay between transactions in ms"
        )
      doValidate <- switch
        (  long "check-consensus"
        <> help "validate databases"
        )
      netInitialDeposit <- option auto
        (  long "deposit"
        <> help "Initial deposit"
        <> metavar "N.N"
        )
      netInitialKeys <- option auto
        (  long "keys"
        <> help "Initial deposit"
        <> metavar "N"
        )
      nodeNumber <- option auto
        (  long "node-n"
        <> help "Node number"
        <> metavar "N"
        )
      totalNodes <- option auto
        (  long "total-nodes"
        <> help "Node number"
        <> metavar "N"
        )
      optTls <- switch
        (  long "tls"
        <> help "Use TLS for node connection"
        )
      netAddresses <- option json
        (  long "peers"
        <> help "List of initial peers"
        <> metavar "JSON"
        )
      pure Opts{..}
    --
    json = maybeReader $ JSON.decodeStrict . BC8.pack
