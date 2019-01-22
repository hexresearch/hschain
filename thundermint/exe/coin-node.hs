{-# LANGUAGE ApplicativeDo       #-}
{-# LANGUAGE BangPatterns        #-}
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
import Control.Monad.Trans.Reader
import Data.Int
import Data.Monoid ((<>))
import Options.Applicative

import Katip.Core           (showLS)
import Network.Simple.TCP   (accept, listen, recv, closeSock)
import Network.Socket       (SockAddr(..), PortNumber)
import Network.Wai.Middleware.Prometheus
import Prometheus
import Prometheus.Metric.GHC
import System.Environment   (getEnv)
import System.FilePath      ((</>))
import qualified Network.Wai.Handler.Warp as Warp

import Thundermint.Blockchain.Internal.Engine.Types
import Thundermint.Crypto.Ed25519            (Ed25519_SHA512)
import Thundermint.Logger
import Thundermint.Run
import Thundermint.Mock.Coin
import Thundermint.Mock.Types
import Thundermint.P2P (generatePeerId)
import Thundermint.P2P.Consts
import Thundermint.P2P.Instances ()
import Thundermint.P2P.Network               ( getLocalAddress, realNetwork, realNetworkUdp
                                             , getCredentialFromBuffer, realNetworkTls)
import qualified Thundermint.P2P.Types as P2PT
import qualified Control.Exception     as E
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
  , optUDP            :: Bool
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
    nodeSpecStr <- BC8.pack <$> getEnv "THUNDERMINT_NODE_SPEC"
    ipMapPath   <- BC8.pack <$> getEnv "THUNDERMINT_KEYS"
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
      netAddresses <- waitForAddrs
      logger InfoS ("net Addresses: " <> showLS netAddresses) ()
      peerId <- generatePeerId
      let peerInfo = P2PT.PeerInfo peerId (fromIntegral listenPort) 0
      netAPI <- case optTls of
        False -> case optUDP of
          False -> return $ realNetwork peerInfo (show listenPort)
          True  -> liftIO $ realNetworkUdp peerInfo (show listenPort)
        True  -> liftIO $ do
          keyPem  <- BC8.pack <$> getEnv "KEY_PEM"
          certPem <- BC8.pack <$> getEnv "CERT_PEM"
          let credential = getCredentialFromBuffer certPem keyPem
          return $ realNetworkTls credential (show thundermintPort)
      let net = BlockchainNet
                   { bchNetwork          = netAPI
                   , bchLocalAddr        = P2PT.sockAddrToNetAddr nodeAddr
                   , bchInitialPeers     = netAddresses
                   }
          genSpec = restrictGenerator nodeNumber totalNodes
                  $ defaultGenerator netInitialKeys netInitialDeposit delay
      (_,act) <- interpretSpec maxH genSpec validatorSet net nodeSpec
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
      optUDP <- switch
        (  long "udp"
        <> help "use UDP instead of TCP when TLS is not used"
        )
      pure Opts{..}


----------------------------------------------------------------
-- TCP server that listens for boostrap addresses
----------------------------------------------------------------

waitForAddrs :: LoggerT IO [P2PT.NetAddr]
waitForAddrs = LoggerT $ ReaderT $ \(_,logenv) -> E.handle (allExc logenv) $ do
  addrs <- listen "0.0.0.0" "49999" $ \ (lsock, _addr) ->
    accept lsock $ \ (conn, _caddr) -> do
    mMsg <- recv conn 1000000
    closeSock conn
    runLoggerT "boostrap" logenv $ do
      logger InfoS ("accept this: " <> showLS mMsg) ()
    case mMsg of
        Nothing  -> fail "Connection closed by peer."
        Just msg -> either fail return $ JSON.eitherDecodeStrict' msg
  runLoggerT "boostrap" logenv $ do
    logger InfoS ("Got " <> showLS addrs <> " boostrap addresses.") ()
    logger InfoS "Stop listening" ()
  return $ map P2PT.sockAddrToNetAddr addrs
  where
    allExc logenv (e::SomeException) = runLoggerT "boostrap" logenv $ do
      logger ErrorS (showLS e) ()
      E.throw e
