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
import Control.Concurrent
import Control.Monad
import Control.Monad.Catch
import Control.Monad.IO.Class
import Data.Int
import Data.Monoid ((<>))
import Options.Applicative

import Katip.Core           (showLS)
import Network.Socket       (PortNumber)
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
import Thundermint.P2P (generatePeerId)
import Thundermint.P2P.Consts
import Thundermint.P2P.Network               ( getLocalAddress, realNetwork, realNetworkUdp
                                             , getCredentialFromBuffer, realNetworkTls)
import qualified Thundermint.P2P.Types as P2PT
import qualified Data.Aeson            as JSON
import qualified Data.ByteString.Char8 as BC8


----------------------------------------------------------------
-- Cmd line specification
----------------------------------------------------------------

-- Please keep these fields lazy - we feel them incrementally because
-- @Parser@ lacks @Monad@ interface.
data Opts = Opts
  { maxH                  :: Maybe Int64
  , listenPort            :: PortNumber
  , prefix                :: FilePath
  , delay                 :: Int
  , doValidate            :: Bool
  , optsNetInitialDeposit :: Integer
  , optsNetInitialKeys    :: Int
  , nodeNumber            :: Int
  , totalNodes            :: Int
  , optTls                :: Bool
  , optUDP                :: Bool
  , netAddresses          :: [P2PT.NetAddr]
  }

emptyOpts :: Opts
emptyOpts = Opts
  { maxH                  = error "no maximal height"
  , listenPort            = error "no listen port"
  , prefix                = error "no prefix"
  , delay                 = error "no delay"
  , doValidate            = error "no do-validate"
  , optsNetInitialDeposit = error "no initial deposit"
  , optsNetInitialKeys    = error "no initial keys"
  , nodeNumber            = error "no node number"
  , totalNodes            = error "no total nodes"
  , optTls                = error "no TLS-enable flag"
  , optUDP                = error "no UDP enable flag"
  , netAddresses          = error "no addresses specified"
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
    Opts{..} <- fmap ($ emptyOpts) $ customExecParser (prefs showHelpOnError)
              $ info (helper <*> parser)
                (  fullDesc
                <> header   "Coin test program"
                <> progDesc ""
                )
    nodeSpecStr <- BC8.pack <$> getEnv    "THUNDERMINT_NODE_SPEC"
    netParamStr <- lookupEnv "THUNDERMINT_NET_PARAM"
    ipMapPath   <- BC8.pack <$> getEnv    "THUNDERMINT_KEYS"
    let netCfg = case netParamStr of
          Nothing -> change (defCfg :: Configuration Example) :: Configuration Example
            where
              change cfg = cfg { cfgNetwork = (cfgNetwork cfg) { pexMinKnownConnections = totalNodes - 1} }
          Just s  -> either error id $ JSON.eitherDecodeStrict' $ BC8.pack s
    let nodeSpec@NodeSpec{..} = either error id $ JSON.eitherDecodeStrict' nodeSpecStr
        loggers = [ makeScribe s { scribe'path = fmap (prefix </>) (scribe'path s) }
                  | s <- nspecLogFile
                  ]
    let port = 1000 + fromIntegral listenPort
    startWebMonitoring port
    withLogEnv "TM" "DEV" loggers $ \logenv -> runLoggerT logenv $ do
      let !validatorSet = makeValidatorSetFromPriv
                        $ (id @[PrivValidator Ed25519_SHA512])
                        $ either error (fmap PrivValidator)
                        $ JSON.eitherDecodeStrict' ipMapPath
      logger InfoS "Listening for bootstrap adresses" ()
      logger InfoS ("net Addresses: " <> showLS netAddresses) ()
      nodeAddr     <- liftIO getLocalAddress
      logger InfoS ("local Address: " <> showLS nodeAddr) ()
      peerId <- generatePeerId
      logger InfoS ("peer Id generated: " <> showLS peerId) ()
      let peerInfo = P2PT.PeerInfo peerId (fromIntegral listenPort) 0
      logger InfoS ("peer Info generated: " <> showLS peerInfo) ()
      netAPI <- case optTls of
        False -> case optUDP of
          False -> return $ realNetwork peerInfo
          True  -> liftIO $ realNetworkUdp peerInfo
        True  -> liftIO $ do
          keyPem  <- BC8.pack <$> getEnv "KEY_PEM"
          certPem <- BC8.pack <$> getEnv "CERT_PEM"
          let credential = getCredentialFromBuffer certPem keyPem
          return $ realNetworkTls credential (show thundermintPort)
      logger InfoS ("network API created") ()
      let net = BlockchainNet
                   { bchNetwork          = netAPI
                   , bchLocalAddr        = P2PT.sockAddrToNetAddr nodeAddr
                   , bchInitialPeers     = netAddresses
                   }
          genSpec = restrictGenerator nodeNumber totalNodes
                  $ defaultGenerator optsNetInitialKeys optsNetInitialDeposit delay
      catch (do
        (_,act) <- interpretSpec maxH genSpec validatorSet net netCfg nodeSpec
        logger InfoS ("spec has been interpreted") ()
        act `catch` (\e -> logger InfoS ("Exiting due to "<> (showLS (e :: SomeException))) ())
        ) (\e -> logger InfoS ("Exiting interpretSpec sequence due to "<> (showLS (e :: SomeException))) ())
      logger InfoS "Normal exit" ()
  where
    parser = foldr (\pf ps -> (.) <$> pf <*> ps) (pure id) parsersList
    parsersList :: [Parser (Opts -> Opts)]
    parsersList =
      [ (\x opts -> opts { maxH = x}) <$> (optional $ option auto
          (  long    "max-h"
          <> metavar "N"
          <> help    "Maximum height"
          ))
      , (\x opts -> opts { listenPort = x}) <$> (option auto
          (  long    "listen-port"
          <> value thundermintPort
          <> metavar "PORT"
          <> help    ("listening port (default " <> show thundermintPort <> ")")
          ))
      , (\x opts -> opts { prefix = x}) <$> (option str
          (  long    "prefix"
          <> value   ""
          <> metavar "PATH"
          <> help    "prefix for db & logs"
          ))
      , (\x opts -> opts { delay = x}) <$> (option auto
          (  long    "delay"
          <> metavar "N"
          <> help    "delay between transactions in ms"
          ))
      , (\x opts -> opts { doValidate = x}) <$> (switch
          (  long "check-consensus"
          <> help "validate databases"
          ))
      , (\x opts -> opts { optsNetInitialDeposit = x}) <$> (option auto
          (  long "deposit"
          <> help "Initial deposit"
          <> metavar "N.N"
          ))
      , (\x opts -> opts { optsNetInitialKeys = x}) <$> (option auto
          (  long "keys"
          <> help "Initial deposit"
          <> metavar "N"
          ))
      , (\x opts -> opts { nodeNumber = x}) <$> (option auto
          (  long "node-n"
          <> help "Node number"
          <> metavar "N"
          ))
      , (\x opts -> opts { totalNodes = x}) <$> (option auto
          (  long "total-nodes"
          <> help "Node number"
          <> metavar "N"
          ))
      , (\x opts -> opts { optTls = x}) <$> (switch
          (  long "tls"
          <> help "Use TLS for node connection"
          ))
      , (\x opts -> opts { optUDP = x}) <$> (switch
          (  long "udp"
          <> help "use UDP instead of TCP when TLS is not used"
          ))
      , (\x opts -> opts { netAddresses =x }) <$> (option json
          (  long "peers"
          <> help "List of initial peers"
          <> metavar "JSON"
          ))
      ]
    --
    json = maybeReader $ \s -> do
            addrStrings <- JSON.decodeStrict $ BC8.pack s
            sequence $ map readNA addrStrings
        where
          readNA addrStr = case reads addrStr of
            ((addr, ""):_) -> Just addr
            _ -> Nothing

