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
import Control.Lens (Lens',lens,(%~))
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

import Katip (Namespace,LogEnv,logF,Katip(..))
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

import HSChain.Config (SnakeCase(..),Config(..),DropSmart(..),TopConfig(..))
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

data AppDict = AppDict
  { dictGauges    :: !PrometheusGauges
  , dictNamespace :: !Namespace
  , dictLogEnv    :: !LogEnv
  , dictConn      :: !(Connection 'RW BData)
  }

ldictNamespace :: Lens' AppDict Namespace
ldictNamespace = lens dictNamespace (\d n -> d { dictNamespace = n })

ldictLogEnv :: Lens' AppDict LogEnv
ldictLogEnv = lens dictLogEnv (\d n -> d { dictLogEnv = n })


newtype AppT m a = AppT { unAppT :: ReaderT AppDict m a }
  deriving newtype (Functor,Applicative,Monad,MonadIO)
  deriving newtype (MonadMask,MonadThrow,MonadCatch,MonadFork)

runAppT :: LogEnv -> PrometheusGauges -> Connection 'RW BData -> AppT m a -> m a
runAppT lenv g conn
  = flip runReaderT AppDict { dictGauges    = g
                            , dictNamespace = mempty
                            , dictLogEnv    = lenv
                            , dictConn      = conn
                            }
  . unAppT


-- Tricky one.
--  - Do we need Reader (Namespace,LogEnv)???
--  - Do we need two readers???
instance MonadIO m => Katip (AppT m) where
  getLogEnv     = AppT $ asks dictLogEnv
  localLogEnv f = AppT . local (ldictLogEnv %~ f) . unAppT

instance MonadIO m => MonadLogger (AppT m) where
  logger sev s a = do
    nm <- AppT $ asks dictNamespace
    logF a nm sev s
  localNamespace f (AppT m) = AppT $ local (ldictNamespace %~ f) m

-- Derived from single Reader
instance MonadIO m => MonadTMMonitoring (AppT m) where
  usingCounter getter n = AppT $ flip addCounterNow  n =<< asks (getter . dictGauges)
  usingGauge   getter n = AppT $ flip setTGaugeNow   n =<< asks (getter . dictGauges)
  usingVector  getter l = AppT $ flip incTCVectorNow l =<< asks (getter . dictGauges)


-- Derived using Reader (Connection 'RW)
instance Monad m => MonadReadDB (AppT m) BData where
  askConnectionRO = AppT $ connectionRO <$> asks dictConn
  
instance Monad m => MonadDB (AppT m) BData where
  askConnectionRW = AppT $ asks dictConn






----------------------------------------------------------------
--
----------------------------------------------------------------

data Opts = Opts
  { cmdConfigPath :: [FilePath]
  , optMaxH       :: Maybe Height
  }

data NodeConfig = NodeConfig
  { nodeSpec          :: NodeSpec BData
  , nodeCoin          :: CoinSpecification
  , nodeDelays        :: Configuration Example
  , nodeValidatorKeys :: [PublicKey (Alg BData)]
  , nodeMaxH          :: Maybe Height
  , nodePort          :: Word16
  , nodeSeeds         :: [NetAddr]
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
    let (mtxGen, genesis) = mintMockCoin [ Validator v 1 | v <- nodeValidatorKeys] nodeCoin
    -- Create network
    let bnet     = BlockchainNet { bchNetwork      = newNetworkTcp nodePort
                                 , bchInitialPeers = nodeSeeds
                                 }
    --- Allocate resources
    (conn, logenv) <- allocNode nodeSpec
    gauges         <- standardMonitoring
    let run = runAppT logenv gauges conn
    -- Actually run node
    lift $ run $ do
      (RunningNode{..},acts) <- interpretSpec
        genesis
        nodeDelays
        bnet
        nodeSpec
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
