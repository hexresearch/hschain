{-# LANGUAGE ApplicativeDo, BangPatterns, DataKinds
           , DeriveGeneric, DerivingStrategies
           , DeriveAnyClass, FlexibleContexts
           , GeneralizedNewtypeDeriving
           , LambdaCase, OverloadedStrings
           , RecordWildCards, ScopedTypeVariables
           , StandaloneDeriving, TypeApplications
           , TypeFamilies, TypeOperators
#-}

module Main where

import Control.Concurrent
import Control.Monad
import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.State
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Cont

import qualified Database.SQLite.Simple as SQLite

import Data.Aeson             (FromJSON)
import Data.Monoid            ((<>))
import Data.String
import qualified Data.Text as Text
import Data.Word

import System.Environment (getArgs, getEnv)
import System.Exit (exitSuccess, exitFailure)
import System.FilePath

import Data.Yaml.Config       (loadYamlSettings, requireEnv)

import Network.Wai.Middleware.Prometheus
import Prometheus   (register, runMonitorT)
import Prometheus.Metric.GHC
import GHC.Generics (Generic)
import qualified Network.Wai.Handler.Warp as Warp

import HSChain.Blockchain.Internal.Engine.Types
import HSChain.Logger
import HSChain.Run

import qualified Data.Aeson as JSON
import Data.ByteString.Lazy (toStrict)
import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as UTF8
import Data.Foldable
import Data.Maybe
import Data.Map             (Map,(!))
import qualified Data.Vector         as V
import qualified Data.Map.Strict     as Map
import qualified Data.HashMap.Strict as HM
import qualified Data.Set            as Set

import Control.DeepSeq
import Codec.Serialise      (Serialise,serialise)

import HSChain.Blockchain.Interpretation
import HSChain.Control
import HSChain.Store
import HSChain.Monitoring
import HSChain.Crypto
import HSChain.Crypto.Ed25519
import HSChain.Crypto.SHA
import HSChain.P2P            (generatePeerId)
import HSChain.P2P.Network    (newNetworkTcp)
import HSChain.Types
import HSChain.Debug.Trace
import HSChain.Mock.Types     (Example)

import qualified HSChain.P2P.Types as P2PT

import Network.Socket

import System.Directory
import System.IO

import Katip (LogEnv)

import HSChain.SQL

-- |Read config, get secret key, run node...
runConsensusNode :: String -> String -> String -> IO ()
runConsensusNode genesisPath configPath envVar = do
  nspec@NodeSpec{} :*: NodeCfg{..} :*: (cfg :: Configuration Example)
    <- loadYamlSettings [reverse configPath] [] requireEnv
  genesis <- readGenesisBlock genesisPath
  ourPrivateKey <- do
    str <- getEnv envVar
    case decodeBase58 $ Text.pack str of
      Just k -> return k
      Nothing -> error $ "Environment variable "++show envVar++" does not contain base58-decodable value of validator's private key"
  startWebMonitoring $ fromIntegral nodeCfgPort + 1000
  -- Start node
  evalContT $ do
    -- Create network
    peerId <- generatePeerId
    let peerInfo = P2PT.PeerInfo peerId nodeCfgPort 0
        bnet     = BlockchainNet { bchNetwork      = newNetworkTcp peerInfo
                                 , bchInitialPeers = nodeCfgSeeds
                                 }
    --- Allocate resources
    (conn, logenv) <- allocNode nspec
    gauges         <- standardMonitoring
    let run = const $ return ()--runMonitorT gauges . runLoggerT logenv . runDBT conn
    -- Actually run node
    lift $ run $ do
      (RunningNode{..},acts) <- interpretSpec ourPrivateKey genesis
        (nspec :*: cfg :*: bnet)
        (mempty)
      cursor <- getMempoolCursor rnodeMempool
      let txGen = do
            transactionGenerator txG
                    rnodeMempool
                    (snd <$> bchCurrentState rnodeState)
                    (void . pushTransaction cursor)
      logOnException $ runConcurrently $ txGen : acts
  return ()

-- |Main program.
main :: IO ()
main = do
  args <- getArgs
  case args of
    [genesisPath, configPath, envVarName] -> runConsensusNode genesisPath configPath envVarName
    _ -> do
      putStrLn "usage: hschain-sql-node path-to-config private-key-env-var-name"
      exitFailure

----------------------------------------------------------------
-- Monitoring and other goodies
----------------------------------------------------------------

startWebMonitoring :: Warp.Port -> IO ()
startWebMonitoring port = do
    void $ register ghcMetrics
    void $ forkIO
         $ Warp.run port
         $ prometheus def
                      { prometheusInstrumentPrometheus = False }
                      metricsApp

-- | Allocate resources for node
allocNode
  :: ( MonadIO m, MonadMask m
     , Crypto alg, Serialise a, Eq a, Show a, Has x NodeSpec)
  => x                          -- ^ Node parameters
  -> ContT r m (Connection 'RW alg a, LogEnv)
allocNode x = do
  liftIO $ createDirectoryIfMissing True $ takeDirectory dbname
  conn   <- ContT $ withDatabase dbname
  logenv <- ContT $ withLogEnv "TM" "DEV" [ makeScribe s | s <- x ^.. nodeSpecLogFile ]
  return (conn,logenv)
  where
    dbname = fromMaybe "" $ x ^.. nodeSpecDBName


----------------------------------------------------------------
-- Basic logic
----------------------------------------------------------------

type Alg = (Ed25519 :& SHA512)

data BData = BData
  { bdataTransactions          :: [Transaction]
  , bdataPostProcessingUpdates :: [Update]
  }
  deriving stock    (Show, Eq, Generic)
  deriving anyclass (NFData)
  deriving anyclass (Serialise)

instance BlockData BData where
  type TX               BData = Transaction
  type InterpreterState BData = SQLiteState
  blockTransactions (BData txs _) = txs
  logBlockData      (BData txs upds) = HM.singleton "Ntx" $ JSON.toJSON $ length txs + length upds

data Transaction = Transaction
  { transactionRequest    :: BS.ByteString
  , transactionArguments  :: Map.Map BS.ByteString BS.ByteString
  }
  deriving stock    (Show, Eq, Generic)
  deriving anyclass (NFData)
  deriving anyclass (Serialise)

data Update = Update
  { updateRequest    :: BS.ByteString
  }
  deriving stock    (Show, Eq, Generic)
  deriving anyclass (NFData)
  deriving anyclass (Serialise)

data SQLiteState = SQLiteState
  deriving stock    (Show, Eq, Generic)
  deriving anyclass (NFData)
  deriving anyclass (Serialise)

data NodeSpec = NodeSpec
  { nodeSpecDBName     :: Maybe String
  , nodeSpecLogFile    :: [ScribeSpec]
  }
  deriving (Show, Generic)

instance JSON.ToJSON NodeSpec
instance JSON.FromJSON NodeSpec

data NodeCfg = NodeCfg
  { nodeCfgValidatorKeys :: [PublicKey Alg]
  , nodeCfgPort          :: Word16
  , nodeCfgSeeds         :: [P2PT.NetAddr]
  , nodeCfgGenesisPath   :: String
  , nodeCfgAPIPort       :: Word16
  }
  deriving (Show,Generic)
instance FromJSON NodeCfg

data RunningNode m alg a = RunningNode
  { rnodeState   :: BChStore m a
  , rnodeConn    :: Connection 'RO alg a
  , rnodeMempool :: Mempool m alg (TX a)
  }

interpretSpec
  :: ( MonadDB m Alg BData, MonadFork m, MonadMask m, MonadLogger m
     , MonadTrace m, MonadTMMonitoring m
     , Has x BlockchainNet
     , Has x (Configuration Example))
  => PrivValidator Alg
  -> Block Alg BData
  -> x
  -> AppCallbacks m Alg BData
  -> m (RunningNode m Alg BData, [m ()])
interpretSpec privateKey genesis p cb = do
  conn  <- askConnectionRO
  transitions <- createTransitions
  store <- newSTMBchStorage $ initialState transitions
  logic <- makeAppLogic store transitions runner
  acts <- runNode (getT p :: Configuration Example) NodeDescription
    { nodeValidationKey = Just privateKey
    , nodeGenesis       = genesis
    , nodeCallbacks     = cb <> nonemptyMempoolCallback (appMempool logic)
    , nodeLogic         = logic
    , nodeNetwork       = getT p
    }
  return
    ( RunningNode { rnodeState   = store
                  , rnodeConn    = conn
                  , rnodeMempool = appMempool logic
                  }
    , acts
    )

createTransitions = return $ BChLogic
  { processTx     = error "processTX is called! I am not sure we should use it."
  , processBlock  = error "process block"
  , generateBlock = error "generate block"
  , initialState  = SQLiteState
  }


----------------------------------------------------------------
-- Genesis block.
--
----------------------------------------------------------------

readGenesisBlock :: String -> IO BData
readGenesisBlock filename = do
  text <- BS.readFile filename
  let ls = BS.split (fromIntegral $ fromEnum '\n') text
  case ls of
    (publickey : signature : salt : updates) -> do
      if checkTextualSignature publickey signature salt updates
        then return $ BData [] $ map Update updates
        else error $ "failed to verify signature on genesis file "++show filename++"."
    _ -> error $ "malformed genesis file "++ show filename ++ " (must have at least three."


----------------------------------------------------------------
-- ODBC extension API interface
----------------------------------------------------------------

startODBCExtensionInterface :: Mempool m Alg Transaction -> Int -> String -> IO ()
startODBCExtensionInterface mempool port dbname = do
  db <- SQLite.open dbname
  sock <- socket AF_INET6 Stream (fromIntegral port)
  listen sock 1
  forkIO $ odbcExtensionInterface mempool db sock
  return ()

odbcExtensionInterface :: Mempool m Alg Transaction -> SQLite.Connection -> Socket -> IO ()
odbcExtensionInterface mempool dbConn sock = do
  flip finally (return ()) $ do
    (connSock, _) <- accept sock
    h <- socketToHandle connSock ReadWriteMode
    flip finally (hClose h) $ do
      pubKeyStr <- BS.hGetLine h
      otherHeight <- BS.hGetLine h
      salt <- BS.hGetLine h
      (request, params) <- if BS.null salt
                   then return (BS.empty, [])
                   else do
                          rq <- BS.hGetLine h
                          ps <- readParams h []
                          return (rq, ps)
      reportAnswer dbConn h otherHeight
      return ()
    return ()
  odbcExtensionInterface mempool dbConn sock
  where
    readParams h acc = do
      paramName <- BS.hGetLine h
      if BS.null paramName
        then return acc
        else do
          value <- BS.hGetLine h
          readParams h ((paramName, value) : acc)

reportAnswer :: SQLite.Connection -> Handle -> BS.ByteString -> IO ()
reportAnswer conn h heightStr
  | ((height, ""):_) <- reads (UTF8.toString heightStr) = do
    currentHeight <- readCurrentHeight
    hPutStrLn h $ show currentHeight
    if height < currentHeight
      then do
        reportDifference height
      else
        return ()
    hPutStrLn h ""
    return ()
  | otherwise = do
    return ()
  where
    readCurrentHeight = do
      r <- SQLite.query conn "SELECT height FROM height;" ()
      case r of
        [SQLite.Only h] -> return (h :: Int)
        _ -> return (-1000)
    reportDifference currentHeight = do
      when (currentHeight < 0) $ do
        -- reporting genesis.
        SQLite.fold conn "SELECT request_sql FROM serialized_genesis_requests ORDER BY seq_index;" () () $ \() (SQLite.Only sql) -> do
          hPutStrLn h sql
          hPutStrLn h "" -- no parameters.
        return ()
      -- reporting transactions happened after that height above.
      let requestsList = fromString $ unwords
                   [ "SELECT sr.height, sr.seq_index, sr.request_id, ar.request_sql"
                   , "FROM serialized_requests AS sr, allowed_requests AS ar"
                   , "WHERE sr.request_id = ar.request_id AND sr.height > :height"
                   , "ORDER BY sr.height, sr.seq_index;"
                   ]
      SQLite.foldNamed conn requestsList [":height" SQLite.:= currentHeight] () $ \() (rh, s, id, sql) -> do
        hPutStrLn h sql
        let paramsList = fromString $ unwords
                       [ "SELECT srp.name, srp.value"
                       , "FROM serialized_requests_params AS srp"
                       , "WHERE     srp.height = :height"
                       , "      AND srp.seq_index = :index"
                       , "      AND srp.request_id = :id"
                       , "ORDER BY srp.name;"
                       ]
            paramsListParams =
                       [ ":height" SQLite.:= (rh :: Int)
                       , ":index"  SQLite.:= (s :: Int)
                       , ":id"     SQLite.:= (id :: String)
                       ]
        ps <- SQLite.queryNamed conn paramsList paramsListParams
        forM_ ps $ \(name, value) -> do
          hPutStrLn h name
          hPutStrLn h value
        hPutStrLn h "" -- empty line as parameter name for a delimiter
      return ()


