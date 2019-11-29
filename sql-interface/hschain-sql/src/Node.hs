{-# LANGUAGE ApplicativeDo, BangPatterns, DataKinds
           , DeriveGeneric, DerivingStrategies
           , DeriveAnyClass, GeneralizedNewtypeDeriving
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
import Data.Word

import System.Environment (getArgs)
import System.Exit (exitSuccess, exitFailure)

import Language.SQL.SimpleSQL.Dialect
import Language.SQL.SimpleSQL.Parse
import Language.SQL.SimpleSQL.Pretty
import Language.SQL.SimpleSQL.Syntax

import Data.Yaml.Config       (loadYamlSettings, requireEnv)

import Network.Wai.Middleware.Prometheus
import Prometheus   (register)
import Prometheus.Metric.GHC
import GHC.Generics (Generic)
import qualified Network.Wai.Handler.Warp as Warp

import HSChain.Blockchain.Internal.Engine.Types
import HSChain.Logger
--import HSChain.Mock.Types
import HSChain.Run

import qualified Data.Aeson as JSON
import Data.ByteString.Lazy (toStrict)
import Data.Foldable
import Data.Maybe
import Data.Map             (Map,(!))
import qualified Data.Vector         as V
import qualified Data.Map.Strict     as Map
import qualified Data.HashMap.Strict as HM
import qualified Data.Set            as Set

import Control.DeepSeq
import Codec.Serialise      (Serialise,serialise)

import HSChain.Control
import HSChain.Store
import HSChain.Monitoring
import HSChain.Crypto         (PublicKey, (:&))
import HSChain.Crypto.Ed25519
import HSChain.Crypto.SHA
import HSChain.P2P            (generatePeerId)
import HSChain.P2P.Network    (newNetworkTcp)
import HSChain.Types
import HSChain.Debug.Trace

import qualified HSChain.P2P.Types as P2PT

import Network.Socket

import System.IO

-- |Read config, get secret key, run node...
runConsensusNode :: String -> String -> IO ()
runConsensusNode configPath envVar = do
  
  return ()

-- |Main program.
main :: IO ()
main = do
  args <- getArgs
  case args of
    [filePath, envVarName] -> runConsensusNode filePath envVarName
    _ -> do
      putStrLn "usage: hschain-sql-node path-to-config private-key-env-var-name"
      exitFailure

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
  { transactionRequest    :: String
  , transactionArguments  :: Map.Map String String
  }
  deriving stock    (Show, Eq, Generic)
  deriving anyclass (NFData)
  deriving anyclass (Serialise)

data Update = Update
  { updateRequest    :: String
  }
  deriving stock    (Show, Eq, Generic)
  deriving anyclass (NFData)
  deriving anyclass (Serialise)

data SQLiteState = SQLiteState
  deriving stock    (Show, Eq, Generic)
  deriving anyclass (NFData)
  deriving anyclass (Serialise)


----------------------------------------------------------------
-- ODBC extension API interface
----------------------------------------------------------------

startODBCExtensionInterface :: Int -> String -> IO ()
startODBCExtensionInterface port dbname = do
  db <- SQLite.open dbname
  sock <- socket AF_INET6 Stream (fromIntegral port)
  listen sock 1
  forkIO $ odbcExtensionInterface db sock
  return ()

odbcExtensionInterface :: SQLite.Connection -> Socket -> IO ()
odbcExtensionInterface dbConn sock = do
  flip finally (return ()) $ do
    (connSock, _) <- accept sock
    h <- socketToHandle connSock ReadWriteMode
    flip finally (hClose h) $ do
      pubKeyStr <- hGetLine h
      otherHeight <- hGetLine h
      request <- hGetLine h
      params <- if null request then return [] else readParams h []
      reportAnswer dbConn h otherHeight
      return ()
    return ()
  odbcExtensionInterface dbConn sock
  where
    readParams h acc = do
      paramName <- hGetLine h
      if null paramName
        then return acc
        else do
          value <- hGetLine h
          readParams h ((paramName, value) : acc)

reportAnswer :: SQLite.Connection -> Handle -> String -> IO ()
reportAnswer conn h heightStr
  | ((height, ""):_) <- reads heightStr = do
    currentHeight <- readCurrentHeight
    hPutStrLn h $ show currentHeight
    if height < currentHeight
      then do
        putStrLn "reporting difference!"
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
    reportDifference currentHeight conn h = do
      when (currentHeight < 0) $ do
        -- reporting genesis.
        return ()
      -- reporting transactions happened after that height above.
      let requestsList = unwords
                   [ "SELECT sr.height, sr.seq_index, sr.request_id, ar.request_sql"
                   , "FROM serialized_requests AS sr, allowed_requests AS ar"
                   , "WHERE sr.request_id = ar.request_id AND sr.height > :height"
                   , "ORDER BY sr.height, sr.seq_index;"
                   ]
      SQLite.foldNamed requestsList [":height" := currentHeight] () $ \(h, s, id, sql) -> do
        hPutStrLn h sql
        let paramsList = unwords
                       [ "SELECT srp.name, srp.value"
                       , "FROM serialized_requests_params AS srp"
                       , "WHERE     srp.height = :height"
                       , "      AND srp.seq_index = :index"
                       , "      AND srp.request_id = :id"
                       , "ORDER BY srp.name;"
                       ]
            paramsListParams =
                       [ ":height" := (h :: Int)
                       , ":index"  := (s :: Int)
                       , ":id"     := (id :: String)
                       ]
        ps <- SQLite.queryNamed paramsList paramsListParams
        forM_ ps $ \(name, value) -> do
          hPutStrLn h name
          hPutStrLn h value
        hPutStrLn h ""
      return ()
