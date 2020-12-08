{-# LANGUAGE ApplicativeDo              #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE NumDecimals                #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
-- |
module Main where

import Control.Concurrent (forkIO, threadDelay)
import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Monad.Trans.Cont
import Control.Lens
import Data.Maybe
import Data.Word
import qualified Data.Aeson      as JSON
import qualified Data.Map.Strict as Map
import qualified Data.Vector     as V
import Data.Yaml.Config (loadYamlSettings, requireEnv)
import Options.Applicative
import GHC.Generics (Generic)

import HSChain.Control.Channels
import HSChain.Control.Util
import HSChain.Crypto
import HSChain.Examples.Coin
import HSChain.Logger
import HSChain.Network.TCP
import HSChain.Network.Types
import HSChain.PoW.Consensus
import HSChain.PoW.P2P
import HSChain.PoW.P2P.Types
import HSChain.PoW.Types
import HSChain.Store.Query
import HSChain.Types.Merkle.Types
import HSChain.Config

----------------------------------------------------------------
--
----------------------------------------------------------------

-- |Node's configuration.
data Cfg = Cfg
  { cfgPort    :: Word16
  , cfgPeers   :: [NetAddr]
  , cfgLog     :: [ScribeSpec]
  , cfgDB      :: Maybe FilePath
  }
  deriving stock (Show, Generic)
  deriving (JSON.FromJSON) via SnakeCase (DropSmart (Config Cfg))

genesis :: Block Coin
genesis = Block
  { blockHeight = Height 0
  , blockTime   = Time 0
  , prevBlock   = Nothing
  , blockData   = Coin { coinData   = merkled []
                       , coinNonce  = 0
                       , coinTarget = Target $ 2^(256-16 :: Int)
                       }
  }

main :: IO ()
main = do
  -- Parse configuration
  Opts{..} <- customExecParser (prefs showHelpOnError)
            $ info (helper <*> parser)
              (  fullDesc
              <> header   "PoW node settings"
              <> progDesc ""
              )
  Cfg{..} <- loadYamlSettings optConfigPath [] requireEnv
  -- Acquire resources
  let net    = newNetworkTcp cfgPort
      netcfg = NodeCfg { nKnownPeers     = 3
                       , nConnectedPeers = 3
                       , initialPeers    = cfgPeers
                       }
  withConnection (fromMaybe "" cfgDB) $ \conn -> 
    withLogEnv "" "" (map makeScribe cfgLog) $ \logEnv -> runCoinT logEnv conn $ evalContT $ do
      lift $ initCoinDB
      lift $ storeCoinBlock genesis
      bIdx <- lift $ buildBlockIndex coinDB
      liftIO $ print $ Map.keys $ _blockIDMap bIdx
      liftIO $ print $ map bhBID $ blockIndexHeads bIdx
      let c0 = createLightConsensus genesis bIdx
      pow <- lightNode netcfg net coinDB c0
      -- report progress
      void $ liftIO $ forkIO $ do
        ch <- atomicallyIO (bestHeadUpdates pow)
        forever $ do bh <- view (bestLightHead . _1) <$> awaitIO ch
                     print (bhHeight bh, bhBID bh)
                     print $ retarget bh
      -- Wait forever
      liftIO $ forever $ threadDelay maxBound

txGeneratorLoop
  :: (MonadReadDB m, MonadIO m)
  => PoW (CoinState m) -> [PrivKey Alg] -> m ()
txGeneratorLoop pow keyList = do
  forever $ do
    mtx <- generateTX keyVec keyMap
    forM_ mtx $ sinkIO (postTransaction (mempoolAPI pow))
    liftIO $ threadDelay 25e3
  where
    keyVec = V.fromList $ Map.keys keyMap
    keyMap = Map.fromList [ (publicKey k, k) | k <- keyList ]


----------------------------------------------------------------
--
----------------------------------------------------------------

data Opts = Opts
  { optConfigPath :: [FilePath]     -- ^ Paths to configuration
  }

parser :: Parser Opts
parser = do
  optConfigPath <- some $ strArgument
    (  metavar "PATH"
    <> help  "Path to configuration"
    <> showDefault
    )
  return Opts{..}
