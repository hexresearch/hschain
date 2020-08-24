{-# LANGUAGE ApplicativeDo              #-}
{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE NumDecimals                #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
-- |
module Main where

import Control.Concurrent (forkIO, threadDelay)
import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Monad.Trans.Cont
import Data.Maybe
import Data.List (unfoldr)
import qualified Data.ByteString as BS
import qualified Data.Map.Strict as Map
import qualified Data.Vector     as V
import System.Random (randoms,mkStdGen)
import Data.Yaml.Config (loadYamlSettings, requireEnv)
import Options.Applicative

import HSChain.Control.Channels
import HSChain.Control.Util
import HSChain.Control.Class
import HSChain.Crypto
import HSChain.Examples.Coin
import HSChain.Logger
import HSChain.Network.TCP
import HSChain.PoW.Consensus
import HSChain.PoW.Node (Cfg(..))
import HSChain.PoW.P2P
import HSChain.PoW.P2P.Types
import HSChain.PoW.Types
import HSChain.PoW.Node (genericMiningLoop)
import HSChain.Store.Query
import HSChain.Types.Merkle.Types


----------------------------------------------------------------
--
----------------------------------------------------------------

genesis :: Block Coin
genesis = GBlock
  { blockHeight = Height 0
  , blockTime   = Time 0
  , prevBlock   = Nothing
  , blockData   = Coin { coinData   = merkled []
                       , coinNonce  = 0
                       , coinTarget = Target $ 2^(256-13 :: Int)
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
  Cfg{..} <- loadYamlSettings cmdConfigPath [] requireEnv
  -- Acquire resources
  let net    = newNetworkTcp cfgPort
      netcfg = NetCfg { nKnownPeers     = 3
                      , nConnectedPeers = 3
                      }
  withConnection (fromMaybe "" cfgDB) $ \conn -> 
    withLogEnv "" "" (map makeScribe cfgLog) $ \logEnv -> runCoinT logEnv conn $ evalContT $ do
      (db, bIdx, sView) <- lift $ coinStateView cfgPriv genesis
      c0  <- lift $ createConsensus db sView bIdx
      pow <- startNode netcfg net cfgPeers db c0
      -- report progress
      void $ liftIO $ forkIO $ do
        ch <- atomicallyIO (chainUpdate pow)
        forever $ do (bh,_) <- awaitIO ch
                     print (bhHeight bh, bhBID bh)
                     print $ retarget bh
      -- Mining and TX generation
      case optMine of
        True  -> do
          cforkLinked $ txGeneratorLoop pow (cfgPriv : take 100 (makePrivKeyStream 1433))
          lift $ genericMiningLoop pow
        False -> liftIO $ forever $ threadDelay maxBound

txGeneratorLoop
  :: (MonadReadDB m, MonadIO m)
  => PoW m Coin -> [PrivKey Alg] -> m ()
txGeneratorLoop pow keyList = do
  forever $ do
    mtx <- generateTX keyVec keyMap
    forM_ mtx $ sinkIO (postTransaction (mempoolAPI pow))
    liftIO $ threadDelay 50e3
  where
    keyVec = V.fromList $ Map.keys keyMap
    keyMap = Map.fromList [ (publicKey k, k) | k <- keyList ]


----------------------------------------------------------------
--
----------------------------------------------------------------

data Opts = Opts
  { cmdConfigPath :: [FilePath] -- ^ Path to configuration
  , optMine       :: Bool       -- ^ Whether to mine blocks
  }

parser :: Parser Opts
parser = do
  cmdConfigPath <- some $ strArgument
    (  metavar "PATH"
    <> help  "Path to configuration"
    <> showDefault
    )
  optMine <- switch
    (  long "mine"
    <> help "Mine blocks"
    )
  return Opts{..}

makePrivKeyStream :: forall alg. CryptoSign alg => Int -> [PrivKey alg]
makePrivKeyStream seed
  = unfoldr step
  $ randoms (mkStdGen seed)
  where
    -- Size of key
    keySize = privKeySize (Proxy @alg)
    -- Generate single key
    step stream = Just (k, stream')
      where
        Just k    = decodeFromBS $ BS.pack bs
        (bs, stream') = splitAt keySize stream
