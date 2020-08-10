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
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
-- |
module Main where

import Control.Concurrent (forkIO)
import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Monad.Trans.Cont
import Data.Maybe
import Data.Yaml.Config (loadYamlSettings, requireEnv)
import Options.Applicative
import Katip (LogEnv, Namespace)
import GHC.Generics (Generic)

import HSChain.PoW.Consensus
import HSChain.PoW.Types
import HSChain.PoW.P2P
import HSChain.PoW.P2P.Types
import HSChain.Network.TCP
import HSChain.Logger
import HSChain.Control.Class
import HSChain.Control.Util
import HSChain.Control.Channels
import HSChain.Types.Merkle.Types
import HSChain.Examples.Coin
import HSChain.PoW.Node (blockDatabase,Cfg(..))
import HSChain.Store.Query


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
                       , coinTarget = Target $ 2^(256-10 :: Int)
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
  let sView = inMemoryView (blockID genesis)
  withConnection (fromMaybe "" cfgDB) $ \conn -> 
    withLogEnv "" "" (map makeScribe cfgLog) $ \logEnv -> runCoinT logEnv conn $ evalContT $ do
      db   <- lift $ blockDatabase genesis
      bIdx <- lift $ buildBlockIndex db
      c0   <- lift $ createConsensus db bIdx sView
      pow  <- startNode netcfg net cfgPeers db c0
      void $ liftIO $ forkIO $ do
        ch <- atomicallyIO (chainUpdate pow)
        forever $ do (bh,_) <- awaitIO ch
                     print $ asHeader bh
                     print $ retarget bh
      lift $ miningLoop pow optMine


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

-- | State view which doesn't do any block validation whatsoever
inMemoryView
  :: (Monad m, BlockData b)
  => BlockID b
  -> StateView m b
inMemoryView = make (error "No revinding past genesis")
  where
    make previous bid = view
      where
        view = StateView
          { stateBID    = bid
          , applyBlock  = \bh _ -> return $ Just $ make view (bhBID bh)
          , revertBlock = return previous
          , flushState  = return view
          , checkTx                  = error "Transaction checking is not supported"
          , createCandidateBlockData = error "Block creation is not supported"
          }
