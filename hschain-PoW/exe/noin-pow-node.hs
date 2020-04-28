-- |noin-pow-node.hs
--
-- An Effervescent POW node tracking UTXO with noins.
--
-- Copyright (C) 2020 ...
{-# LANGUAGE ApplicativeDo      #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeApplications   #-}
module Main where

import qualified Data.Aeson as JSON
import Control.Concurrent
import Control.Monad
import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Cont
import Data.IORef
import Data.Word
import Data.Yaml.Config       (loadYamlSettings, requireEnv)
import qualified Data.Map.Strict as Map
import Lens.Micro
import Options.Applicative
import System.Random (randomRIO)
import GHC.Generics (Generic)

import HSChain.PoW.Consensus
import HSChain.PoW.Logger
import HSChain.PoW.P2P
import HSChain.PoW.P2P.Types
import HSChain.PoW.Types
import HSChain.Network.TCP
import HSChain.Network.Types
import HSChain.Types.Merkle.Types
--import HSChain.Examples.Simple
import HSChain.Control.Util


----------------------------------------------------------------
--
----------------------------------------------------------------

-- | Simple in-memory implementation of DB
inMemoryView
  :: (Monad m, BlockData b, Show (BlockID b))
  => (Block b -> s -> Maybe s)  -- ^ Step function 
  -> s                          -- ^ Initial state
  -> BlockID b
  -> StateView m b
inMemoryView step = make (error "No revinding past genesis")
  where
    make previous s bid = view
      where
        view = StateView
          { stateBID    = bid
          , applyBlock  = \b -> case step b s of
              Nothing -> return Nothing
              Just s' -> return $ Just $  make view s' (blockID b)
          , revertBlock = return previous
          , flushState  = return ()
          }

viewKV :: Monad m => BlockID KV -> StateView m KV
viewKV bid = inMemoryView step Map.empty bid
  where
    step b m
      | or [ k `Map.member` m | (k,_) <- txs ] = Nothing
      | otherwise                              = Just $ Map.fromList txs <> m
      where
        txs = merkleValue $ kvData $ blockData b

inMemoryDB
  :: (MonadIO m, MonadIO n, BlockData b)
  => m (BlockDB n b)
inMemoryDB = do
  var <- liftIO $ newIORef Map.empty
  return BlockDB
    { storeBlock     = \b   -> liftIO $ modifyIORef' var $ Map.insert (blockID b) b
    , retrieveBlock  = \bid -> liftIO $ Map.lookup bid <$> readIORef var
    , retrieveHeader = \bid -> liftIO $ fmap toHeader . Map.lookup bid <$> readIORef var
    }

genesis :: Block KV
genesis = GBlock { blockHeight = Height 0
                 , prevBlock   = Nothing
                 , blockData   = KV { kvData = merkled [] }
                 }


mineBlock :: IsMerkle f => String -> GBlock KV f -> Block KV
mineBlock val b = GBlock
  { blockHeight = succ $ blockHeight b
  , prevBlock   = Just $! blockID b
  , blockData   = KV { kvData = merkled [ let Height h = blockHeight b
                                          in (fromIntegral h, val)
                                        ]
                     }
  }

----------------------------------------------------------------
-- Configuration
----------------------------------------------------------------

data Opts = Opts
  { cmdConfigPath :: [FilePath]
  , optPrintBCH   :: Bool
  , optMine       :: Bool
  }

parser :: Parser Opts
parser = do
  cmdConfigPath <- some $ strArgument
    (  metavar "PATH"
    <> help  "Path to configuration"
    <> showDefault
    )
  optPrintBCH <- switch
    (  long "print"
    <> help "Print blockchain"
    )
  optMine <- switch
    (  long "mine"
    <> help "Mine blocks"
    )
  return Opts{..}

data Cfg = Cfg
  { cfgPort  :: Word16
  , cfgPeers :: [NetAddr]
  , cfgLog   :: [ScribeSpec]
  , cfgStr   :: String
  }
  deriving stock    (Show,Generic)
  deriving anyclass (JSON.FromJSON)

main :: IO ()
main = do
  -- Parse CLI & read config
  Opts{..} <- customExecParser (prefs showHelpOnError)
            $ info (helper <*> parser)
              (  fullDesc
              <> header   "PoW node settings"
              <> progDesc ""
              )
  Cfg{..} <- loadYamlSettings (reverse cmdConfigPath) [] requireEnv
  -- 
  let netcfg = NetCfg { nKnownPeers     = 3
                      , nConnectedPeers = 3
                      }
  let net = newNetworkTcp cfgPort
  db <- inMemoryDB @_ @_ @KV
  let s0 = consensusGenesis genesis (viewKV (blockID genesis))
  withLogEnv "" "" (map makeScribe cfgLog) $ \logEnv ->
    runLoggerT logEnv $ evalContT $ do
      pow <- startNode netcfg net cfgPeers db s0
      let printBCH = when optPrintBCH $ do
            c <- atomicallyIO $ currentConsensus pow
            let loop bh@BH{bhBID = bid} = do
                  liftIO $ print (cfgStr, bid, bhHeight bh)
                  liftIO . print =<< retrieveBlock db bid
                  maybe (return ()) loop $ bhPrevious bh
            loop (c ^. bestHead . _1) 
      lift $ flip onException printBCH $ forever $ do
        t <- liftIO $ negate . log <$> randomRIO (0.5, 2)
        liftIO $ threadDelay $ round (1e6 * t :: Double)
        --
        when optMine $ do
          c <- atomicallyIO $ currentConsensus pow
          let h = c ^. bestHead . _1 . to asHeader
              b = mineBlock cfgStr h
          sendNewBlock pow b

