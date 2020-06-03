-- |HSChain.PoW.Node.hs
--
-- Main node loop.
--
-- You may use it directly or copy and tailor.
--
-- Copyright (C) ... 2020

{-# LANGUAGE ApplicativeDo       #-}
{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}

module HSChain.PoW.Node
  ( runNode
  ) where

import HSChain.PoW.Consensus
import HSChain.PoW.Logger
import HSChain.PoW.P2P
import HSChain.PoW.P2P.Types
import HSChain.PoW.Types
import qualified HSChain.POW as POWFunc
import HSChain.Network.TCP
import HSChain.Network.Types
import HSChain.Types.Merkle.Types
import HSChain.Examples.Simple
import HSChain.Examples.Util
import HSChain.Control.Class

-- |Node's configuration.
data Cfg = Cfg
  { cfgPort  :: Word16
  , cfgPeers :: [NetAddr]
  , cfgLog   :: [ScribeSpec]
  , cfgStr   :: String
  , cfgMaxH  :: Maybe Height
  }
  deriving stock    (Show, Generic)
  deriving anyclass (JSON.FromJSON)

-- |The process to run nodes.
--
-- Requires places to load config from, a flag indicating that
-- we are mining and genesis block.
runNode :: forall b . (BlockData b)
        => [String] -> Bool -> Block b ->IO ()
runNode pathsToConfig mine genesisBlock = do
  Cfg{..} <- loadYamlSettings pathsToConfig [] requireEnv
  --
  let netcfg = NetCfg { nKnownPeers     = 3
                      , nConnectedPeers = 3
                      }
  let net = newNetworkTcp cfgPort
  db <- inMemoryDB @_ @_ @b
  let s0 = consensusGenesis genesisBlock (viewKV (blockID genesisBlock))
  withLogEnv "" "" (map makeScribe cfgLog) $ \logEnv ->
    runLoggerT logEnv $ evalContT $ do
      pow' <- startNode netcfg net cfgPeers db s0
      let pow = pow'
                { sendNewBlock = \b -> do
                                 liftIO $ putStrLn $ "mined: "++show b
                                 (sendNewBlock pow') b
                }
      let printBCH = when optPrintBCH $ do
            c <- atomicallyIO $ currentConsensus pow
            let loop bh@BH{bhBID = bid} = do
                  liftIO $ print (cfgStr, bid, bhHeight bh)
                  liftIO . print =<< retrieveBlock db bid
                  maybe (return ()) loop $ bhPrevious bh
            loop (c ^. bestHead . _1)
      lift $ flip onException printBCH $ do
        upd <- atomicallyIO $ chainUpdate pow
        let doMine = do
              c   <- atomicallyIO $ currentConsensus pow
              now <- getCurrentTime
              let bh = c ^. bestHead . _1
              case cfgMaxH of
                Just h -> liftIO $ when (bhHeight bh > h) $ forever $ threadDelay maxBound
                Nothing -> return ()
              maybeB <- liftIO $ mineBlock now cfgStr bh
              case maybeB of
                Just b -> sendNewBlock pow b >>= \case
                                                Right () -> return ()
                                                Left  e  -> error $ show e
                Nothing -> doMine
        let loop tid = do
              (bh,_) <- awaitIO upd
              Just b <- retrieveBlock db (bhBID bh)
              liftIO $ print ( blockHeight b
                             , blockTime b
                             , bhBID bh
                             , kvTarget $ blockData b
                             , merkleValue $ kvData $ blockData b
                             )
              liftIO $ mapM_ killThread tid
              loop =<< if optMine then Just <$> fork doMine else return Nothing
        --
        loop =<< if optMine then Just <$> fork doMine else return Nothing


