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
{-# LANGUAGE DerivingVia         #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE ViewPatterns        #-}
module HSChain.PoW.Node
  ( Cfg(..)
  , runNode
  , hoistCont
    -- * Block storage
  , inMemoryDB
  , blockDatabase
  ) where

import qualified Data.Aeson as JSON
import Codec.Serialise

import Control.Concurrent
import Control.Monad
import Control.Monad.Cont
import Control.Monad.Trans.Cont

import Data.Word
import Data.Yaml.Config
import GHC.Generics (Generic)

import HSChain.Control.Channels
import HSChain.PoW.Consensus
import HSChain.Logger
import HSChain.PoW.P2P
import HSChain.PoW.P2P.Types
import HSChain.PoW.Types
import HSChain.PoW.Store
import HSChain.Network.TCP
import HSChain.Network.Types
import HSChain.Types.Merkle.Types
import HSChain.Control.Util
import HSChain.Control.Class
import HSChain.Config

-- |Node's configuration.
data Cfg = Cfg
  { cfgPort  :: Word16
  , cfgPeers :: [NetAddr]
  , cfgLog   :: [ScribeSpec]
  , cfgMaxH  :: Maybe Height
  , cfgDB    :: Maybe FilePath
  }
  deriving stock (Show, Generic)
  deriving (JSON.FromJSON) via SnakeCase (DropSmart (Config Cfg))


-- |The process to run nodes.
--
-- Requires places to load config from, a flag indicating that
-- we are mining and genesis block.
runNode
  :: (BlockData b, Mineable b, Show (b Proxy), Show (b Identity), Serialise (b Proxy), Serialise (b Identity))
  => [String]
  -> Bool
  -> StateView (LoggerT IO) b
  -> BlockDB   (LoggerT IO) b
  -> IO ()
runNode pathsToConfig miningNode sView db = do
  Cfg{..} <- loadYamlSettings pathsToConfig [] requireEnv
  --
  let netcfg = NetCfg { nKnownPeers     = 3
                      , nConnectedPeers = 3
                      }
  let net = newNetworkTcp cfgPort
  withLogEnv "" "" (map makeScribe cfgLog) $ \logEnv ->
    runLoggerT logEnv $ evalContT $ do
      c0  <- lift $ createConsensus db sView
      pow <- startNode netcfg net cfgPeers db c0
      when miningNode $ cforkLinked $ genericMiningLoop pow
      liftIO $ do
        ch <- atomicallyIO (chainUpdate pow)
        forever $ do (bh,_) <- awaitIO ch
                     print $ asHeader bh
                     print $ retarget bh

genericMiningLoop :: (Mineable b, MonadFork m) => PoW m b -> m x
genericMiningLoop pow = start
  where
    --
    start = do
      c  <- atomicallyIO $ currentConsensus pow
      ch <- atomicallyIO $ mempoolUpdates $ mempoolAPI pow
      let (bh, st, _) = _bestHead c
      loop ch =<< mine bh st Nothing
    --
    loop ch tid = do
      (bh, st, txs) <- awaitIO ch
      liftIO $ killThread tid
      loop ch =<< mine bh st (Just txs)
    -- Here we simply try again to create new block in case we wasnt'
    -- able to create one by fiddling nonce. At very least time should
    -- change
    mine bh st = fork . tryMine
      where
        tryMine mtxs = do
          t      <- getCurrentTime
          txs    <- case mtxs of
            Just txs -> return txs
            Nothing  -> atomicallyIO $ mempoolContent $ mempoolAPI pow
          bCand  <- createCandidateBlock st bh t txs
          (bMined,_) <- adjustPuzzle bCand
          case bMined of
            Just b  -> void $ sendNewBlock pow b
            Nothing -> tryMine Nothing

hoistCont :: (Monad n, Monad m) => (m a -> n b) -> ContT a m a -> ContT r n b
hoistCont f m = lift $ f $ evalContT m

