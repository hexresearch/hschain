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
  , inMemoryView
    -- * Block storage
  , inMemoryDB
  , blockDatabase
  ) where

import qualified Data.Aeson as JSON
import Codec.Serialise

-- import Control.Concurrent
-- import Control.Concurrent.STM
-- import Control.Monad
import Control.Monad.Catch
import Control.Monad.Cont
-- import Control.Monad.Trans.Cont

import Data.Word
-- import Data.Yaml.Config
import GHC.Generics (Generic)
-- import Lens.Micro

import HSChain.PoW.Consensus
import HSChain.Logger
-- import HSChain.PoW.P2P
-- import HSChain.PoW.P2P.Types
import HSChain.PoW.Types
import HSChain.PoW.Store
-- import HSChain.Network.TCP
import HSChain.Network.Types
import HSChain.Types.Merkle.Types
-- import HSChain.Control.Util
-- import HSChain.Control.Class
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
runNode :: (BlockData b, Mineable b, Show (b Identity), Serialise (b Proxy), Serialise (b Identity))
        => [String] -> Bool -> Block b
        -> (Block b -> s -> Maybe s)
        -> (BH b -> s -> ((Header b, [Tx b]), s))
        -> (Header b -> [Tx b] -> IO (Maybe (Block b)))
        -> s
        -> IO ()
runNode = undefined
-- runNode pathsToConfig miningNode genesisBlock step inventHeaderTxs inventBlock startState = do
--   Cfg{..} <- loadYamlSettings pathsToConfig [] requireEnv
--   --
--   let netcfg = NetCfg { nKnownPeers     = 3
--                       , nConnectedPeers = 3
--                       }
--   let net = newNetworkTcp cfgPort
--   db <- inMemoryDB genesisBlock
--   let s0 = consensusGenesis genesisBlock $
--                             inMemoryView step startState $
--                             blockID genesisBlock
--   withLogEnv "" "" (map makeScribe cfgLog) $ \logEnv ->
--     runLoggerT logEnv $ evalContT $ do
--       pow <- startNode netcfg net cfgPeers db s0
--       let printBCH = do
--             c <- atomicallyIO $ currentConsensus pow
--             let loop n bh@BH{bhBID = bid} = when (n > (0 :: Int)) $ do
--                   liftIO $ print (bid, bhHeight bh)
--                   liftIO . print =<< retrieveBlock db bid
--                   maybe (return ()) (loop (n-1)) $ bhPrevious bh
--             loop 100 (c ^. bestHead . _1)
--       lift $ flip onException printBCH $ do
--         let doMine currentBlock = do
--               now <- getCurrentTime
--               let toMine = currentBlock { blockTime = now }
--               maybeB <- fmap fst $ liftIO $ adjustPuzzle toMine
--               case maybeB of
--                 Just b -> sendNewBlock pow b >>= \case
--                                                   Right () -> return ()
--                                                   Left  e  -> error $ show e
--                 Nothing -> doMine currentBlock
--         let mineLoop baseBestHead tid = do
--               -- FIXME: Do something about this later
--               --              
--               -- maybeNewSituation <- atomicallyIO $ do
--               --    let cv = currentConsensusTVar pow
--               --    cc <- readTVar cv
--               --    let (bchBH, _, _) = _bestHead cc
--               --        (toMine, cc') = consensusComputeOnState cc $ inventHeaderTxs bchBH
--               --    if bchBH /= baseBestHead
--               --      then do
--               --        writeTVar cv cc'
--               --        return $ Just (bchBH, toMine)
--               --      else return Nothing
--               -- case maybeNewSituation of
--               --   Just (newBestHead, newHeaderTxs) -> do
--               --     liftIO $ killThread tid
--               --     case cfgMaxH of
--               --       Just h
--               --         | bhHeight newBestHead > h -> return ()
--               --       _ -> do
--               --            Just newBlock <- liftIO $ uncurry inventBlock newHeaderTxs
--               --            mineLoop newBestHead =<< fork (doMine newBlock)
--               --   Nothing -> mineLoop baseBestHead tid
--               undefined
--         --
--         if miningNode
--           then do
--             (startBestHead, headerTxs) <- atomicallyIO $ do
--                -- FIXME: Do something about this later
--                --
--                -- let cv = currentConsensusTVar pow
--                -- cc <- readTVar cv
--                -- let (bchBH, _, _) = _bestHead cc
--                --     (toMine, cc') = consensusComputeOnState cc $ inventHeaderTxs bchBH
--                -- writeTVar cv cc'
--                -- return (bchBH, toMine)
--                undefined
--             Just startBlock <- liftIO $ uncurry inventBlock headerTxs
--             mineLoop startBestHead =<< fork (doMine startBlock)
--           else liftIO $ forever $ threadDelay maxBound


-- | Simple in-memory implementation of DB
inMemoryView
  :: (Monad m, BlockData b, Show (BlockID b))
  => (Block b -> s -> Maybe s)       -- ^ Step function
  -> s                               -- ^ Initial state
  -> BlockID b
  -> StateView m b
inMemoryView step = make (error "No revinding past genesis")
  where
    make previous s bid = view
      where
        view = StateView
          { stateBID           = bid
          , applyBlock         = \_ b -> case step b s of
              Nothing -> return Nothing
              Just s' -> return $ Just $ make view s' (blockID b)
          , revertBlock        = return previous
          , flushState         = return view
          }

----------------------------------------------------------------
-- Databases
----------------------------------------------------------------

