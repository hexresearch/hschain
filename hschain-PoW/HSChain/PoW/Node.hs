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
  , inMemoryView
  , inMemoryDB
  ) where

import qualified Data.Aeson as JSON
import Codec.Serialise

import Control.Concurrent

import Control.Monad
import Control.Monad.Catch
import Control.Monad.Cont
import Control.Monad.Trans.Cont

import Data.IORef

import qualified Data.Map.Strict as Map

import Data.Word

import Data.Yaml.Config

import GHC.Generics (Generic)

import Lens.Micro

import HSChain.PoW.Consensus
import HSChain.PoW.Logger
import HSChain.PoW.P2P
import HSChain.PoW.P2P.Types
import HSChain.PoW.Types
import HSChain.Network.TCP
import HSChain.Network.Types
import HSChain.Types.Merkle.Types
import HSChain.Control.Util
import HSChain.Control.Class

-- |Node's configuration.
data Cfg = Cfg
  { cfgPort  :: Word16
  , cfgPeers :: [NetAddr]
  , cfgLog   :: [ScribeSpec]
  , cfgMaxH  :: Maybe Height
  }
  deriving stock    (Show, Generic)
  deriving anyclass (JSON.FromJSON)

-- |The process to run nodes.
--
-- Requires places to load config from, a flag indicating that
-- we are mining and genesis block.
runNode :: forall b s . (BlockData b, Mineable b, Show (b Identity), Serialise (b Proxy), Serialise (b Identity))
        => [String] -> Bool -> Block b
        -> (Block b -> s -> Maybe s) -> s
        -> (BH b -> IO (Block b)) -> IO ()
runNode pathsToConfig miningNode genesisBlock step startState fetchBlock = do
  Cfg{..} <- loadYamlSettings pathsToConfig [] requireEnv
  --
  let netcfg = NetCfg { nKnownPeers     = 3
                      , nConnectedPeers = 3
                      }
  let net = newNetworkTcp cfgPort
  db <- inMemoryDB @_ @_ @b
  let s0 = consensusGenesis genesisBlock (inMemoryView step startState (blockID genesisBlock))
  withLogEnv "" "" (map makeScribe cfgLog) $ \logEnv ->
    runLoggerT logEnv $ evalContT $ do
      pow' <- startNode netcfg net cfgPeers db s0
      let pow = pow'
                { sendNewBlock = \b -> do
                                 liftIO $ putStrLn $ "mined: "++show b
                                 (sendNewBlock pow') b
                }
      let printBCH = do
            c <- atomicallyIO $ currentConsensus pow
            let loop n bh@BH{bhBID = bid} = when (n > (0 :: Int)) $ do
                  liftIO $ print (bid, bhHeight bh)
                  liftIO . print =<< retrieveBlock db bid
                  maybe (return ()) (loop (n-1)) $ bhPrevious bh
            loop 100 (c ^. bestHead . _1)
      lift $ flip onException printBCH $ do
        let doMine = do
              c   <- atomicallyIO $ currentConsensus pow
              let bh = c ^. bestHead . _1
              case cfgMaxH of
                Just h -> liftIO $ when (bhHeight bh > h) $ forever $ threadDelay maxBound
                Nothing -> return ()
              toMine <- liftIO $ fetchBlock bh
              maybeB <- fmap fst $ liftIO $ adjustPuzzle toMine
              case maybeB of
                Just b -> sendNewBlock pow b >>= \case
                                                Right () -> return ()
                                                Left  e  -> error $ show e
                Nothing -> doMine
        let loop tid = do
              liftIO $ mapM_ killThread tid
              loop =<< if miningNode then Just <$> fork doMine else return Nothing
        --
        loop =<< if miningNode then Just <$> fork doMine else return Nothing


-- | Simple in-memory implementation of DB
inMemoryView
  :: (Monad m, BlockData b, Show (BlockID b))
  => m (Block b)                -- ^ Invent a block from state.
  -> (Tx b -> m ())             -- ^ Add transaction into state.
  -> (Block b -> s -> Maybe s)  -- ^ Step function 
  -> s                          -- ^ Initial state
  -> BlockID b
  -> StateView m b
inMemoryView inventBlock addTransaction step = make (error "No revinding past genesis")
  where
    make previous s bid = view
      where
        view = StateView
          { stateBID        = bid
          , applyBlock      = \b -> case step b s of
              Nothing -> return Nothing
              Just s' -> return $ Just $  make view s' (blockID b)
          , revertBlock     = return previous
          , flushState      = return ()
          , inventBlock     = inventBlock
          , addTransactions = addTransaction
          }

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
