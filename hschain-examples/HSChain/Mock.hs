{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}
-- |
module HSChain.Mock where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Catch
import Control.Monad.Trans.Cont
import Data.Maybe                     (fromMaybe)
import qualified Data.Map.Strict as Map
import Katip           (LogEnv)
import System.Directory (createDirectoryIfMissing)
import System.FilePath  (takeDirectory)

import HSChain.Blockchain.Internal.Engine.Types
import HSChain.Control
import HSChain.Logger
import HSChain.Mock.KeyList
import HSChain.Mock.Types
import HSChain.P2P
import HSChain.Run
import HSChain.Store
import HSChain.Types
import qualified HSChain.Network.Mock as P2P


-- | Allocate resources for node
allocNode
  :: ( MonadIO m, MonadMask m)
  => NodeSpec a
  -> ContT r m (Connection 'RW a, LogEnv)
allocNode spec = do
  liftIO $ createDirectoryIfMissing True $ takeDirectory dbname
  conn   <- ContT $ withDatabase dbname
  logenv <- ContT $ withLogEnv "TM" "DEV" [ makeScribe s | s <- nspecLogFile spec ]
  return (conn,logenv)
  where
    dbname = fromMaybe "" $ nspecDbName spec

allocNetwork
  :: ( MonadIO m, MonadMask m)
  => P2P.MockNet
  -> Topology
  -> [NodeSpec a]
  -> ContT r m [(NodeSpec a, BlockchainNet, Connection 'RW a, LogEnv)]
allocNetwork net topo specs
  = forM networked $ \(net,spec) -> do
      (conn,logEnv) <- allocNode spec
      return (spec,net,conn,logEnv)
  where
    networked = allocateMockNetAddrs net topo specs

-- | Allocate mock P2P connections for node
allocateMockNetAddrs
  :: P2P.MockNet                -- ^ Mock network
  -> Topology                   -- ^ Nodes connection Interconnection
  -> [a]                        -- ^ List of nodes
  -> [(BlockchainNet, a)]
allocateMockNetAddrs net topo nodes =
  [ ( BlockchainNet { bchNetwork      = P2P.createMockNode net addr
                    , bchInitialPeers = connections addresses addr
                    }
    , n
    )
  | (addr, n) <- Map.toList addresses
  ]
  where
    addresses   = Map.fromList $ [ NetAddrV4 i 1337 | i <- [0..]] `zip` nodes
    connections = case topo of
        Ring    -> connectRing
        All2All -> connectAll2All


-- | Callback which aborts execution when blockchain exceed given
--   height. It's done by throwing 'Abort'.
callbackAbortAtH :: MonadThrow m => Height -> AppCallbacks m a
callbackAbortAtH hMax = mempty
  { appCommitCallback = \b ->
      when (blockHeight b > hMax) $ throwM Abort
  }
