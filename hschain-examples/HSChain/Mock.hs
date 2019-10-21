{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
-- |
module HSChain.Mock where

import Codec.Serialise
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
import HSChain.Crypto
import HSChain.Logger
import HSChain.Mock.KeyList
import HSChain.Mock.Types
import HSChain.P2P
import HSChain.Run
import HSChain.Store
import HSChain.Types
import qualified HSChain.P2P.Network as P2P


-- | Allocate mock P2P connections for node
allocateMockNetAddrs
  :: P2P.MockNet                -- ^ Mock network
  -> Topology                   -- ^ Nodes connection Interconnection
  -> [a]                        -- ^ List of nodes
  -> [BlockchainNet :*: a]
allocateMockNetAddrs net topo nodes =
  [ BlockchainNet { bchNetwork      = P2P.createMockNode net addr
                  , bchInitialPeers = connections addresses addr
                  } :*: n
  | (addr, n) <- Map.toList addresses
  ]
  where
    addresses   = Map.fromList $ [ NetAddrV4 i 1337 | i <- [0..]] `zip` nodes
    connections = case topo of
        Ring    -> connectRing
        All2All -> connectAll2All


-- | Allocate resources for node
allocNode
  :: ( MonadIO m, MonadMask m
     , Crypto alg, Serialise a, Eq a, Show a, Has x NodeSpec)
  => Block alg a                -- ^ Genesis block
  -> x                          -- ^ Node parameters
  -> ContT r m (Connection 'RW alg a, LogEnv)
allocNode genesis x = do
  liftIO $ createDirectoryIfMissing True $ takeDirectory dbname
  conn   <- ContT $ withDatabase dbname genesis
  logenv <- ContT $ withLogEnv "TM" "DEV" [ makeScribe s | s <- x ^.. nspecLogFile ]
  return (conn,logenv)
  where
    dbname = fromMaybe "" $ x ^.. nspecDbName

-- | Callback which aborts execution when blockchain exceed given
--   height. It's done by throwing 'Abort'.
callbackAbortAtH :: MonadThrow m => Height -> AppCallbacks m alg a
callbackAbortAtH hMax = mempty
  { appCommitCallback = \b ->
      when (headerHeight (blockHeader b) > hMax) $ throwM Abort
  }
