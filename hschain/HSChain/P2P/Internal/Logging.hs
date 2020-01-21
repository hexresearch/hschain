{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}

module HSChain.P2P.Internal.Logging where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.IORef
import HSChain.Logger
import HSChain.Monitoring

import qualified Data.Aeson    as JSON
import qualified Data.Aeson.TH as JSON
import qualified Katip

-- | Counter for counting send/receive event
data Counter = Counter !(IORef Int) !(IORef Int)

newCounter :: MonadIO m => m Counter
newCounter = Counter <$> liftIO (newIORef 0) <*> liftIO (newIORef 0)

tickSend :: (MonadIO m) => Counter -> m ()
tickSend (Counter s _) = liftIO $ atomicModifyIORef' s (\n -> (n+1, ()))

tickRecv :: (MonadIO m) => Counter -> m ()
tickRecv (Counter _ r) = liftIO $ atomicModifyIORef' r (\n -> (n+1, ()))

readSend :: MonadIO m => Counter -> m Int
readSend (Counter s _) = liftIO $ readIORef s

readRecv :: MonadIO m => Counter -> m Int
readRecv (Counter _ r) = liftIO $ readIORef r

----------------------------------------------------------------
-- Data types
----------------------------------------------------------------

data LogGossip = LogGossip
  { gossip'TxPV  :: !Int
  , gossip'RxPV  :: !Int
  , gossip'TxPC  :: !Int
  , gossip'RxPC  :: !Int
  , gossip'TxB   :: !Int
  , gossip'RxB   :: !Int
  , gossip'TxP   :: !Int
  , gossip'RxP   :: !Int
  , gossip'TxTx  :: !Int
  , gossip'RxTx  :: !Int
  , gossip'TxPex :: !Int
  , gossip'RxPex :: !Int
  }
  deriving (Show)
JSON.deriveJSON JSON.defaultOptions
  { JSON.fieldLabelModifier = drop 7 } ''LogGossip
instance Katip.ToObject LogGossip
instance Katip.LogItem  LogGossip where
  payloadKeys _        _ = Katip.AllKeys

data GossipCounters = GossipCounters
  { prevote   :: !Counter
  , precommit :: !Counter
  , blocks    :: !Counter
  , proposals :: !Counter
  , tx        :: !Counter
  , pex       :: !Counter
  }

newGossipCounters :: MonadIO m => m GossipCounters
newGossipCounters = GossipCounters
                 <$> newCounter
                 <*> newCounter
                 <*> newCounter
                 <*> newCounter
                 <*> newCounter
                 <*> newCounter

logGossip
  :: ( MonadIO m, MonadLogger m, MonadTMMonitoring m )
  => GossipCounters
  -> m ()
logGossip GossipCounters{..} = do
  (gossip'TxPV  , gossip'RxPV ) <- readPair prevote
  (gossip'TxPC  , gossip'RxPC ) <- readPair precommit
  (gossip'TxB   , gossip'RxB  ) <- readPair blocks
  (gossip'TxP   , gossip'RxP  ) <- readPair proposals
  (gossip'TxTx  , gossip'RxTx ) <- readPair tx
  (gossip'TxPex , gossip'RxPex) <- readPair pex
  --
  usingVector prometheusGossip ("TX","prevote")   gossip'TxPV
  usingVector prometheusGossip ("RX","prevote")   gossip'RxPV
  usingVector prometheusGossip ("TX","precommit") gossip'TxPC
  usingVector prometheusGossip ("RX","precommit") gossip'RxPC
  usingVector prometheusGossip ("TX","proposal")  gossip'TxP
  usingVector prometheusGossip ("RX","proposal")  gossip'RxP
  usingVector prometheusGossip ("TX","block")     gossip'TxB
  usingVector prometheusGossip ("RX","block")     gossip'RxB
  --
  logger InfoS "Gossip stats" LogGossip{..}
 where
   readPair cnt = (,) <$> readSend cnt
                      <*> readRecv cnt


