{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE RecordWildCards   #-}
-- |
-- Abstract layer for distinguish Thundermint and some monitoring
-- libraries, such as Prometheus
module Thundermint.Monitoring (
    PrometheusGauges(..)
  , standardMonitoring
  , MonadTMMonitoring(..)
  ) where

import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Data.Text                  (Text)
import Prometheus

import Thundermint.Logger      (LoggerT(..),NoLogsT(..))
import Thundermint.Store       (DBT(..))
import Thundermint.Debug.Trace (TracerT(..))
import Thundermint.Blockchain.Types

----------------------------------------------------------------
-- Gauges
----------------------------------------------------------------

-- | Prometheus gauge to which values of given type could e written
data TGauge a = TGauge (a -> Double) !Gauge

-- | Collection of metrics for monitoring. This is dictionary of
--   functions which should be called to update
data PrometheusGauges = PrometheusGauges
  { prometheusHeight           :: !(TGauge Height)
  , prometheusRound            :: !(TGauge Round)
  , prometheusNumPeers         :: !(TGauge Int)
  , prometheusMempoolSize      :: !(TGauge Int)
  , prometheusMempoolAdded     :: !(TGauge Int)
  , prometheusMempoolDiscarded :: !(TGauge Int)
  , prometheusMempoolFiltered  :: !(TGauge Int)
  }

standardMonitoring :: (MonadIO m) => m PrometheusGauges
standardMonitoring = createMonitoring "thundermint"

-- | Allocate set of gauges with custom prefix
createMonitoring :: (MonadIO m) => Text -> m PrometheusGauges
createMonitoring prefix = do
  prometheusHeight      <- makeGauge (\(Height h) -> fromIntegral h)
    "blockchain_height_total"
    "Number of processed blocks"
  prometheusRound       <- makeGauge (\(Round r) -> fromIntegral r)
    "blockchain_rounds_total"
    "Number of rounds to commit block"
  prometheusNumPeers    <- makeGauge fromIntegral
    "peers_total"
    "Number of current connected peers"
  prometheusMempoolSize <- makeGauge fromIntegral
    "mempool_size_total"
    "Number of transactions in mempool"
  prometheusMempoolAdded <- makeGauge fromIntegral
    "mempool_added_total"
    "Number of transactions which were added to mempool"
  prometheusMempoolDiscarded <- makeGauge fromIntegral
    "mempool_discarded_total"
    "Number of transactions which were discarded immediately"
  prometheusMempoolFiltered <- makeGauge fromIntegral
    "mempool_filtered_total"
    "Number of transactions which were removed after being added"
  return PrometheusGauges{..}
  where
    makeGauge f nm help = do
      g <- register $ gauge $ Info (prefix <> "_" <> nm) help
      return $ TGauge f g


----------------------------------------------------------------
-- Monadic API
----------------------------------------------------------------

-- | Monad which supports monitoring of thundermint.
class Monad m => MonadTMMonitoring m where
  usingGauge :: (PrometheusGauges -> TGauge a) -> a -> m ()

-- | IO doesn't have monitoring
instance MonadTMMonitoring IO where
  usingGauge _ _ = return ()

instance MonadTMMonitoring m => MonadTMMonitoring (LoggerT m) where
  usingGauge f = lift . usingGauge f

instance MonadTMMonitoring m => MonadTMMonitoring (NoLogsT m) where
  usingGauge f = lift . usingGauge f

instance MonadTMMonitoring m => MonadTMMonitoring (TracerT m) where
  usingGauge f = lift . usingGauge f

instance MonadTMMonitoring m => MonadTMMonitoring (DBT rm alg a m) where
  usingGauge f = lift . usingGauge f
