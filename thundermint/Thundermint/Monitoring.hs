{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE RecordWildCards   #-}
-- |
-- Abstract layer for distinguish Thundermint and some monitoring
-- libraries, such as Prometheus
module Thundermint.Monitoring (
    PrometheusGauges(..)
  , hoistPrometheusGauges
  , noMonitoring
  , standardMonitoring
  ) where

import Control.Monad.IO.Class
import Prometheus

import Thundermint.Blockchain.Types

----------------------------------------------------------------
-- Data types

-- | Collection of metrics for monitoring. This is dictionary of
--   functions which should be called to update 
data PrometheusGauges m = PrometheusGauges
  { prometheusHeight      :: Height -> m ()
  , prometheusRound       :: Round  -> m ()
  , prometheusNumPeers    :: Int    -> m ()
  , prometheusMempoolSize :: Int    -> m ()
  }

hoistPrometheusGauges :: (forall a. m a -> n a) -> PrometheusGauges m -> PrometheusGauges n
hoistPrometheusGauges f PrometheusGauges{..} = PrometheusGauges
  { prometheusHeight      = f . prometheusHeight
  , prometheusRound       = f . prometheusRound
  , prometheusNumPeers    = f . prometheusNumPeers
  , prometheusMempoolSize = f . prometheusMempoolSize
  }


-- | Don't do any monitoring
noMonitoring :: Monad m => PrometheusGauges m
noMonitoring = PrometheusGauges
  { prometheusHeight      = \_ -> return ()
  , prometheusRound       = \_ -> return ()
  , prometheusNumPeers    = \_ -> return ()
  , prometheusMempoolSize = \_ -> return ()
  }

-- | Allocate set of gauges with standard names 
standardMonitoring :: (MonadIO m, MonadMonitor n) => m (PrometheusGauges n)
standardMonitoring = do
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
  return PrometheusGauges{..}
  where
    makeGauge f nm help = do
      g <- register $ gauge $ Info ("thundermint_" <> nm) help
      return $ \x -> setGauge g (f x)
