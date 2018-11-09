{-# LANGUAGE OverloadedStrings #-}
module Thundermint.Monitoring.Metrics
    ( metricHeight
    , metricRounds
    , metricPeers
    ) where


import Data.Text (Text)
import Prometheus


-- | Prefix for metrics
--
prefix :: Text
prefix = "thundermint_"


-- | Current blockchain height
--
metricHeight :: Gauge
metricHeight = unsafeRegister $ gauge $
    Info (prefix <> "blockchain_height_total")
         "Number of processed blocks"
{-# NOINLINE metricHeight #-}


-- | Current rounds
--
metricRounds :: Gauge
metricRounds = unsafeRegister $ gauge $
    Info (prefix <> "blockchain_rounds_total")
         "Number of rounds to commit block"
{-# NOINLINE metricRounds #-}


-- | Current peers
--
metricPeers :: Gauge
metricPeers = unsafeRegister $ gauge $
    Info (prefix <> "peers_total")
         "Number of current connected peers"
{-# NOINLINE metricPeers #-}
