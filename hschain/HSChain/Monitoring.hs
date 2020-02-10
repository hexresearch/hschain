{-# LANGUAGE DefaultSignatures          #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}
-- |
-- Abstract layer for distinguish HSChain and some monitoring
-- libraries, such as Prometheus
module HSChain.Monitoring (
    PrometheusGauges(..)
  , TGauge(..)
  , standardMonitoring
  , MonadTMMonitoring(..)
  , addCounterNow
  , setTGaugeNow
  , incTCVectorNow
  ) where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import qualified Control.Monad.Trans.State.Strict as SS
import qualified Control.Monad.Trans.State.Lazy   as SL
import Data.Monoid ((<>))
import Data.Text                  (Text)
import Numeric.Natural
import Pipes       (Proxy)
import Prometheus

import HSChain.Logger      (LoggerT(..),NoLogsT(..),StdoutLogT(..))
import HSChain.Store       (DBT(..))
import HSChain.Types.Blockchain

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
  , prometheusNTx              :: !Counter
  , prometheusNumPeers         :: !(TGauge Int)
  , prometheusKnownAddrs       :: !(TGauge Int)
  , prometheusMempoolSize      :: !(TGauge Int)
  , prometheusMempoolAdded     :: !(TGauge Int)
  , prometheusMempoolDiscarded :: !(TGauge Int)
  , prometheusMempoolFiltered  :: !(TGauge Int)
  , prometheusGossip           :: !(Vector (Text,Text) Counter)
  , prometheusMsgQueue         :: !(TGauge   Natural)
  }

standardMonitoring :: (MonadIO m) => m PrometheusGauges
standardMonitoring = createMonitoring "hschain"

-- | Allocate set of gauges with custom prefix
createMonitoring :: (MonadIO m) => Text -> m PrometheusGauges
createMonitoring prefix = do
  prometheusHeight      <- makeGauge (\(Height h) -> fromIntegral h)
    "blockchain_height_total"
    "Number of processed blocks"
  prometheusRound       <- makeGauge (\(Round r) -> fromIntegral r)
    "blockchain_rounds_total"
    "Number of rounds to commit block"
  prometheusNTx         <- makeCounter
    "blockchain_n_tx"
    "Number of committed transactions since app start"
  prometheusNumPeers    <- makeGauge fromIntegral
    "peers_total"
    "Number of current connected peers"
  prometheusKnownAddrs  <- makeGauge fromIntegral
    "peers_known"
    "Number of known peer addresses"
  -- Mempool
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
  -- Gossip
  prometheusGossip <- makeVector ("dir","type")
    "gossip_total"
    "Gossip statistics"
  --
  prometheusMsgQueue <- makeGauge fromIntegral
    "msg_queue_incoming"
    "Number of messages in incoming queue"
  return PrometheusGauges{..}
  where
    makeCounter nm help =
      register $ counter $ Info (prefix <> "_" <> nm) help
    --
    makeGauge :: MonadIO m => (a -> Double) -> Text -> Text -> m (TGauge a)
    makeGauge f nm help = do
      g <- register $ gauge $ Info (prefix <> "_" <> nm) help
      return $ TGauge f g
    --
    makeVector label nm help = do
      register $ vector label $ counter $ Info (prefix <> "_" <> nm) help

----------------------------------------------------------------
-- Monadic API
----------------------------------------------------------------

-- | Monad which supports monitoring of HSChain.
class Monad m => MonadTMMonitoring m where
  usingCounter :: (PrometheusGauges -> Counter) -> Int -> m ()
  default usingCounter :: (m ~ t n, MonadTrans t, MonadTMMonitoring n)
                       => (PrometheusGauges -> Counter) -> Int -> m ()
  usingCounter f n = lift $ usingCounter f n
  --
  usingGauge   :: (PrometheusGauges -> TGauge a) -> a -> m ()
  default usingGauge :: (m ~ t n, MonadTrans t, MonadTMMonitoring n)
                     => (PrometheusGauges -> TGauge a) -> a -> m ()
  usingGauge f n = lift $ usingGauge f n
  --
  usingVector  :: (Label l) => (PrometheusGauges -> Vector l Counter) -> l -> m ()
  default usingVector :: (m ~ t n, MonadTrans t, MonadTMMonitoring n, Label l)
                      => (PrometheusGauges -> Vector l Counter) -> l -> m ()
  usingVector f l = lift $ usingVector f l

addCounterNow :: (MonadIO m) => Counter -> Int -> m ()
addCounterNow cnt = void . runMonitorNowT . addCounter cnt . fromIntegral

setTGaugeNow :: (MonadIO m) => TGauge a -> a ->  m ()
setTGaugeNow (TGauge f g) x = runMonitorNowT $ setGauge g (f x)

incTCVectorNow :: (Label l, MonadIO m) => Vector l Counter -> l -> m ()
incTCVectorNow v l = runMonitorNowT $
  withLabel v l incCounter


newtype MonitorNowT m a = MonitorNowT { runMonitorNowT :: m a }
  deriving (Functor, Applicative, Monad, MonadIO)

instance MonadIO m => MonadMonitor (MonitorNowT m) where
  doIO = liftIO


-- | IO doesn't have monitoring
instance MonadTMMonitoring IO where
  usingCounter _ _ = return ()
  usingGauge   _ _ = return ()
  usingVector  _ _ = return ()

instance MonadTMMonitoring m => MonadTMMonitoring (LoggerT    m)
instance MonadTMMonitoring m => MonadTMMonitoring (NoLogsT    m)
instance MonadTMMonitoring m => MonadTMMonitoring (StdoutLogT m)
instance MonadTMMonitoring m => MonadTMMonitoring (DBT rm a m)
instance MonadTMMonitoring m => MonadTMMonitoring (Proxy a b c d m)
instance MonadTMMonitoring m => MonadTMMonitoring (ReaderT   r m)
instance MonadTMMonitoring m => MonadTMMonitoring (SS.StateT s m)
instance MonadTMMonitoring m => MonadTMMonitoring (SL.StateT s m)
