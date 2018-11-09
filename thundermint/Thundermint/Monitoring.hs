-- | Abstract layer for distinguish Thundermint and some monitoring libraries, such as Prometheus
--
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE FlexibleInstances          #-}

module Thundermint.Monitoring (
    module Prometheus,
    module Thundermint.Monitoring.Metrics,
    MonitorT'(..),
    runMonitorT',
    hoistMonitorT,
    setGaugeI
    ) where


import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Writer.Strict
import Control.Monad.Trans.Writer.Strict (WriterT, runWriterT, tell)
import Prometheus

import Thundermint.Logger
import Thundermint.Monitoring.Metrics
import Thundermint.Store.Internal.Query


newtype MonitorT' m a = MkMonitorT' (WriterT [IO ()] m a)
    deriving (Applicative, Functor, Monad, MonadTrans)


instance Monad m => MonadMonitor (MonitorT' m) where
    doIO f = MkMonitorT' $ tell [f] -- TODO fold all `[IO a]` into `IO a`


runMonitorT' :: Monad m => MonitorT' m a -> m (a, IO ())
runMonitorT' (MkMonitorT' writerT) = do
    (v, operations) <- runWriterT writerT
    return (v, sequence_ operations)


instance (MonadIO m) => MonadIO (MonitorT' m) where
    liftIO = lift . liftIO


instance (MonadReadDB m alg a) => MonadReadDB (MonitorT' m) alg a where
    askConnectionRO = lift askConnectionRO


instance (MonadDB m alg a) => MonadDB (MonitorT' m) alg a where
    askConnectionRW = lift askConnectionRW


instance (MonadLogger ml) => MonadLogger (MonitorT' ml) where
    logger sev str a = lift $ logger sev str a
    localNamespace fun (MkMonitorT' wrt) =
        MkMonitorT' (mapWriterT (localNamespace fun) wrt)


hoistMonitorT :: Monad m => m a -> MonitorT' m a
hoistMonitorT m = MkMonitorT' (WriterT ((,[]) <$> m))

setGaugeI :: (MonadMonitor m, Integral n) => Gauge -> n -> m ()
setGaugeI g i = setGauge g (fromIntegral i)
