{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- |
module HSChain.Control.Channels where

import Control.Applicative
import Control.Concurrent.STM
import Control.Monad.IO.Class
import Data.Functor.Contravariant

import HSChain.Control.Util


-- | Sink for channels
newtype Sink a = Sink { sink :: a -> STM () }

sinkIO :: MonadIO m => Sink a -> a -> m ()
sinkIO s = atomicallyIO . sink s

instance Contravariant Sink where
  contramap f (Sink s) = Sink $ s . f

instance Semigroup (Sink a) where
  Sink f <> Sink g = Sink $ \a -> f a >> g a

instance Monoid (Sink a) where
  mempty = Sink $ \_ -> return ()


-- | Source of data in concurrent programs
newtype Src a = Src { await :: STM a }
  deriving newtype Functor

awaitIO :: MonadIO m => Src a -> m a
awaitIO = atomicallyIO . await

instance Semigroup (Src a) where
  Src a <> Src b = Src $ a <|> b

instance Monoid (Src a) where
  mempty = Src empty


queuePair :: MonadIO m => m (Sink a, Src a)
queuePair = do
  q <- liftIO newTQueueIO
  return ( Sink $ writeTQueue q
         , Src  $ readTQueue  q
         )

broadcastPair :: MonadIO m => m (Sink a, STM (Src a))
broadcastPair = do
  ch <- liftIO newBroadcastTChanIO
  return ( Sink $ writeTChan ch
         , do ch' <- dupTChan ch
              return $ Src $ readTChan ch'
         )
