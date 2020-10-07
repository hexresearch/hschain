{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- |
module HSChain.Control.Channels
  ( -- * Sink and sources
    Sink(..)
  , sinkIO
  , Src(..)
  , awaitIO
    -- ** Creation
  , queuePair
  , broadcastPair
    -- * Blocking calls to another thread
  , BlockingCall(..)
  , blockingCall
  , handleCall
  , handleCall_
  , handleBlockingCall
  , handleBlockingCall_
  ) where

import Control.Applicative
import Control.Concurrent.MVar
import Control.Concurrent.STM
import Control.Monad.IO.Class
import Data.Functor.Contravariant

import HSChain.Control.Util


-- | Sink end of 'TChan'\/'TQueue'. Monoid instance allows to write to
--   several channels simultaneously.
newtype Sink a = Sink { sink :: a -> STM () }

sinkIO :: MonadIO m => Sink a -> a -> m ()
sinkIO s = atomicallyIO . sink s

instance Contravariant Sink where
  contramap f (Sink s) = Sink $ s . f

instance Semigroup (Sink a) where
  Sink f <> Sink g = Sink $ \a -> f a >> g a

instance Monoid (Sink a) where
  mempty = Sink $ \_ -> return ()


-- | Source end of 'TChan'\/'TQueue'. Monoid instance allows to read
--   from several channels simultaneously.
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



-- | Data type which allows to send value to type @a@ to another
--   thread and block until another thread responds.
data BlockingCall a b = BlockingCall !a (MVar b)

blockingCall :: MonadIO m => Sink (BlockingCall a b) -> a -> m b
blockingCall dst a = do
  mv <- liftIO newEmptyMVar
  sinkIO dst (BlockingCall a mv)
  liftIO $ readMVar mv


handleBlockingCall :: MonadIO m => BlockingCall a b -> (a -> m (b,c)) -> m c
handleBlockingCall (BlockingCall a mv) fun = do
  (b,c) <- fun a
  liftIO $ c <$ tryPutMVar mv b

handleBlockingCall_ :: MonadIO m => BlockingCall a b -> (a -> m b) -> m ()
handleBlockingCall_ (BlockingCall a mv) fun = do
  b <- fun a
  liftIO $ () <$ tryPutMVar mv b


handleCall_ :: MonadIO m => Src (BlockingCall a b) -> (a -> m b) -> Src (m ())
handleCall_ src fun = flip handleBlockingCall_ fun <$> src

handleCall :: MonadIO m => Src (BlockingCall a b) -> (a -> m (b,c)) -> Src (m c)
handleCall src fun = flip handleBlockingCall fun <$> src
