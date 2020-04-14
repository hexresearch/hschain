{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Rank2Types            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}
-- |
module HSChain.Control (
    -- * Contol
    iterateM
    -- * Generalized MVar-code
  , withMVarM
  , modifyMVarM
  , modifyMVarM_
    -- * throwing on Maybe and Either
    -- * Products with lookup by type
  , (:*:)(..)
  , Has(..)
  , (^..)
  ) where

import Control.Concurrent.MVar
import Control.Monad
import Control.Monad.IO.Class
import qualified Data.Aeson                       as JSON
import Control.Monad.Catch (MonadMask, mask, onException)
import Data.Type.Equality
import GHC.Exts              (Proxy#,proxy#)


----------------------------------------------------------------
-- MVars
----------------------------------------------------------------

withMVarM :: (MonadMask m, MonadIO m) => MVar a -> (a -> m b) -> m b
withMVarM m action =
  mask $ \restore -> do
    a <- liftIO $ takeMVar m
    b <- restore (action a) `onException` liftIO (putMVar m a)
    liftIO $ putMVar m a
    return b

modifyMVarM_ :: (MonadMask m, MonadIO m) => MVar a -> (a -> m a) -> m ()
modifyMVarM_ m action =
  mask $ \restore -> do
    a  <- liftIO $ takeMVar m
    a' <- restore (action a) `onException` liftIO (putMVar m a)
    liftIO $ putMVar m a'

modifyMVarM :: (MonadMask m, MonadIO m) => MVar a -> (a -> m (a,b)) -> m b
modifyMVarM m action =
  mask $ \restore -> do
    a      <- liftIO $ takeMVar m
    (a',b) <- restore (action a) `onException` liftIO (putMVar m a)
    liftIO $ putMVar m a'
    return b



----------------------------------------------------------------
-- Anonymous products
----------------------------------------------------------------

-- | Anonymous products. Main feature is lookup of value by its type
data a :*: b = a :*: b
  deriving (Show,Eq)
infixr 5 :*:

instance (JSON.FromJSON a, JSON.FromJSON b) => JSON.FromJSON (a :*: b) where
  parseJSON = JSON.withObject "Expecting object" $ \o -> do
    a <- JSON.parseJSON (JSON.Object o)
    b <- JSON.parseJSON (JSON.Object o)
    return (a :*: b)


-- | Obtain value from product using its type
class Has a x where
  getT :: a -> x

-- | Lens-like getter
(^..) :: (Has a x) => a -> (x -> y) -> y
a ^.. f = f (getT a)


class HasCase a x (eq :: Bool) where
  getCase :: Proxy# eq -> a -> x

instance {-# OVERLAPPABLE #-} (a ~ b) => Has a b where
  getT = id
instance HasCase (a :*: b) x (a == x) => Has (a :*: b) x where
  getT = getCase (proxy# :: Proxy# (a == x))

instance (a ~ x)   => HasCase (a :*: b) x 'True where
  getCase _ (a :*: _) = a
instance (Has b x) => HasCase (a :*: b) x 'False where
  getCase _ (_ :*: b) = getT b


iterateM :: (Monad m) => a -> (a -> m a) -> m b
iterateM x0 f = let loop = f >=> loop in loop x0

