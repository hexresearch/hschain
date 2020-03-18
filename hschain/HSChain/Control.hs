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
    runConcurrently
    -- * Contol
  , iterateM
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
import Control.Concurrent  (killThread)
import Control.Exception   (AsyncException, Exception(..))
import Control.Monad.Catch (MonadMask, bracket, mask, onException, throwM, try)
import Data.Type.Equality
import GHC.Exts              (Proxy#,proxy#)

import qualified Control.Concurrent as Conc
import HSChain.Control.Class

----------------------------------------------------------------
--
----------------------------------------------------------------


-- | Run computations concurrently. As soon as one thread finishes
--   execution normally or abnormally all other threads are killed.
--   Function blocks until all child threads finish execution. If
--   thread is killed by exceptions it's rethrown. Being killed by
--   'AsyncException' is considered normal termination.
runConcurrently
  :: (MonadIO m, MonadMask m, MonadFork m)
  => [m ()]              -- ^ Functions to run
  -> m ()
runConcurrently []      = return ()
runConcurrently [act]   = act
runConcurrently actions = do
  -- We communicate return status of thread via channel since we don't
  -- know a priory which will terminated first
  ch <- liftIO Conc.newChan
  -- Run child threads. We wait until one of threads terminate and
  -- then kill all others.
  (r, tids) <- bracket
    -- Acquisution is done under mask and fork doesn't block so we're
    -- couldn't be interrupted here
    (forM actions $ \f -> forkWithUnmask $ \restore -> do
        try (restore f) >>= liftIO . \case
          Right _ -> Conc.writeChan ch (Right ())
          Left  e -> case fromException e of
            Just (_ :: AsyncException) -> Conc.writeChan ch (Right ())
            _                          -> Conc.writeChan ch (Left  e)
    )
    -- throwTo and therefore killThread block and as such could be
    -- interrupted and leave threads behind. In order to avoid this
    -- and to parallelise killing we create threads per killThread
    (liftIO . mapM (Conc.forkIO . killThread))
    -- Wait until one of threads completes execution.
    (\tids -> do r <- liftIO $ Conc.readChan ch
                 return (r,tids)
    )
  -- Wait until all other threads terminate. We can be killed by async
  -- exceptions here. But it doesn't matter at this point since we
  -- sent black mark to every child thread
  case tids of
    []   -> return ()
    _:ts -> liftIO $ forM_ ts $ const $ Conc.readChan ch
  case r of
    Right () -> return ()
    Left  e  -> throwM e



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

