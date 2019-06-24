{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE Rank2Types          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}
-- |
module Thundermint.Control (
    -- *
    FunctorF(..)
  , FloatOut(..)
  , foldF
  , traverseF_
    -- *
  , MonadFork(..)
  , forkLinked
  , runConcurrently
    -- * Generalized MVar-code
  , withMVarM
  , modifyMVarM
  , modifyMVarM_
    -- * Mutex
  , Mutex
  , newMutex
  , withMutex
    -- * throwing on Maybe and Either
  , throwNothing
  , throwNothingM
  , throwLeft
  , throwLeftM
    -- * Generalized bracket
  , withMany
  ) where

import Control.Applicative
import Control.Concurrent.MVar
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Cont   (ContT(..))
import Control.Monad.Trans.Reader
import qualified Control.Monad.Trans.State.Strict as SS
import qualified Control.Monad.Trans.State.Lazy   as SL
import Control.Concurrent  (ThreadId, killThread, myThreadId, throwTo)
import Control.Exception   (AsyncException, Exception(..), SomeException(..))
import Control.Monad.Catch (MonadMask, MonadThrow, bracket, mask, onException, throwM, try)
import Data.Functor.Compose
import Data.Functor.Identity
import Data.Typeable         (Proxy(..))

import qualified Control.Concurrent as Conc

----------------------------------------------------------------
--
----------------------------------------------------------------

class FunctorF k where
  fmapF :: (forall a. f a -> g a) -> k f -> k g

class FunctorF k => FloatOut k where
  -- | Sequence outer layer of effects in container
  floatOut    :: Applicative f => k (f `Compose` g) -> f (k g)
  -- | Sequence effects inside container
  floatEffect :: Applicative f => k f -> f ()
  floatEffect = void . floatOut . fmapF (Compose . fmap Identity)
  -- | Traverse container using effectful function and discard result
  --   of traversal
  traverseEff :: Applicative f => (forall a. g a -> f ()) -> k g -> f ()
  traverseEff f = void . floatOut . fmapF (Compose . fmap Const . f)

foldF :: (Monoid m, FloatOut k) => (forall a. f a -> m) -> k f -> m
foldF f = getConst . traverseEff (Const . f)

traverseF_ :: (FloatOut k, Applicative f) => (forall a. g a -> f a) -> k g -> f ()
traverseF_ f = floatEffect . fmapF f


instance FunctorF Proxy where
  fmapF _ Proxy = Proxy

instance FloatOut Proxy where
  floatOut      Proxy = pure Proxy
  floatEffect   Proxy = pure ()
  traverseEff _ Proxy = pure ()

----------------------------------------------------------------
--
----------------------------------------------------------------

-- | Type class for monads which could be forked
class MonadIO m => MonadFork m where
  fork           :: m () -> m ThreadId
  forkFinally    :: m a -> (Either SomeException a -> m ()) -> m ThreadId
  forkWithUnmask :: ((forall a. m a -> m a) -> m ()) -> m ThreadId

instance MonadFork IO where
  fork           = Conc.forkIO
  forkFinally    = Conc.forkFinally
  forkWithUnmask = Conc.forkIOWithUnmask

instance MonadFork m => MonadFork (ReaderT r m) where
  fork (ReaderT action) = ReaderT $ fork . action
  forkFinally (ReaderT action) fini = ReaderT $ \r ->
    forkFinally (action r) (\e -> runReaderT (fini e) r)
  forkWithUnmask cont = ReaderT $ \r -> forkWithUnmask $ \restore ->
    runReaderT (cont (liftF restore)) r
    where
      liftF f m = ReaderT $ f . runReaderT m

instance MonadFork m => MonadFork (SS.StateT s m) where
  fork action = SS.StateT $ \s -> do
    tid <- fork $ SS.evalStateT action s
    return (tid,s)
  forkFinally action fini = SS.StateT $ \s -> do
    tid <- forkFinally (SS.evalStateT action s) (flip SS.evalStateT s . fini)
    return (tid,s)
  forkWithUnmask cont = SS.StateT $ \s -> do
    tid <- forkWithUnmask $ \restore ->
      SS.evalStateT (cont (liftF restore)) s
    return (tid,s)
    where
      liftF f m = SS.StateT $ f . SS.runStateT m

instance MonadFork m => MonadFork (SL.StateT s m) where
  fork action = SL.StateT $ \s -> do
    tid <- fork $ SL.evalStateT action s
    return (tid,s)
  forkFinally action fini = SL.StateT $ \s -> do
    tid <- forkFinally (SL.evalStateT action s) (flip SL.evalStateT s . fini)
    return (tid,s)
  forkWithUnmask cont = SL.StateT $ \s -> do
    tid <- forkWithUnmask $ \restore ->
      SL.evalStateT (cont (liftF restore)) s
    return (tid,s)
    where
      liftF f m = SL.StateT $ f . SL.runStateT m


-- | Fork thread. Any exception except `AsyncException` in forked
--   thread is forwarded to original thread. When parent thread dies
--   child thread is killed too
forkLinked :: (MonadIO m, MonadMask m, MonadFork m)
           => m a              -- ^ Action to execute in forked thread
           -> m b              -- ^ What to do while thread executes
           -> m b
forkLinked action io = do
  tid <- liftIO myThreadId
  -- NOTE: Resource in bracket is acquired with asynchronous
  --       exceptions masked so child thread inherits masked state and
  --       needs to be explicitly unmasked
  bracket
    (forkWithUnmask $ \restore ->
         try (restore action) >>= \case
          Right _ -> return ()
          Left  e -> case fromException e of
            Just (_ :: AsyncException) -> return ()
            _                          -> liftIO $ throwTo tid e
    )
    (liftIO . killThread)
    (const io)



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
runConcurrently actions = do
  -- We communicate return status of thread via channel since we don't
  -- know a priory which will terminated first
  ch <- liftIO $ Conc.newChan
  -- Run child threads. We wait until one of threads terminate and
  -- then kill all others.
  (r, tids) <- bracket
    (forM actions $ \f -> forkWithUnmask $ \restore -> do
        try (restore f) >>= liftIO . \case
          Right _ -> Conc.writeChan ch (Right ())
          Left  e -> case fromException e of
            Just (_ :: AsyncException) -> Conc.writeChan ch (Right ())
            _                          -> Conc.writeChan ch (Left  e)
    )
    (liftIO . mapM killThread)
    (\tids -> do r <- liftIO $ Conc.readChan ch
                 return (r,tids)
    )
  -- Wait until all other threads terminate. We can be killed by async
  -- exceptions here. But so be it
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
-- Mutex
----------------------------------------------------------------

newtype Mutex = Mutex (MVar ())

newMutex :: MonadIO m => m Mutex
newMutex = Mutex <$> liftIO (newMVar ())

withMutex :: (MonadMask m, MonadIO m) => Mutex -> m a -> m a
withMutex (Mutex m) action = do
  mask $ \restore -> do
    () <- liftIO (takeMVar m)
    a  <- restore action `onException` liftIO (putMVar m ())
    liftIO (putMVar m ())
    return a

----------------------------------------------------------------
--
----------------------------------------------------------------

-- | Convert some @withResource@ function to work with multiple
--   resources at once
withMany
  :: (Monad m, Traversable t)
  => (a   -> (b   -> m r) -> m r)
  -> (t a -> (t b -> m r) -> m r)
withMany run = runContT . traverse (ContT . run)

throwNothing :: (Exception e, MonadThrow m) => e -> Maybe a -> m a
throwNothing e = maybe (throwM e) pure

throwNothingM :: (Exception e, MonadThrow m) => e -> m (Maybe a) -> m a
throwNothingM e m = m >>= throwNothing e

throwLeft :: (Exception e, MonadThrow m) => Either e a -> m a
throwLeft = either throwM pure

throwLeftM :: (Exception e, MonadThrow m) => m (Either e a) -> m a
throwLeftM m = m >>= throwLeft
