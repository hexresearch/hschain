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
    -- *
    MonadFork(..)
  , forkFinally
  , forkLinked
  , forkLinkedIO
  , runConcurrently
    -- * Shepherd
  , Shepherd
  , withShepherd
  , newSheep
  , newSheepFinally
    -- * Contol
  , iterateM
  , atomicallyIO
    -- * Generalized MVar-code
  , withMVarM
  , modifyMVarM
  , modifyMVarM_
    -- * throwing on Maybe and Either
  , throwNothing
  , throwNothingM
  , throwLeft
  , throwLeftM
    -- * Products with lookup by type
  , (:*:)(..)
  , Has(..)
  , (^..)
  ) where

import Control.Concurrent.MVar
import Control.Concurrent.STM
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
import qualified Control.Monad.Trans.State.Strict as SS
import qualified Control.Monad.Trans.State.Lazy   as SL
import qualified Data.Aeson                       as JSON
import qualified Data.Map.Strict                  as Map
import Control.Concurrent  (ThreadId, killThread, myThreadId, throwTo)
import Control.Exception   (AsyncException, Exception(..), SomeException(..))
import Control.Monad.Catch (MonadMask, MonadThrow, bracket, mask, onException, throwM, try, finally)
import Data.Type.Equality
import GHC.Exts              (Proxy#,proxy#)

import qualified Control.Concurrent as Conc


----------------------------------------------------------------
--
----------------------------------------------------------------

-- | Type class for monads which could be forked
class MonadIO m => MonadFork m where
  fork           :: m () -> m ThreadId
  forkWithUnmask :: ((forall a. m a -> m a) -> m ()) -> m ThreadId

forkFinally
  :: (MonadFork m, MonadMask m)
  => m a -> (Either SomeException a -> m ()) -> m ThreadId
forkFinally action fini =
  mask $ \restore ->
    fork $ try (restore action) >>= fini

instance MonadFork IO where
  fork           = Conc.forkIO
  forkWithUnmask = Conc.forkIOWithUnmask

instance MonadFork m => MonadFork (ReaderT r m) where
  fork (ReaderT action) = ReaderT $ fork . action
  forkWithUnmask cont = ReaderT $ \r -> forkWithUnmask $ \restore ->
    runReaderT (cont (liftF restore)) r
    where
      liftF f m = ReaderT $ f . runReaderT m

instance MonadFork m => MonadFork (SS.StateT s m) where
  fork action = SS.StateT $ \s -> do
    tid <- fork $ SS.evalStateT action s
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

-- | Fork thread. Any exception except `AsyncException` in forked
--   thread is forwarded to original thread. When parent thread dies
--   child thread is killed too
forkLinkedIO :: (MonadIO m, MonadMask m)
             => IO a             -- ^ Action to execute in forked thread
             -> m b              -- ^ What to do while thread executes
             -> m b
forkLinkedIO action io = do
  tid <- liftIO myThreadId
  -- NOTE: Resource in bracket is acquired with asynchronous
  --       exceptions masked so child thread inherits masked state and
  --       needs to be explicitly unmasked
  bracket
    (liftIO $ forkWithUnmask $ \restore ->
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
-- Dynamic thread pools
----------------------------------------------------------------

-- | Opaque handler for threads. It could be used to create new
--   threads using either 'newSheep' or 'newSheepFinally'
data Shepherd = Shepherd (TVar (Map.Map ThreadId (MVar ())))

-- | Create shepherd object that will ensure that 
withShepherd :: (MonadMask m, MonadIO m) => (Shepherd -> m a) -> m a
withShepherd
  = bracket (liftIO $ Shepherd <$> newTVarIO Map.empty)
            (liftIO . cleanup)
  where
    -- Cleanup routine which:
    --  1. Ensures that no thread left running
    --  2. We continue execution only after all threads terminated
    cleanup (Shepherd tv) = do
      children <- readTVarIO tv
      -- Kill children
      sacrifice $ Map.keys children
      -- Wait until all threads terminate
      forM_ children takeMVar
    -- Sacrifice all threads from herd to gods of program
    -- stability. Weq need to take to special care to ensure that
    -- _all_ threads are terminated. Despite being run under mask we
    -- can receive asynchronous exception when calling killThread.  If
    -- we do we receive exception we continue to kill remaining
    -- children in separate thread.
    sacrifice []          = return ()
    sacrifice tAll@(t:ts) = do
      killThread t `onException` Conc.forkIO (mapM_ killThread tAll)
      sacrifice ts

-- | Create new thread which will be terminated when we exit
--   corresponding 'withShepherd' block
newSheep
  :: (MonadFork m, MonadIO m, MonadMask m)
  => Shepherd -> m () -> m ()
newSheep shepherd action = do
  lock <- liftIO newEmptyMVar
  mask $ \restore -> do
    tid <- fork $ restore action `finally` liftIO (putMVar lock ())
    liftIO $ addSheep shepherd tid lock

-- | Create new thread which will be terminated when we exit
--   corresponding 'withShepherd' block. Finalizer action will be
--   called in any case
newSheepFinally
  :: (MonadFork m, MonadIO m, MonadMask m)
  => Shepherd -> m () -> m () -> m ()
newSheepFinally shepherd action fini = do
  lock <- liftIO newEmptyMVar
  mask $ \restore -> do
    tid <- fork $ (restore action `finally` fini) `finally` liftIO (putMVar lock ())
    liftIO $ addSheep shepherd tid lock

addSheep :: Shepherd -> ThreadId -> MVar () -> IO ()
addSheep (Shepherd tv) tid lock = do
  atomically $ modifyTVar' tv $ Map.insert tid lock

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
--
----------------------------------------------------------------

throwNothing :: (Exception e, MonadThrow m) => e -> Maybe a -> m a
throwNothing e = maybe (throwM e) pure

throwNothingM :: (Exception e, MonadThrow m) => e -> m (Maybe a) -> m a
throwNothingM e m = m >>= throwNothing e

throwLeft :: (Exception e, MonadThrow m) => Either e a -> m a
throwLeft = either throwM pure

throwLeftM :: (Exception e, MonadThrow m) => m (Either e a) -> m a
throwLeftM m = m >>= throwLeft


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

atomicallyIO :: MonadIO m => STM a -> m a
atomicallyIO = liftIO . atomically
