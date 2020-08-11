-- |
module HSChain.Logger.Class where

import Control.Monad.Trans.Reader
import Control.Monad.Morph
import Control.Monad.Trans.Maybe             (MaybeT(..))
import Control.Monad.Trans.State.Strict as SS(StateT(..))
import Control.Monad.Trans.State.Lazy   as SL(StateT(..))
import Control.Monad.Trans.Except            (ExceptT(..))
import Control.Monad.Trans.Identity          (IdentityT(..))
import Data.Text                             (Text)
import Katip


-- | Monad which supports logging. Unlike 'Katip' it ties namespace to
--   the monad.
class Monad m => MonadLogger m where
  -- | Log value
  logger :: LogItem a => Severity -> LogStr -> a -> m ()
  -- | Change current namespace
  localNamespace :: (Namespace -> Namespace) -> m a -> m a

-- | Change logger's namespace
setNamespace :: MonadLogger m => Namespace -> m a -> m a
setNamespace nm = localNamespace (const nm)

-- | Append string to namespace
descendNamespace :: MonadLogger m => Text -> m a -> m a
descendNamespace nm = localNamespace $ \(Namespace x) -> Namespace (x ++ [nm])


instance MonadLogger m => MonadLogger (MaybeT m) where
  logger sev str a = lift $ logger sev str a
  localNamespace fun (MaybeT m) = MaybeT (localNamespace fun m)

instance MonadLogger m => MonadLogger (ReaderT r m) where
  logger sev str a = lift $ logger sev str a
  localNamespace fun (ReaderT m) = ReaderT $ localNamespace fun . m

instance MonadLogger m => MonadLogger (SS.StateT s m) where
  logger sev str a = lift $ logger sev str a
  localNamespace fun (SS.StateT m) = SS.StateT $ localNamespace fun . m

instance MonadLogger m => MonadLogger (SL.StateT s m) where
  logger sev str a = lift $ logger sev str a
  localNamespace fun (SL.StateT m) = SL.StateT $ localNamespace fun . m

instance MonadLogger m => MonadLogger (ExceptT e m) where
  logger sev str a = lift $ logger sev str a
  localNamespace fun (ExceptT m) = ExceptT $ localNamespace fun m

instance MonadLogger m => MonadLogger (IdentityT m) where
  logger sev str a = lift $ logger sev str a
  localNamespace fun (IdentityT m) = IdentityT $ localNamespace fun m
