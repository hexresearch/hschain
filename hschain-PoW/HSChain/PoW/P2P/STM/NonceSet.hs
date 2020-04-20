-- |
-- 
module HSChain.PoW.P2P.STM.NonceSet
  ( NonceSet
  , newNonceSet
  , withHandshakeNonce
  , isSelfConnection
  ) where

import Control.Concurrent.STM
import Control.Monad.Catch
import Control.Monad.IO.Class
import System.Random
import Data.Set (Set)
import qualified Data.Set as Set

import HSChain.Control.Util
import HSChain.PoW.P2P.Types



newtype NonceSet = NonceSet (TVar (Set HandshakeNonce))

newNonceSet :: MonadIO m => m NonceSet
newNonceSet = NonceSet <$> liftIO (newTVarIO mempty)

withHandshakeNonce
  :: (MonadIO m, MonadMask m)
  => NonceSet
  -> (HandshakeNonce -> m a)
  -> m a
withHandshakeNonce (NonceSet tvNonces) action = do
  nonce <- HandshakeNonce <$> liftIO randomIO
  let ini  = do atomicallyIO $ modifyTVar' tvNonces $ Set.insert nonce
                return nonce
      fini = atomicallyIO . modifyTVar' tvNonces . Set.delete
  bracket ini fini action

-- | Returns true if nonce is among
isSelfConnection :: MonadIO m => NonceSet -> HandshakeNonce -> m Bool
isSelfConnection (NonceSet tvNonces) nonce = do
  nonceSet <- liftIO $ readTVarIO tvNonces
  return $! nonce `Set.member` nonceSet
