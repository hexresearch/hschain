{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module HSChain.P2P.Internal.PeerRegistry where

import Control.Concurrent     (ThreadId, myThreadId)
import Control.Concurrent.STM
import Control.Monad.Catch    (MonadMask, finally, mask)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Map.Strict        (Map)
import Data.Set               (Set)
import Katip                  (sl)

import qualified Data.Map.Strict as Map
import qualified Data.Set        as Set

import HSChain.Control (atomicallyIO)
import HSChain.Logger
import HSChain.P2P.Types


----------------------------------------------------------------
-- Peer registry
----------------------------------------------------------------

-- Set of currently running peers
data PeerRegistry = PeerRegistry
    { prTidMap        :: !(TVar (Map ThreadId NetAddr))
      -- ^ Threads that process connection to address
    , prConnected     :: !(TVar (Set NetAddr))
      -- ^ Connected addresses
    , prKnownAddreses :: !(TVar (Set NetAddr))
      -- ^ New addresses to connect
    }

-- | Create new empty and active registry
newPeerRegistry :: MonadIO m => m PeerRegistry
newPeerRegistry = do
  prTidMap        <- liftIO (newTVarIO Map.empty)
  prConnected     <- liftIO (newTVarIO Set.empty)
  prKnownAddreses <- liftIO (newTVarIO Set.empty)
  return PeerRegistry{..}


-- | Register peer using current thread ID. If we already have
--   registered peer with given address do nothing
withPeer :: (MonadMask m, MonadLogger m, MonadIO m)
         => PeerRegistry -> NetAddr -> m () -> m ()
withPeer PeerRegistry{..} addr action = do
  tid <- liftIO myThreadId
  logger DebugS "withPeer" ("addr" `sl` addr)
  mask $ \restore -> do
    atomicallyIO $ registerPeer tid
    restore action
      `finally`
      atomicallyIO (unregisterPeer tid)
  where
    -- Add peer to registry and return whether it was success
    registerPeer tid = do
      addrs <- readTVar prConnected
      case addr `Set.member` addrs of
        True  -> throwSTM ConnectionLoop
        False -> do
          modifyTVar' prTidMap    $ Map.insert tid addr
          modifyTVar' prConnected $ Set.insert addr
    -- Remove peer from registry
    unregisterPeer tid = do
      tids <- readTVar prTidMap
      case tid `Map.lookup` tids of
        Nothing -> return ()
        Just a  -> do modifyTVar' prTidMap    $ Map.delete tid
                      modifyTVar' prConnected $ Set.delete a
                      return ()

    -- logUnregister = logger DebugS ("withPeer: unregister " <> showLS addr) ()
