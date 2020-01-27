{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module HSChain.P2P.Internal.PeerRegistry where

import Control.Concurrent     (ThreadId, killThread, myThreadId)
import Control.Concurrent.STM
    (TVar, atomically, modifyTVar', newTVarIO, readTVar, retry, writeTVar)
import Control.Monad          (when)
import Control.Monad.Catch    (MonadMask, finally, uninterruptibleMask)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Map.Strict        (Map)
import Data.Monoid            ((<>))
import Data.Set               (Set)
import Katip                  (showLS,sl)

import qualified Data.Map.Strict as Map
import qualified Data.Set        as Set

import HSChain.Debug.Trace
import HSChain.Logger
import HSChain.P2P.Types

data ConnectMode = CmAccept !PeerId
                 | CmConnect
                 deriving Show

-- Set of currently running peers
data PeerRegistry = PeerRegistry
    { prTidMap        :: !(TVar (Map ThreadId NetAddr))
      -- ^ Threads that process connection to address
    , prConnected     :: !(TVar (Set NetAddr))
      -- ^ Connected addresses
    , prKnownAddreses :: !(TVar (Set NetAddr))
      -- ^ New addresses to connect
    , prPeerId        :: !PeerId
      -- ^ Unique peer id for controlling simultaneous connections
    }

-- | Create new empty and active registry
newPeerRegistry :: MonadIO m => PeerId -> m PeerRegistry
newPeerRegistry pid = PeerRegistry
               <$> liftIO (newTVarIO Map.empty)
               <*> liftIO (newTVarIO Set.empty)
               <*> liftIO (newTVarIO Set.empty)
               <*> return pid

-- | Register peer using current thread ID. If we already have
--   registered peer with given address do nothing
withPeer :: (MonadMask m, MonadLogger m, MonadIO m, MonadTrace m)
         => PeerRegistry -> NetAddr -> ConnectMode -> m () -> m ()
withPeer PeerRegistry{..} addr connMode action = do
  tid <- liftIO myThreadId
  logger DebugS "withPeer"
    (  "addr"     `sl` addr
    <> "peerID"   `sl` prPeerId
    <> "connMode" `sl` show connMode
    )
  -- NOTE: we need uninterruptibleMask since we STM operation are
  --       blocking and they must not be interrupted
  uninterruptibleMask $ \restore -> do
    (ok, addrs) <- liftIO $ atomically $ registerPeer tid
    when ok $
        restore (tracePRChange addrs >> action)
        `finally`
        (logUnregister >> liftIO (atomically (unregisterPeer tid)) >>= tracePRChange)
  where
    tracePRChange addrs = trace $ TePeerRegistryChanged (Set.map show addrs)
    -- Add peer to registry and return whether it was success
    registerPeer tid = do
        addrs <- readTVar prConnected
        if addr `Set.member` addrs then
          case connMode of
            CmConnect -> return (False, addrs)
            CmAccept otherPeerId ->
              -- Something terrible happened: mutual connection!
              -- So we compare peerId-s: lesser let greater have connection.
              if prPeerId > otherPeerId then
                -- Wait for closing connection on other side
                -- and release addr from addrs.
                retry
              else
                -- Deny connection on this size
                -- (for releasing addr from addrs on other side).
                return (False, addrs)
        else do
          modifyTVar' prTidMap    $ Map.insert tid addr
          modifyTVar' prConnected $ Set.insert addr
          addrs' <- readTVar prConnected
          return (True, addrs')
    -- Remove peer from registry
    unregisterPeer tid = do
                  tids <- readTVar prTidMap
                  case tid `Map.lookup` tids of
                    Nothing -> return Set.empty
                    Just a  -> do modifyTVar' prTidMap    $ Map.delete tid
                                  modifyTVar' prConnected $ Set.delete a
                                  readTVar prConnected
    logUnregister = logger DebugS ("withPeer: unregister " <> showLS addr) ()
