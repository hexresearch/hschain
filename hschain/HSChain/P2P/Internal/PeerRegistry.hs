{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module HSChain.P2P.Internal.PeerRegistry
  ( PeerRegistry
  , newPeerRegistry
  , withPeer
  , knownAddresses
  , knownAddressesSTM
  , connectedAddresses
  , connectedAddressesSTM
  , selfAddresses
  , addAddresses
  , addSelfAddress
  ) where

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
import HSChain.Network.Types
import HSChain.P2P.Types (NetworkError(..))


----------------------------------------------------------------
-- Peer registry
----------------------------------------------------------------

-- | Data structure that tracks set of known and connected peers
data PeerRegistry = PeerRegistry
  { prTidMap        :: !(TVar (Map ThreadId NetAddr))
    -- ^ Threads that process connection to address
  , prConnected     :: !(TVar (Set NetAddr))
    -- ^ Connected addresses
  , prKnownAddreses :: !(TVar (Set NetAddr))
    -- ^ New addresses to connect
  , prSelfAddresses :: !(TVar (Set NetAddr))
    -- ^ Set of our addresses.
  }

-- | Create new empty and active registry
newPeerRegistry :: MonadIO m => m PeerRegistry
newPeerRegistry = liftIO $ do
  prTidMap        <- newTVarIO Map.empty
  prConnected     <- newTVarIO Set.empty
  prKnownAddreses <- newTVarIO Set.empty
  prSelfAddresses <- newTVarIO Set.empty
  return PeerRegistry{..}

-- | Return set of known addresses
knownAddresses :: MonadIO m => PeerRegistry -> m (Set NetAddr)
knownAddresses = liftIO . readTVarIO . prKnownAddreses

-- | Return set of known addresses
knownAddressesSTM :: PeerRegistry -> STM (Set NetAddr)
knownAddressesSTM = readTVar . prKnownAddreses

-- | Return set of addresses to which we're connected
connectedAddresses :: MonadIO m => PeerRegistry -> m (Set NetAddr)
connectedAddresses = liftIO . readTVarIO . prConnected

-- | Return set of addresses to which we're connected
connectedAddressesSTM :: PeerRegistry -> STM (Set NetAddr)
connectedAddressesSTM = readTVar . prConnected

-- | Set of our own addresses
selfAddresses :: MonadIO m => PeerRegistry -> m (Set NetAddr)
selfAddresses = liftIO . readTVarIO . prSelfAddresses

-- | Add more addresses to the registry
addAddresses :: PeerRegistry -> [NetAddr] -> STM ()
addAddresses PeerRegistry{..} addrs = do
  modifyTVar' prKnownAddreses (<> Set.fromList addrs)

addSelfAddress :: MonadIO m => PeerRegistry -> NetAddr -> m ()
addSelfAddress PeerRegistry{..} addr
  = atomicallyIO
  $ modifyTVar' prSelfAddresses $ Set.insert addr

-- | Register peer using current thread ID. Peer will be unregistered
--   on exit from this function.
--
--   This function will throw if we're alredy connected to this peer.
--   It only poses minor problem if both nodes try to connect to each
--   other simultaneously. In this case it's possible that both will
--   detect duplicate connection and will close connection from their
--   side. It doesn't considered big problem since nodes will try to
--   connect this or another node later.
withPeer :: (MonadMask m, MonadLogger m, MonadIO m)
         => PeerRegistry -> NetAddr -> m () -> m ()
withPeer PeerRegistry{..} addr action = do
  tid <- liftIO myThreadId
  mask $ \restore -> do
    conns <- atomicallyIO $ registerPeer tid
    restore (do logger DebugS "peer registry update" (sl "conns" conns)
                action
            )
      `finally`
      atomicallyIO (unregisterPeer tid)
  where
    -- Add peer to registry while detecting duplicate connections
    registerPeer tid = do
      addrs <- readTVar prConnected
      case addr `Set.member` addrs of
        True  -> throwSTM ConnectionLoop
        False -> do
          modifyTVar' prTidMap    $ Map.insert tid addr
          modifyTVar' prConnected $ Set.insert addr
          readTVar prConnected
    -- Remove peer from registry
    unregisterPeer tid = do
      tids <- readTVar prTidMap
      case tid `Map.lookup` tids of
        Nothing -> return ()
        Just a  -> do modifyTVar' prTidMap    $ Map.delete tid
                      modifyTVar' prConnected $ Set.delete a
