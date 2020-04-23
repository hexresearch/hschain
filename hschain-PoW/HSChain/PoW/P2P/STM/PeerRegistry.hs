{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- |
module HSChain.PoW.P2P.STM.PeerRegistry
  ( PeerRegistry
  , newPeerRegistry
  , withPeer
    -- *
  , knownPeers
  , connectedPeers
  , connectedPeersList
  , availableToConnect
  , addSelfAddress
  , addKnownAddresses
  ) where

import Control.Concurrent.STM
import Control.Monad
import Control.Monad.Catch
import Control.Monad.IO.Class
import Data.Map.Strict (Map)
import Data.Foldable   (toList,foldl')
-- import Data.Set        (Set)
import Data.Time       (UTCTime,getCurrentTime,addUTCTime)
import qualified Data.Map.Strict as Map
import qualified Data.Aeson      as JSON
import GHC.Generics (Generic)

-- import qualified Data.Set        as Set
import HSChain.Control.Util
import HSChain.Network.Types
import HSChain.PoW.Exceptions

----------------------------------------------------------------

-- | State of peer
data PeerState
  = KnownPeer       -- ^ We know about this address
  | Banned UTCTime  -- ^ Address is banned for some infraction
  | Connected       -- ^ We're connected to this peer
  | SelfAddress     -- ^ Our own address
  deriving stock    (Show,Eq,Generic)
  deriving anyclass (JSON.ToJSON, JSON.FromJSON)

-- | Peer registry
data PeerRegistry = PeerRegistry
  { peerRegistry :: TVar (Map NetAddr PeerState)
  }

newPeerRegistry :: MonadIO m => m PeerRegistry
newPeerRegistry = liftIO $ do
  peerRegistry <- newTVarIO Map.empty
  return PeerRegistry{..}

-- | Register peer.
withPeer
  :: (MonadMask m, MonadIO m)
  => PeerRegistry
  -> NetAddr
  -> m ()
  -> m ()
withPeer PeerRegistry{..} addr action
  = mask $ join . register
  where
    -- Attempt to add established connection to peer registry
    register restore = do
      now <- liftIO getCurrentTime
      atomicallyIO $ do
        peers <- readTVar peerRegistry
        case addr `Map.lookup` peers of
          Nothing          -> continue
          Just KnownPeer   -> continue
          Just SelfAddress -> abort
          Just Connected   -> abort
          Just (Banned t)
            | t > now      -> abort
            | otherwise    -> continue
          where
            continue = runPeer restore
            abort    = return (return ())
    -- Peer is not banned and we aren't connected to peer. Proceed
    -- normally
    runPeer restore = do
      setPeerState Connected
      return $ do
        try (restore action) >>= \case
          Left e | Just (_::PeerError) <- fromException e -> banPeer
                 | otherwise                              -> unregister
          Right ()                                        -> unregister
    -- Ban peer. For now we just ban for one hour. Beu really we
    -- should change ban duration on basis of offence
    banPeer    = do
      now <- liftIO getCurrentTime
      atomicallyIO $ setPeerState $ Banned (addUTCTime 3600 now)
    -- We ended communications for whatever reason but did it
    -- normally. Simply mark peer as known.
    unregister = do
      atomicallyIO $ setPeerState KnownPeer
    --
    setPeerState = modifyTVar' peerRegistry . Map.insert addr


knownPeers :: PeerRegistry -> STM Int
knownPeers (PeerRegistry v) = do
  m <- readTVar v
  return $ length $ filter ok $ toList m
  where
    ok Connected   = True
    ok KnownPeer   = True
    ok Banned{}    = False
    ok SelfAddress = False

connectedPeers :: PeerRegistry -> STM Int
connectedPeers (PeerRegistry v) = do
  m <- readTVar v
  return $ length [ () | Connected <- toList m ]

connectedPeersList :: PeerRegistry -> STM [NetAddr]
connectedPeersList (PeerRegistry v) = do
  m <- readTVar v
  return [ a | (a,Connected) <- Map.toList m ]

availableToConnect :: PeerRegistry -> STM [NetAddr]
availableToConnect (PeerRegistry v) = do
  m <- readTVar v
  return [ a | (a, KnownPeer) <- Map.toList m ]

addSelfAddress :: PeerRegistry -> NetAddr -> STM ()
addSelfAddress (PeerRegistry v) addr = do
  modifyTVar' v $ Map.insert addr SelfAddress

addKnownAddresses :: PeerRegistry -> [NetAddr] -> STM ()
addKnownAddresses (PeerRegistry v) addrs =
  modifyTVar' v $ \m0 -> foldl' (\m a -> Map.alter add a m) m0 addrs
  where
    add Nothing = Just KnownPeer
    add x       = x
