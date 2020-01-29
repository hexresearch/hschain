{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE UndecidableInstances       #-}

module HSChain.P2P.Internal.Types where

import Codec.Serialise        (Serialise)
import Control.Concurrent.STM
import Control.Monad.Catch
import Control.Monad.IO.Class
import Data.Word
import Data.Set               (Set)
import qualified Data.Set        as Set
import System.Random          (randomIO)
import GHC.Generics           (Generic)

import HSChain.Control                          (Shepherd,atomicallyIO)
import HSChain.Blockchain.Internal.Engine.Types (NetworkCfg)
import HSChain.Blockchain.Internal.Types        (Announcement, MessageTx, MessageRx, TMState)
import HSChain.Crypto                           (Crypto, SignedState(..), CryptoHashable(..))
import HSChain.P2P.Types                        (NetAddr)
import HSChain.Types
import HSChain.P2P.Internal.PeerRegistry


-- | Random nonce which is used to detect self-connections
newtype GossipNonce = GossipNonce Word64
  deriving newtype (Show,Eq,Ord,Serialise)

-- | Message sent by node initiating connection
data GossipHello = GossipHello !GossipNonce !Word16
  deriving stock    (Show,Eq,Generic)
  deriving anyclass (Serialise)

-- | Message sent as reply to GossipHello
data GossipAck = GossipAck
  deriving stock    (Show,Eq,Generic)
  deriving anyclass (Serialise)


-- | Messages which peers exchange with each other
--
--   FIXME: We don't have good way to prevent DoS by spamming too much
--          data
data GossipMsg a
  = GossipPreVote   !(Signed 'Unverified (Alg a) (Vote 'PreVote   a))
  | GossipPreCommit !(Signed 'Unverified (Alg a) (Vote 'PreCommit a))
  | GossipProposal  !(Signed 'Unverified (Alg a) (Proposal a))
  | GossipBlock     !(Block a)
  | GossipAnn       !(Announcement (Alg a))
  | GossipTx        !(TX a)
  | GossipPex       !PexMessage
  deriving (Generic)
deriving instance (Show a, Show (TX a), Crypto (Alg a)) => Show (GossipMsg a)
instance (Serialise (TX a), Serialise a, CryptoHashable a, Crypto (Alg a)) => Serialise (GossipMsg a)

--
-- | Peer exchage gossip sub-message
--
data PexMessage
  = PexMsgAskForMorePeers
  -- ^ Peer need yet connections to peers
  | PexMsgMorePeers ![NetAddr]
  -- ^ Some addresses of other connected peers
  deriving (Show, Generic)

instance Serialise PexMessage

--
-- | Connection handed to process controlling communication with peer
data PeerChans a = PeerChans
  { peerChanTx              :: !(TChan (MessageTx a))
    -- ^ Broadcast channel for outgoing messages
  , peerChanRx              :: !(MessageRx 'Unverified a -> STM ())
    -- ^ STM action for sending message to main application
  , peerChanPex             :: !(TChan PexMessage)
    -- ^ Broadcast channel for outgoing PEX messages
  , consensusState          :: !(STM (Maybe (Height, TMState a)))   -- TODO try strict Maybe and Tuple
    -- ^ Read only access to current state of consensus state machine
  , p2pConfig               :: !NetworkCfg
  , peerShepherd            :: !Shepherd
  , peerNonceSet            :: !NonceSet
  , peerRegistry            :: !PeerRegistry
  }


----------------------------------------------------------------
-- Storage for nonces
----------------------------------------------------------------

newtype NonceSet = NonceSet (TVar (Set GossipNonce))

newNonceSet :: MonadIO m => m NonceSet
newNonceSet = NonceSet <$> liftIO (newTVarIO mempty)

withGossipNonce
  :: (MonadIO m, MonadMask m)
  => NonceSet
  -> (GossipNonce -> m a)
  -> m a
withGossipNonce (NonceSet tvNonces) action = do
  nonce <- GossipNonce <$> liftIO randomIO
  let ini  = do atomicallyIO $ modifyTVar' tvNonces $ Set.insert nonce
                return nonce
      fini = atomicallyIO . modifyTVar' tvNonces . Set.delete
  bracket ini fini action

-- | Returns true if nonce is among
isSelfConnection :: MonadIO m => NonceSet -> GossipNonce -> m Bool
isSelfConnection (NonceSet tvNonces) nonce = do
  nonceSet <- liftIO $ readTVarIO tvNonces
  return $! nonce `Set.member` nonceSet
