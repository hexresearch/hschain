{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE UndecidableInstances #-}

module HSChain.P2P.Internal.Types where

import Codec.Serialise        (Serialise)
import Control.Concurrent.STM (STM, TChan)
import GHC.Generics           (Generic)

import HSChain.Blockchain.Internal.Engine.Types (NetworkCfg)
import HSChain.Blockchain.Internal.Types        (Announcement, MessageTx, MessageRx, TMState)
import HSChain.Crypto                           (Crypto, SignedState(..), CryptoHashable(..))
import HSChain.P2P.Types                        (NetAddr)

import HSChain.Types

import HSChain.P2P.Internal.Logging

import qualified Katip


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
  | PexPing
  -- ^ Message to estimate connection speed between peers
  | PexPong
  -- ^ Answer for Ping
  deriving (Show, Generic)

instance Serialise PexMessage

--
-- | Connection handed to process controlling communication with peer
data PeerChans a = PeerChans
  { peerChanTx              :: !(TChan (MessageTx a))
    -- ^ Broadcast channel for outgoing messages
  , peerChanPex             :: !(TChan PexMessage)
    -- ^ Broadcast channel for outgoing PEX messages
  , peerChanPexNewAddresses :: !(TChan [NetAddr])
    -- ^ Channel for new addreses
  , peerChanRx              :: !(MessageRx 'Unverified a -> STM ())
    -- ^ STM action for sending message to main application
  , consensusState          :: !(STM (Maybe (Height, TMState a)))   -- TODO try strict Maybe and Tuple
    -- ^ Read only access to current state of consensus state machine
  , p2pConfig               :: !NetworkCfg

  , gossipCnts              :: !GossipCounters
  }

-- | Dump GossipMsg without (Show) constraints
--
showGossipMsg :: GossipMsg a -> Katip.LogStr
showGossipMsg (GossipPreVote _)   = "GossipPreVote {}"
showGossipMsg (GossipPreCommit _) = "GossipPreCommit {}"
showGossipMsg (GossipProposal _)  = "GossipProposal {}"
showGossipMsg (GossipBlock _)     = "GossipBlock {}"
showGossipMsg (GossipAnn ann)     = "GossipAnn { " <> Katip.showLS ann <> " }"
showGossipMsg (GossipTx _)        = "GossipTx {}"
showGossipMsg (GossipPex p)       = "GossipPex { " <> Katip.showLS p <> " }"

