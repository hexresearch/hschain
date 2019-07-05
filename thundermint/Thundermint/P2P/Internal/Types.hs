{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE UndecidableInstances #-}

module Thundermint.P2P.Internal.Types where

import Codec.Serialise        (Serialise)
import Control.Concurrent.STM (STM, TChan)
import GHC.Generics           (Generic)

import Thundermint.Blockchain.Internal.Engine.Types (NetworkCfg)
import Thundermint.Blockchain.Internal.Types        (Announcement, MessageRx, TMState)
import Thundermint.Crypto                           (Crypto, Signed, SignedState(..))
import Thundermint.P2P.Types                        (NetAddr)
import Thundermint.Store                            (ProposalStorage)
import Thundermint.Store.Internal.Query             (Access(..))

import Thundermint.Types.Blockchain
import Thundermint.Types.Validators

import Thundermint.P2P.Internal.Logging
import Thundermint.P2P.PeerState

import qualified Katip

-- | Messages which peers exchange with each other
--
--   FIXME: We don't have good way to prevent DoS by spamming too much
--          data
data GossipMsg alg a
  = GossipPreVote   !(Signed (ValidatorIdx alg) 'Unverified alg (Vote 'PreVote   alg a))
  | GossipPreCommit !(Signed (ValidatorIdx alg) 'Unverified alg (Vote 'PreCommit alg a))
  | GossipProposal  !(Signed (ValidatorIdx alg) 'Unverified alg (Proposal alg a))
  | GossipBlock     !(Block alg a)
  | GossipAnn       !(Announcement alg)
  | GossipTx        !(TX a)
  | GossipPex       !PexMessage
  deriving (Generic)
deriving instance (Show a, Show (TX a), Crypto alg) => Show (GossipMsg alg a)
instance (Serialise (TX a), Serialise a, Crypto alg) => Serialise (GossipMsg alg a)

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
data PeerChans m alg a = PeerChans
  { peerChanTx              :: !(TChan (Announcement alg))
    -- ^ Broadcast channel for outgoing messages
  , peerChanPex             :: !(TChan PexMessage)
    -- ^ Broadcast channel for outgoing PEX messages
  , peerChanPexNewAddresses :: !(TChan [NetAddr])
    -- ^ Channel for new addreses
  , peerChanRx              :: !(MessageRx 'Unverified alg a -> STM ())
    -- ^ STM action for sending message to main application
  , proposalStorage         :: !(ProposalStorage 'RO m alg a)
    -- ^ Read only access to storage of proposals
  , consensusState          :: !(STM (Maybe (Height, TMState alg a)))   -- TODO try strict Maybe and Tuple
    -- ^ Read only access to current state of consensus state machine
  , p2pConfig               :: !NetworkCfg

  , gossipCnts              :: !GossipCounters
  }

-- | Dump GossipMsg without (Show) constraints
--
showGossipMsg :: GossipMsg alg a -> Katip.LogStr
showGossipMsg (GossipPreVote _)   = "GossipPreVote {}"
showGossipMsg (GossipPreCommit _) = "GossipPreCommit {}"
showGossipMsg (GossipProposal _)  = "GossipProposal {}"
showGossipMsg (GossipBlock _)     = "GossipBlock {}"
showGossipMsg (GossipAnn ann)     = "GossipAnn { " <> Katip.showLS ann <> " }"
showGossipMsg (GossipTx _)        = "GossipTx {}"
showGossipMsg (GossipPex p)       = "GossipPex { " <> Katip.showLS p <> " }"

showPeerState :: PeerState alg a -> Katip.LogStr
showPeerState (Lagging _) = "Lagging {}"
showPeerState (Current _) = "Current {}"
showPeerState (Ahead fs)  = "Ahead { " <> Katip.showLS fs <> " }"
showPeerState Unknown     = "Unknown {}"