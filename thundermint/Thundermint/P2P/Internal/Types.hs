{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE StandaloneDeriving #-}

module Thundermint.P2P.Internal.Types where

import Codec.Serialise
import Control.Concurrent     (MVar, newMVar, readMVar)
import Control.Concurrent.STM
import Control.Monad.Catch
import Control.Monad.IO.Class (MonadIO, liftIO)
import GHC.Generics           (Generic)

import Thundermint.Blockchain.Internal.Engine.Types (NetworkCfg)
import Thundermint.Blockchain.Internal.Types        (Announcement, MessageRx, TMState)
import Thundermint.Control                          (modifyMVarM_)
import Thundermint.Crypto
import Thundermint.P2P.Types
import Thundermint.Store                            (ProposalStorage)
import Thundermint.Store.Internal.Query             (Access(..))
import Thundermint.Types.Blockchain                 (Height)

import Thundermint.Types.Blockchain
import Thundermint.Types.Validators
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

  , cntGossipPrevote        :: !Counter
  , cntGossipPrecommit      :: !Counter
  , cntGossipBlocks         :: !Counter
  , cntGossipProposals      :: !Counter
  , cntGossipTx             :: !Counter
  , cntGossipPex            :: !Counter
  }

-- | Counter for counting send/receive event
data Counter = Counter !(MVar Int) !(MVar Int)

newCounter :: MonadIO m => m Counter
newCounter = Counter <$> liftIO (newMVar 0) <*>liftIO (newMVar 0)

tickSend :: (MonadMask m, MonadIO m) => Counter -> m ()
tickSend (Counter s _) = modifyMVarM_ s (return . succ)

tickRecv :: (MonadMask m, MonadIO m) => Counter -> m ()
tickRecv (Counter _ r) = modifyMVarM_ r (return . succ)

readSend :: MonadIO m => Counter -> m Int
readSend (Counter s _) = liftIO $ readMVar s

readRecv :: MonadIO m => Counter -> m Int
readRecv (Counter _ r) = liftIO $ readMVar r

