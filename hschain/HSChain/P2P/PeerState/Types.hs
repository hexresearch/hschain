{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE InstanceSigs         #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

module HSChain.P2P.PeerState.Types where

import Control.Concurrent.STM (STM)
import Data.Map               (Map)
import Data.Set               (Set)

import Lens.Micro.TH

import HSChain.Blockchain.Internal.Types
import HSChain.Crypto
import HSChain.Store
import HSChain.Types.Blockchain
import HSChain.Types.Validators

import HSChain.P2P.Internal.Types
import HSChain.P2P.Internal.Logging (GossipCounters(..))

-- | State of peer which is lagging behind us. In this case we only
--   interested in precommits which are part of commit justifying next
--   block and whether it have proposal for commit round and block for
--   that round.
data LaggingState alg a = LaggingState
  { _lagPeerStep        :: !FullStep              -- ^ Step of peer
  , _lagPeerCommitR     :: !Round                 -- ^ Round when block was commited
  , _lagPeerValidators  :: !(ValidatorSet alg)    -- ^ Set of validators for peer's height
  , _lagPeerPrecommits  :: !ValidatorISet         -- ^ Precommits that peer have
  , _lagPeerHasProposal :: !Bool                  -- ^ Whether peer have proposal
  , _lagPeerHasBlock    :: !Bool                  -- ^ Whether peer have block
  , _lagPeerBlockID     :: !(BlockID alg a)       -- ^ ID of commited block
  }
  deriving Show
makeLenses ''LaggingState

-- | Peer which is at the same height as we. Here state is more
--   complicated and somewhat redundant. Tendermint only tracks votes
--   for peer's round. For algorithm simplicity we track
--
--   FIXME: simplify state along tern
data CurrentState alg a = CurrentState
  { _peerStep       :: !FullStep                  -- ^ Step of peer
  , _peerValidators :: !(ValidatorSet alg)        -- ^
  , _peerPrevotes   :: !(Map Round ValidatorISet) -- ^ Peer's prevotes
  , _peerPrecommits :: !(Map Round ValidatorISet) -- ^ Peer's precommits
  , _peerProposals  :: !(Set Round)               -- ^ Set of proposals peer has
  , _peerBlocks     :: !(Set (BlockID alg a))     -- ^ Set of blocks for proposals
  }
  deriving Show
deriving instance Eq   (PublicKey alg) => Eq   (CurrentState alg a)
makeLenses ''CurrentState

-- | State of peer that's ahead of us. We can't send anything of use
--   to it so we only track its step
newtype AheadState alg a = AheadState { _aheadPeerStep :: FullStep }
  deriving (Show, Eq)
makeLenses ''AheadState

data UnknownState alg a = UnknownState
  deriving Show

-- | State of a peer.
data State alg a where
  Lagging :: LaggingState alg a -> State alg a
  Current :: CurrentState alg a -> State alg a
  Ahead   :: AheadState   alg a -> State alg a
  Unknown :: UnknownState alg a -> State alg a

class Wrapable s where
    wrap :: s alg a -> State alg a

instance Wrapable LaggingState where
    wrap = Lagging

instance Wrapable CurrentState where
    wrap = Current

instance Wrapable AheadState where
    wrap = Ahead

instance Wrapable UnknownState where
    wrap = Unknown

data Command alg a
  = SendRX !(MessageRx 'Unverified alg a)
  | Push2Mempool !(TX a)
  | Push2Gossip !(GossipMsg alg a)
  | SendPEX !PexMessage

data Event alg a
  = EGossip !(GossipMsg alg a)
  | EMempoolTimeout
  | EVotesTimeout
  | EBlocksTimeout
  | EAnnounceTimeout
  | EAnnouncement !(MessageTx alg a)

data Config m alg a = Config { _propStorage    :: !(ProposalStorage 'RO m alg a)
                             , _mempCursor     :: !(MempoolCursor m alg (TX a))
                             , _consensusSt    :: !(STM (Maybe (Height, TMState alg a)))
                             , _gossipCounters :: !GossipCounters
                             }
makeLenses ''Config


deriving instance (Show a, Crypto alg, Show (TX a)) => Show (Command alg a)
deriving instance (Show a, Crypto alg, Show (TX a)) => Show (Event alg a)
