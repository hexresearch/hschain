{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DerivingStrategies   #-}
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

import Control.Lens
import Control.Concurrent.STM (STM)
import Data.Map               (Map)
import Data.Set               (Set)

import HSChain.Blockchain.Internal.Types
import HSChain.Crypto
import HSChain.Internal.Types.Messages
import HSChain.Types.Blockchain
import HSChain.Types.Validators
import HSChain.P2P.Internal.Types


-- | State of peer which is lagging behind us. In this case we only
--   interested in precommits which are part of commit justifying next
--   block and whether it have proposal for commit round and block for
--   that round.
data LaggingState a = LaggingState
  { _lagPeerStep        :: !FullStep               -- ^ Step of peer
  , _lagPeerCommitR     :: !Round                  -- ^ Round when block was commited
  , _lagPeerValidators  :: !(ValidatorSet (Alg a)) -- ^ Set of validators for peer's height
  , _lagPeerPrecommits  :: !ValidatorISet          -- ^ Precommits that peer have
  , _lagPeerHasProposal :: !Bool                   -- ^ Whether peer have proposal
  , _lagPeerHasBlock    :: !Bool                   -- ^ Whether peer have block
  , _lagPeerBlockID     :: !(BlockID a)            -- ^ ID of commited block
  }
deriving stock instance (Crypto (Alg a)) => Show (LaggingState a)
makeLenses ''LaggingState

-- | Peer which is at the same height as we. Here state is more
--   complicated and somewhat redundant. Tendermint only tracks votes
--   for peer's round. For algorithm simplicity we track
data CurrentState a = CurrentState
  { _peerStep       :: !FullStep                  -- ^ Step of peer
  , _peerValidators :: !(ValidatorSet (Alg a))    -- ^
  , _peerPrevotes   :: !(Map Round ValidatorISet) -- ^ Peer's prevotes
  , _peerPrecommits :: !(Map Round ValidatorISet) -- ^ Peer's precommits
  , _peerProposals  :: !(Set Round)               -- ^ Set of proposals peer has
  , _peerBlocks     :: !(Set (BlockID a))         -- ^ Set of blocks for proposals
  , _peerLock       :: !(Maybe Round)             -- ^ Round peer locked on
  }
deriving stock instance (Crypto (Alg a)) => Show (CurrentState a)
deriving stock instance Eq (PublicKey (Alg a)) => Eq (CurrentState a)
makeLenses ''CurrentState

-- | State of peer that's ahead of us. We can't send anything of use
--   to it so we only track its step
newtype AheadState a = AheadState { _aheadPeerStep :: FullStep }
  deriving (Show, Eq)
makeLenses ''AheadState

data UnknownState a = UnknownState
  deriving Show

-- | State of a peer.
data State a where
  Lagging :: LaggingState a -> State a
  Current :: CurrentState a -> State a
  Ahead   :: AheadState   a -> State a
  Unknown :: UnknownState a -> State a

class Wrapable s where
    wrap :: s a -> State a

instance Wrapable LaggingState where
    wrap = Lagging

instance Wrapable CurrentState where
    wrap = Current

instance Wrapable AheadState where
    wrap = Ahead

instance Wrapable UnknownState where
    wrap = Unknown

data Command a
  = SendRX !(MessageRx 'Unverified a)
  | Push2Mempool !(TX a)
  | Push2Gossip !(GossipMsg a)
  | SendPEX !PexMessage

data GossipTimeout
  = TimeoutProposal
  | TimeoutPrevote
  | TimeoutPrecommit
  | TimeoutBlock
  | TimeoutAnnounce
  deriving Show

newtype Config view = Config
  { _consensusSt :: STM (Maybe (Height, TMState view))
  }
makeLenses ''Config

deriving stock instance (Show a, Crypto (Alg a), Show (TX a)) => Show (Command a)

