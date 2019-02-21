{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
-- |
module Thundermint.Blockchain.Internal.Types (
    MessageRx(..)
  , unverifyMessageRx
  , Announcement(..)
    -- * Vote sets
  , VoteSet
  , newVoteSet
  , HeightVoteSet
  , newHeightVoteSet
    -- * State of tendermint consensus
  , ProposalState(..)
  , TMState(..)
  ) where

import Codec.Serialise        (Serialise)
import GHC.Generics           (Generic)
import           Data.SafeCopy
import qualified Data.Aeson          as JSON
import           Data.Map              (Map)
import qualified Data.HashMap.Strict as HM
import qualified Katip

import Thundermint.Crypto
import Thundermint.Crypto.Containers
import Thundermint.Types.Blockchain
import Thundermint.Types.Validators


----------------------------------------------------------------
-- Vote sets
----------------------------------------------------------------

type VoteSet ty alg a = SignedSet 'Verified alg (Vote ty alg a) (Maybe (BlockID alg a))

type HeightVoteSet ty alg a = SignedSetMap Round 'Verified alg (Vote ty alg a) (Maybe (BlockID alg a))

-- | Create new empty vote set
newVoteSet :: ValidatorSet alg -> Time -> VoteSet ty alg a
newVoteSet valSet t = emptySignedSet valSet voteBlockID ((> t) . voteTime)

-- | Create new empty vote set
newHeightVoteSet :: ValidatorSet alg -> Time -> HeightVoteSet ty alg a
newHeightVoteSet valSet t = emptySignedSetMap valSet voteBlockID ((> t) . voteTime)

----------------------------------------------------------------
-- State for of tendermint state machine
----------------------------------------------------------------

-- | Proposal state as seen by consensus algorithm
data ProposalState
  = GoodProposal
    -- ^ Proposal is valid and we could vote for it
  | InvalidProposal
    -- ^ Proposal is invalid for some reason
  | UnseenProposal
    -- ^ We don't have complete block data for particular block ID yet
  deriving (Show, Eq, Generic)
instance Serialise     ProposalState
instance JSON.ToJSON   ProposalState
instance JSON.FromJSON ProposalState

-- | State for tendermint consensus at some particular height.
data TMState alg a = TMState
  { smRound         :: !Round
    -- ^ Current round
  , smStep          :: !Step
    -- ^ Current step in the round
  , smProposals     :: !(Map Round (Signed 'Verified alg (Proposal alg a)))
    -- ^ Proposal for current round
  , smPrevotesSet   :: !(HeightVoteSet 'PreVote alg a)
    -- ^ Set of all received valid prevotes
  , smPrecommitsSet :: !(HeightVoteSet 'PreCommit alg a)
    -- ^ Set of all received valid precommits
  , smLockedBlock   :: !(Maybe (Round, BlockID alg a))
    -- ^ Round and block we're locked on
  , smLastCommit    :: !(Maybe (Commit alg a))
    -- ^ Commit for previous block. Nothing if previous block is
    --   genesis block.
  }
  deriving (Show)

----------------------------------------------------------------
-- Messages
----------------------------------------------------------------

-- | Message received by main application
data MessageRx ty alg a
  = RxPreVote   !(Signed ty alg (Vote 'PreVote   alg a))
  | RxPreCommit !(Signed ty alg (Vote 'PreCommit alg a))
  | RxProposal  !(Signed ty alg (Proposal alg a))
  | RxTimeout   !Timeout
  | RxBlock     !(Pet (Block alg a))
  deriving (Show, Generic)
instance (SafeCopy a, Crypto alg) => SafeCopy (MessageRx 'Unverified alg a)

unverifyMessageRx :: MessageRx 'Verified alg a -> MessageRx 'Unverified alg a
unverifyMessageRx = \case
  RxPreVote   s -> RxPreVote   (unverifySignature s)
  RxPreCommit s -> RxPreCommit (unverifySignature s)
  RxProposal  s -> RxProposal  (unverifySignature s)
  RxTimeout   t -> RxTimeout   t
  RxBlock     b -> RxBlock     b


-- | Messages which should be delivered to peers immediately. Those
--   are control messages in gossip protocol. Actual proposals, votes
--   and blocks are delivered pure via gossip.
data Announcement alg
  = AnnStep         !FullStep
  | AnnHasProposal  !Height !Round
  | AnnHasBlock     !Height !Round
  | AnnHasPreVote   !Height !Round !(ValidatorIdx alg)
  | AnnHasPreCommit !Height !Round !(ValidatorIdx alg)
  deriving (Show,Generic)
instance SafeCopy (Announcement alg)

instance Katip.ToObject ProposalState where
  toObject p = HM.singleton "val" (JSON.toJSON p)
instance Katip.LogItem ProposalState where
  payloadKeys _ _ = Katip.AllKeys
