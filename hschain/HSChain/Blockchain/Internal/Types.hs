{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE DerivingStrategies   #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
-- |
module HSChain.Blockchain.Internal.Types (
    -- * Vote sets
    VoteSet
  , newVoteSet
  , HeightVoteSet
  , newHeightVoteSet
    -- * State of tendermint consensus
  , ProposalState(..)
  , TMState(..)
  ) where

import GHC.Generics           (Generic)
import qualified Data.Aeson          as JSON
import           Data.Map              (Map)
import qualified Data.HashMap.Strict as HM
import qualified Katip

import HSChain.Crypto
import HSChain.Crypto.Containers
import HSChain.Store.Internal.Proposals
import HSChain.Types.Blockchain
import HSChain.Types.Validators
import HSChain.Internal.Types.Consensus

----------------------------------------------------------------
-- Vote sets
----------------------------------------------------------------

type VoteSet ty a = SignedSet (Alg a) (Vote ty a) (Maybe (BlockID a))

type HeightVoteSet ty a = SignedSetMap Round (Alg a) (Vote ty a) (Maybe (BlockID a))

-- | Create new empty vote set
newVoteSet :: ValidatorSet (Alg a) -> VoteSet ty a
newVoteSet valSet = emptySignedSet valSet voteBlockID

-- | Create new empty vote set
newHeightVoteSet :: ValidatorSet (Alg a) -> HeightVoteSet ty a
newHeightVoteSet valSet = emptySignedSetMap valSet voteBlockID

----------------------------------------------------------------
-- State for of tendermint state machine
----------------------------------------------------------------

-- | Proposal state as seen by consensus algorithm
data ProposalState
  = GoodProposal
    -- ^ Proposal is valid and we could vote for it
  | InvalidProposal !JSON.Value
    -- ^ Proposal is invalid for some reason
  | UnseenProposal
    -- ^ We don't have complete block data for particular block ID yet
  deriving stock    (Show, Eq, Generic)
  deriving anyclass (JSON.FromJSON, JSON.ToJSON)

instance Katip.ToObject ProposalState where
  toObject p = HM.singleton "val" (JSON.toJSON p)
instance Katip.LogItem ProposalState where
  payloadKeys _ _ = Katip.AllKeys


-- | State for tendermint consensus at some particular height.
data TMState view = TMState
  { smRound          :: !Round
    -- ^ Current round
  , smStep           :: !Step
    -- ^ Current step in the round
  , smProposals      :: !(Map Round (Signed 'Verified (AlgOf view) (Proposal (BlockType view))))
    -- ^ Proposal for current round
  , smProposedBlocks :: !(Props view)
    -- ^ Proposed blocks and their validation state
  , smPrevotesSet    :: !(HeightVoteSet 'PreVote (BlockType view))
    -- ^ Set of all received valid prevotes
  , smPrecommitsSet  :: !(HeightVoteSet 'PreCommit (BlockType view))
    -- ^ Set of all received valid precommits
  , smLockedBlock    :: !(Maybe (Round, BlockIdOf view))
    -- ^ Round and block we're locked on
  , smLastCommit     :: !(Maybe (Commit (BlockType view)))                   -- TODO try strict Maybe
    -- ^ Commit for previous block. Nothing if previous block is
    --   genesis block.
  }
