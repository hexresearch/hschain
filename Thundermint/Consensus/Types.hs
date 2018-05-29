{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
-- |
-- Data types for implementation of consensus algorithm
module Thundermint.Consensus.Types (
    -- * Newtype wrappers
    Height(..)
  , Round(..)
  , Time(..)
  , Sequence(..)
    -- * Basic data types for blockchain
  , BlockID
  , Block(..)
  , Header(..)
  , Commit(..)
    -- * Data types for establishing consensus
  , Step(..)
  , Timeout(..)
  , Proposal(..)
    -- ** Votes
  , VoteType(..)
  , Vote(..)
  , VoteSet
  , HeightVoteSet
    -- * State of tendermint consensus
  , ProposalState(..)
  , TMState(..)
  ) where

import           Codec.Serialise
import           Data.ByteString   (ByteString)
import           Data.Map          (Map)
import           Data.Int
import GHC.Generics (Generic)

import Thundermint.Crypto
import Thundermint.Crypto.Containers

----------------------------------------------------------------
-- Primitives
----------------------------------------------------------------

newtype Height = Height Int64
  deriving (Show, Eq, Ord, Serialise)

newtype Round = Round Int64
  deriving (Show, Eq, Ord, Serialise)

newtype Time = Time Int64
  deriving (Show, Eq, Ord, Serialise)

class Sequence a where
  next :: a -> a
  rangeExclusive :: a -> a -> [a]

instance Sequence Height where
  next (Height n) = Height (succ n)
  rangeExclusive (Height n) (Height m) = [ Height i | i <- [n+1, m-1]]
instance Sequence Round where
  next (Round n) = Round (succ n)
  rangeExclusive (Round n) (Round m) = [ Round i | i <- [n+1, m-1]]


----------------------------------------------------------------
-- Blocks
----------------------------------------------------------------

-- | Block identified by hash
type BlockID alg a = BlockHash alg (Block alg a)

-- | Block data type
data Block alg a = Block
  { blockHeader     :: Header alg a
  , blockData       :: a
  , blockLastCommit :: Maybe (Commit alg a)
    -- ^ Commit information for previous block. Nothing iff block
    --   is a genesis block or block at height 1.
  }
  deriving (Show, Generic)
instance Serialise a => Serialise (Block alg a)

-- | Block header
data Header alg a = Header
  { headerChainID     :: ByteString -- ^ Identifier of chain we're working on
  , headerHeight      :: Height     -- ^ Height of block
  , headerTime        :: Time       -- ^ Time of block creation
  , headerLastBlockID :: Maybe (BlockID alg a)
    -- ^ Nothing iff block is a genesis block

  -- FIXME: Add various hashes
  -- , headerDataHash       :: Hash
  -- , headerValidatorsHash :: Hash
  -- , headerConsensusHash  :: Hash
  }
  deriving (Show,Generic)
instance Serialise (Header alg a)

-- | Data justifying commit
data Commit alg a = Commit
  { commitBlockID    :: BlockID alg a
    -- ^ Block for which commit is done
  , commitPrecommits :: [Signed 'Verified alg (Vote 'PreCommit alg a)]
    -- ^ List of precommits which justify commit
  }
  deriving (Show,Generic)
instance Serialise (Commit alg a)



----------------------------------------------------------------
-- Data types for establishing consensus
----------------------------------------------------------------

-- | Step of the algorithm
data Step
  = StepNewHeight
  | StepProposal
  | StepPrevote
  | StepPrecommit
  deriving (Show,Eq,Ord)

data Timeout = Timeout Height Round Step
  deriving (Show,Eq,Ord)

-- | Proposal for new block. Proposal include only hash of block and
--   block itself is gossiped separately.
data Proposal alg a = Proposal
  { propHeight    :: Height
    -- ^ Proposal height
  , propRound     :: Round
    -- ^ Propoasl round
  , propTimestamp :: Time
    -- ^ Time of proposal
  , propPOL       :: Maybe (Round, BlockID alg a)
    -- ^ Proof of Lock for proposal
    --
    -- FIXME: why it's needed? How should it be used?
  , propBlockID   :: BlockID alg a
    -- ^ Hash of proposed block
  }
  deriving (Show,Generic)

instance Serialise a => Serialise (Proposal alg a) where

-- | Type of vote. Used for type-tagging of votes
data VoteType = PreVote
              | PreCommit
              deriving (Show,Eq,Generic)

instance Serialise VoteType

-- | Single vote cast validator. Type of vote is determined by its
--   type tag
data Vote (ty :: VoteType) alg a= Vote
  { voteHeight     :: Height
  , voteRound      :: Round
  , voteTime       :: Time
  , voteBlockID    :: Maybe (BlockID alg a)
  }
  deriving (Show,Eq,Ord,Generic)


-- FIXME: We need to embed type tag into encoded data to disallow
--        spoofing prevote as precommit and vice-versa
--
instance Serialise (Vote ty alg a) where
-- instance Serialise (Vote 'PreVote   alg a) where
--   encode (Vote h r t bid) = encode (h,r,t,bid,
--   decode = undefined
-- instance Serialise (Vote 'PreCommit alg a) where
--   encode = undefined
--   decode = undefined

type VoteSet ty alg a = SignedSet 'Verified (Vote ty alg a)

type HeightVoteSet ty alg a = SignedSetMap Round 'Verified alg (Vote ty alg a)


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
  deriving (Show, Eq)

-- | State for tendermint consensus at some particular height.
data TMState alg a = TMState
  { smRound            :: Round
    -- ^ Current round
  , smStep             :: Step
    -- ^ Current step in the round
  , smProposals        :: Map Round (Signed 'Verified alg (Proposal alg a))
    -- ^ Proposal for current round
  , smPrevotesSet      :: HeightVoteSet 'PreVote alg a
    -- ^ Set of all received valid prevotes
  , smPrecommitsSet    :: HeightVoteSet 'PreCommit alg a
    -- ^ Set of all received valid precommits
  , smLockedBlock      :: Maybe (Round, BlockID alg a)
    -- ^ Round and block we're locked on
  , smLastCommit       :: Maybe (Commit alg a)
    -- ^ Commit for previous block. Nothing if previous block is
    --   genesis block.
  }
  deriving (Show)
