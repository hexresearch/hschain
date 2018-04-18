{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE KindSignatures     #-}
{-# LANGUAGE StandaloneDeriving #-}
-- |
-- Data types for implementation of consensus algorithm
module Thundermint.Consensus.Types where

import           Data.ByteString   (ByteString)
import           Data.Int

import Thundermint.Crypto
import Thundermint.Crypto.Containers

----------------------------------------------------------------
-- Primitives
----------------------------------------------------------------

newtype Height = Height Int64
  deriving (Show, Eq, Ord)

newtype Round = Round Int64
  deriving (Show, Eq, Ord)

newtype Time = Time Int64
  deriving (Show, Eq, Ord)

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
  , blockLastCommit :: Commit alg a   -- ^ Commit information for previous block
  }
deriving instance ( Show (Address alg)
                  , Show (Signature alg)
                  , Show a
                  ) => Show (Block alg a)

-- | Block header
data Header alg a = Header
  { headerChainID     :: ByteString -- ^ Identifier of chain we're working on
  , headerHeight      :: Height     -- ^ Height of block
  , headerTime        :: Time       -- ^ Time of block creation
  , headerLastBlockID :: BlockID alg a

  -- FIXME: Add various hashes
  -- , headerDataHash       :: Hash
  -- , headerValidatorsHash :: Hash
  -- , headerConsensusHash  :: Hash
  }
  deriving (Show)

-- | Data justifying commit
data Commit alg a = Commit
  { commitBlockID    :: BlockID alg a
    -- ^ Block for which commit is done
  , commitPrecommits :: [Signed 'Verified alg (Vote 'PreVote alg a)]
    -- ^ List of precommits which justify commit
  }
  deriving (Show)


-- | Step of the algorithm
data Step
  = StepProposal
  | StepPrevote
  | StepPrecommit
  deriving (Show,Eq,Ord)

data Timeout = Timeout Height Round Step
  deriving (Show,Eq,Ord)

----------------------------------------------------------------
-- Votes
----------------------------------------------------------------

-- | Proposal which could be made by 
data Proposal alg a = Proposal
  -- FIXME: we need to carry actual proposal data as well!
  { propHeight    :: Height
    -- ^ Proposal height
  , propRound     :: Round
    -- ^ Propoasl round
  , propTimestamp :: Time
    -- ^ Time of proposal
  , propPOL       :: Maybe (Round, BlockID alg a)
    -- ^ Proof of Lock for proposal [FIXME: why it's needed?]
  , propBlockID   :: BlockID alg a
  }
  deriving (Show)

-- | Type of vote. Used for type-tagging of votes
data VoteType = PreVote
              | PreCommit
              deriving (Show,Eq)

-- | Single vote cast validator. Type of vote is determined by its
--   type tag
data Vote (ty :: VoteType) alg a= Vote
  { voteHeight     :: Height
  , voteRound      :: Round
  , voteTime       :: Time
  , voteBlockID    :: Maybe (BlockID alg a)
  }
  deriving (Show,Eq,Ord)

type VoteSet ty alg a = SignedSet 'Verified (Vote ty alg a)

type HeightVoteSet ty alg a = SignedSetMap Round 'Verified alg (Vote ty alg a)

