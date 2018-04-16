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


----------------------------------------------------------------
-- Blocks
----------------------------------------------------------------

type BlockID alg a = BlockHash alg (Block alg a)

-- | Block 
data Block alg a = Block
  { blockHeader     :: Header alg a
  , blockData       :: a
  , blockLastCommit :: Commit alg a   -- ^ Commit information for previous block
  }
deriving instance ( Show (Address alg)
                  , Show (Signature alg)
                  , Show a
                  ) => Show (Block alg a)


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

data Commit alg a = Commit
  { commitBlockID    :: BlockID alg a
    -- ^ Block for which commit is done
  , commitPrecommits :: [Signed 'Verified alg (Vote 'PreVote alg a)]
    -- ^ List of precommits which justify commit
  }
deriving instance ( Show (Address alg)
                  , Show (Signature alg)
                  ) => Show (Commit alg a)

----------------------------------------------------------------
-- Votes
----------------------------------------------------------------

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
  deriving (Show,Eq)

type VoteSet ty alg a = SignedSet 'Verified (Vote ty alg a)

type HeightVoteSet ty alg a = SignedSetMap Round  'Verified (Vote ty alg a)

