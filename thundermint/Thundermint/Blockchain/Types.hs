{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}
-- |
-- Data types for implementation of consensus algorithm
module Thundermint.Blockchain.Types (
    -- * Newtype wrappers
    Height(..)
  , Round(..)
  , Time(..)
    -- * Basic data types for blockchain
  , BlockID
  , Block(..)
  , Header(..)
  , Commit(..)
  , BlockData(..)
    -- * Data types for establishing consensus
  , Step(..)
  , FullStep(..)
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
import           Codec.Serialise.Decoding
import           Codec.Serialise.Encoding
import qualified Data.Aeson               as JSON
import           Data.ByteString          (ByteString)
import qualified Data.HashMap.Strict      as HM
import           Data.Int
import           Data.Map                 (Map)
import           Data.Monoid              ((<>))
import           GHC.Generics             (Generic)
import qualified Katip

import Thundermint.Crypto
import Thundermint.Crypto.Containers

----------------------------------------------------------------
-- Primitives
----------------------------------------------------------------

-- | Height of block in blockchain. It's used in several contexts with
--   subtle differences. Namely:
--
--   * Height of blockchain is height of topmost block
--
--   * Height of proposal or vote is height of block we're voting for
--
--   * Current height in consensus algorithm is height of block we're
--     deciding on.
newtype Height = Height Int64
  deriving (Show, Eq, Ord, Serialise, JSON.ToJSON, JSON.FromJSON, Enum)

-- | Voting round
newtype Round = Round Int64
  deriving (Show, Eq, Ord, Serialise, JSON.ToJSON, JSON.FromJSON, Enum)

-- | Unix timestamp
newtype Time = Time Int64
  deriving (Show, Eq, Ord, Serialise)



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
  deriving (Show, Eq, Generic)
instance Serialise a => Serialise (Block alg a)

-- | Block header
data Header alg a = Header
  { headerChainID        :: ByteString
    -- ^ Identifier of chain we're working on. It should be same in
    --   all blocks in blockchain
  , headerHeight         :: Height
    -- ^ Height of block
  , headerTime           :: Time
    -- ^ Time of block creation
  , headerLastBlockID    :: Maybe (BlockID alg a)
    -- ^ Hash of previous block. Nothing iff block is a genesis block
  , headerValidatorsHash :: Hash alg
    -- ^ Hash of validators for current block.

  -- FIXME: Add various hashes
  -- , headerDataHash       :: Hash
  -- , headerConsensusHash  :: Hash
  }
  deriving (Show, Eq, Generic)
instance Serialise (Header alg a)

-- | Data justifying commit
data Commit alg a = Commit
  { commitBlockID    :: BlockID alg a
    -- ^ Block for which commit is done
  , commitPrecommits :: [Signed 'Verified alg (Vote 'PreCommit alg a)]
    -- ^ List of precommits which justify commit
  }
  deriving (Show, Eq, Generic)
instance Serialise (Commit alg a)


-- | Type class for data which could be put into block
class (Serialise a, Serialise (TX a)) => BlockData a where
  -- | Transaction type of block
  type TX a
  -- | Return list of transaction in block
  blockTransactions :: a -> [TX a]
  logBlockData      :: a -> JSON.Object

instance (Serialise a) => BlockData [a] where
  type TX [a] = a
  blockTransactions = id
  logBlockData      = HM.singleton "Ntx" . JSON.toJSON . length


----------------------------------------------------------------
-- Data types for establishing consensus
----------------------------------------------------------------

-- | Step of the algorithm
data Step
  = StepNewHeight
    -- ^ We have just entered new height and waiting for stragglers
    --   precommits for block
  | StepProposal
  | StepPrevote
  | StepPrecommit
  | StepAwaitCommit
    -- ^ We already reached consensus and now waiting for data to
    --   perform commit. Node could only stay in this state if it
    --   catching up and got all required precommits before getting
    --   block.
  deriving (Show,Eq,Ord,Generic)
instance Serialise     Step
instance JSON.ToJSON   Step
instance JSON.FromJSON Step

data FullStep = FullStep !Height !Round !Step
  deriving (Show,Eq,Ord,Generic)
instance Serialise FullStep

data Timeout = Timeout Height Round Step
  deriving (Show,Eq,Ord,Generic)
instance Serialise Timeout

-- | Proposal for new block. Proposal include only hash of block and
--   block itself is gossiped separately.
data Proposal alg a = Proposal
  { propHeight    :: Height
    -- ^ Proposal height
  , propRound     :: Round
    -- ^ Propoasl round
  , propTimestamp :: Time
    -- ^ Time of proposal
  , propPOL       :: Maybe Round
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
  { voteHeight  :: Height
  , voteRound   :: Round
  , voteTime    :: Time
  , voteBlockID :: Maybe (BlockID alg a)
  }
  deriving (Show,Eq,Ord,Generic)

instance Serialise (Vote 'PreVote alg a) where
    encode = encodeVote 0
    decode = decodeVote 0

instance Serialise (Vote 'PreCommit alg a) where
    encode = encodeVote 1
    decode = decodeVote 1

encodeVote :: Word -> Vote ty alg a -> Encoding
encodeVote tag Vote{..} =
    encodeListLen 5 <>
    encodeWord tag <>
    encode voteHeight <>
    encode voteRound <>
    encode voteTime <>
    encode voteBlockID

decodeVote :: Word -> Decoder s (Vote ty alg a)
decodeVote expectedTag = do
    len <- decodeListLen
    tag <- decodeWord
    case len of
        5 | tag == expectedTag ->
                Vote <$> decode <*> decode <*> decode <*> decode
          | otherwise ->
                fail ("Invalid Vote tag, expected: " ++ show expectedTag
                      ++ ", actual: " ++ show tag)
        _ -> fail $ "Invalid Vote encoding"

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
  deriving (Show, Eq, Generic)
instance Serialise     ProposalState
instance JSON.ToJSON   ProposalState
instance JSON.FromJSON ProposalState

-- | State for tendermint consensus at some particular height.
data TMState alg a = TMState
  { smRound         :: Round
    -- ^ Current round
  , smStep          :: Step
    -- ^ Current step in the round
  , smProposals     :: Map Round (Signed 'Verified alg (Proposal alg a))
    -- ^ Proposal for current round
  , smPrevotesSet   :: HeightVoteSet 'PreVote alg a
    -- ^ Set of all received valid prevotes
  , smPrecommitsSet :: HeightVoteSet 'PreCommit alg a
    -- ^ Set of all received valid precommits
  , smLockedBlock   :: Maybe (Round, BlockID alg a)
    -- ^ Round and block we're locked on
  , smLastCommit    :: Maybe (Commit alg a)
    -- ^ Commit for previous block. Nothing if previous block is
    --   genesis block.
  }
  deriving (Show)



----------------------------------------------------------------
-- Logging instances
----------------------------------------------------------------

instance Katip.ToObject Round where
  toObject (Round i) = HM.singleton "R" (JSON.toJSON i)
instance Katip.LogItem Round where
  payloadKeys _ _ = Katip.AllKeys

instance Katip.ToObject Height where
  toObject (Height i) = HM.singleton "H" (JSON.toJSON i)
instance Katip.LogItem Height where
  payloadKeys _ _ = Katip.AllKeys

instance Katip.ToObject FullStep where
  toObject (FullStep (Height h) (Round r) s) = HM.fromList
    [ ("H", JSON.toJSON h)
    , ("R", JSON.toJSON r)
    , ("S", JSON.toJSON s)
    ]
instance Katip.LogItem FullStep where
  payloadKeys _ _ = Katip.AllKeys

instance Katip.ToObject ProposalState where
  toObject p = HM.singleton "val" (JSON.toJSON p)
instance Katip.LogItem ProposalState where
  payloadKeys _ _ = Katip.AllKeys
