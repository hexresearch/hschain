{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
-- |
-- Data types for implementation of consensus algorithm
module Thundermint.Types.Blockchain (
    -- * Newtype wrappers
    Height(..)
  , Round(..)
  , Time(..)
  , getCurrentTime
  , timeToUTC
    -- * Basic data types for blockchain
  , BlockID(..)
  , blockHash
  , Block(..)
  , makeGenesis
  , Header(..)
  , Commit(..)
  , commitTime
  , ByzantineEvidence(..)
  , BlockData(..)
    -- * Data types for establishing consensus
  , Step(..)
  , FullStep(..)
  , Timeout(..)
  , Proposal(..)
    -- ** Votes
  , VoteType(..)
  , Vote(..)
  , CheckSignature(..)
  ) where

import           Codec.Serialise
import           Codec.Serialise.Decoding
import           Codec.Serialise.Encoding
import           Control.DeepSeq
import           Control.Monad
import           Control.Monad.IO.Class   (MonadIO(..))
import qualified Data.Aeson               as JSON
import           Data.Aeson               ((.=), (.:))
import           Data.ByteString          (ByteString)
import qualified Data.HashMap.Strict      as HM
import           Data.Bits                ((.&.))
import           Data.Coerce
import           Data.Int
import           Data.List                (sortBy)
import           Data.Monoid              ((<>))
import           Data.Ord                 (comparing)
import           Data.Time.Clock          (UTCTime)
import           Data.Time.Clock.POSIX    (getPOSIXTime,posixSecondsToUTCTime)
import           Data.Vector.Unboxed.Deriving
import           GHC.Generics             (Generic)
#ifdef INSTANCES_SQLITE
import qualified Database.SQLite.Simple.FromField as SQL
import qualified Database.SQLite.Simple.ToField   as SQL
#endif

import Thundermint.Crypto
import Thundermint.Types.Validators


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
  deriving (Show, Generic, Eq, Ord, NFData, Serialise, JSON.ToJSON, JSON.FromJSON, Enum)

-- | Voting round
newtype Round = Round Int64
  deriving (Show, Generic, Eq, Ord, NFData, Serialise, JSON.ToJSON, JSON.FromJSON, Enum)

-- | Time in milliseconds since UNIX epoch.
newtype Time = Time Int64
  deriving (Show, Generic, Eq, Ord, NFData, Serialise, JSON.ToJSON, JSON.FromJSON)

-- | Get current time
getCurrentTime :: MonadIO m => m Time
getCurrentTime = do
  t <- liftIO getPOSIXTime
  return $! Time $ round $ 1000 * t

-- | Convert timestamp to UTCTime
timeToUTC :: Time -> UTCTime
timeToUTC (Time t) = posixSecondsToUTCTime (realToFrac t / 1000)


----------------------------------------------------------------
-- Blocks
----------------------------------------------------------------

-- | Block identified by hash
data BlockID alg a = BlockID !(Hashed alg (Header alg a))
  deriving (Show,Eq,Ord,Generic)
instance NFData        (BlockID alg a)
instance Serialise     (BlockID alg a)
instance CryptoHash alg => JSON.ToJSON   (BlockID alg a)
instance CryptoHash alg => JSON.FromJSON (BlockID alg a)

blockHash
  :: (Crypto alg, Serialise a)
  => Block alg a
  -> BlockID alg a
blockHash b = BlockID (hashed (blockHeader b))


-- | Block data type
data Block alg a = Block
  { blockHeader     :: !(Header alg a)
  , blockData       :: !a
    -- ^ Payload of block. Thundermint treats it completely opaque and
    --   rely on callback to do anything to it.
  , blockValChange  :: [ValidatorChange alg]
    -- ^ Changes in set of validators as result of block evaluation
  , blockLastCommit :: !(Maybe (Commit alg a))
    -- ^ Commit information for previous block. Nothing iff block
    --   is a genesis block or block at height 1.
  , blockEvidence   :: [ByzantineEvidence alg a]
    -- ^ Evidence of byzantine behavior by nodes.
  }
  deriving (Show, Generic)

instance (NFData a, NFData (PublicKey alg)) => NFData (Block alg a)
deriving instance (Eq (PublicKey alg), Eq a) => Eq (Block alg a)
instance (Crypto alg, Serialise     a) => Serialise     (Block alg a)
instance (Crypto alg, JSON.FromJSON a) => JSON.FromJSON (Block alg a)
instance (Crypto alg, JSON.ToJSON   a) => JSON.ToJSON   (Block alg a)

-- | Genesis block has many field with predetermined content so this
--   is convenience function to create genesis block.
makeGenesis
  :: (Crypto alg, Serialise a)
  => ByteString                 -- ^ Text identifier of chain
  -> Time                       -- ^ Time of genesis
  -> a                          -- ^ Block data
  -> ValidatorSet alg           -- ^ Set of validators for block 1
  -> Block alg a
makeGenesis chainID t dat valSet = Block
  { blockHeader = Header
      { headerChainID        = chainID
      , headerHeight         = Height 0
      , headerTime           = t
      , headerLastBlockID    = Nothing
      , headerValidatorsHash = hashed valSet
      , headerValChangeHash  = hashed []
      , headerDataHash       = hashed dat
      , headerLastCommitHash = hashed Nothing
      , headerEvidenceHash   = hashed []
      }
  , blockData       = dat
  , blockValChange  = []
  , blockLastCommit = Nothing
  , blockEvidence   = []
  }

-- | Block header
data Header alg a = Header
  { headerChainID        :: !ByteString
    -- ^ Identifier of chain we're working on. It should be same in
    --   all blocks in blockchain
  , headerHeight         :: !Height
    -- ^ Height of block
  , headerTime           :: !Time
    -- ^ Time of block creation
  , headerLastBlockID    :: !(Maybe (BlockID alg a))
    -- ^ Hash of previous block. Nothing iff block is a genesis block
  , headerValidatorsHash :: !(Hashed alg (ValidatorSet alg))
    -- ^ Hash of validators for current block.

  , headerDataHash       :: !(Hashed alg a)
    -- ^ Hash of block data
  , headerValChangeHash  :: !(Hashed alg [ValidatorChange alg])
    -- ^ Hash of change in validators set.
  , headerLastCommitHash :: !(Hashed alg (Maybe (Commit alg a)))
    -- ^ Hash of last commit
  , headerEvidenceHash   :: !(Hashed alg [ByzantineEvidence alg a])
    -- ^ Hash of evidence of byzantine behavior
  }
  deriving (Show, Eq, Generic)
instance NFData    (Header alg a)
instance Serialise (Header alg a)

instance CryptoHash alg => JSON.ToJSON (Header alg a) where
  toJSON Header{..} =
    JSON.object [ "headerChainID"        .= encodeBase58 headerChainID
                , "headerHeight"         .= headerHeight
                , "headerTime"           .= headerTime
                , "headerLastBlockID"    .= headerLastBlockID
                , "headerValidatorsHash" .= headerValidatorsHash
                , "headerDataHash"       .= headerDataHash
                , "headerValChangeHash"  .= headerValChangeHash
                , "headerLastCommitHash" .= headerLastCommitHash
                , "headerEvidenceHash"   .= headerEvidenceHash
                ]

instance CryptoHash alg => JSON.FromJSON (Header alg a) where
  parseJSON = JSON.withObject "Header" $ \o -> do
    headerChainID        <- fromBase58 =<< o .: "headerChainID"
    headerHeight         <- o .: "headerHeight"
    headerTime           <- o .: "headerTime"
    headerLastBlockID    <- o .: "headerLastBlockID"
    headerValidatorsHash <- o .: "headerValidatorsHash"
    headerDataHash       <- o .: "headerDataHash"
    headerValChangeHash  <- o .: "headerValChangeHash"
    headerLastCommitHash <- o .: "headerLastCommitHash"
    headerEvidenceHash   <- o .: "headerEvidenceHash"
    return Header{..}
    where
      fromBase58 = maybe complain return . decodeBase58
      complain   = fail "Incorrect Base58 encoding" 

-- | Evidence of byzantine behaviour by some node.
data ByzantineEvidence alg a
  = OutOfTurnProposal !(Signed 'Unverified alg (Proposal alg a))
    -- ^ Node made proposal out of turn
  | ConflictingPreVote
      !(Signed 'Unverified alg (Vote 'PreVote alg a))
      !(Signed 'Unverified alg (Vote 'PreVote alg a))
    -- ^ Node made conflicting prevotes in the same round
  | ConflictingPreCommit
      !(Signed 'Unverified alg (Vote 'PreVote alg a))
      !(Signed 'Unverified alg (Vote 'PreVote alg a))
    -- ^ Node made conflicting precommits in the same round
  deriving (Show, Eq, Generic)
instance NFData        (ByzantineEvidence alg a)
instance Serialise     (ByzantineEvidence alg a)
instance CryptoHash alg => JSON.FromJSON (ByzantineEvidence alg a)
instance CryptoHash alg => JSON.ToJSON   (ByzantineEvidence alg a)


-- | Data justifying commit
data Commit alg a = Commit
  { commitBlockID    :: !(BlockID alg a)
    -- ^ Block for which commit is done
  , commitPrecommits :: !([Signed 'Unverified alg (Vote 'PreCommit alg a)])
    -- ^ List of precommits which justify commit
  }
  deriving (Show, Eq, Generic)
instance NFData        (Commit alg a)
instance Serialise     (Commit alg a)
instance CryptoHash alg => JSON.FromJSON (Commit alg a)
instance CryptoHash alg => JSON.ToJSON   (Commit alg a)

-- | Calculate time of commit as median of time of votes where votes
--   are weighted according to voting power of corresponding
--   validators.
commitTime
  :: ValidatorSet alg -- ^ Set of validators for commit
  -> Time             -- ^ Time of previous block. Votes that aren't
                      --   cast later that that are discarded.
  -> Commit alg a     -- ^ Commit to calculate time
  -> Maybe Time
commitTime vset t0 Commit{..} = do
  votes <- forM commitPrecommits $ \sv -> do
    val <- validatorByAddr vset (signedAddr sv)
    return ( validatorVotingPower val
           , signedValue sv
           )
  -- Here we discard invalid votes and calculate median time
  let times    = sortBy (comparing snd)
               $ [ (w,voteTime) | (w,Vote{..}) <- votes
                                , voteTime > t0
                                , voteBlockID == Just commitBlockID
                                ]
      totPower = sum (fst <$> times)
      half     = fromIntegral $ totPower `div` 2
  case odd totPower of
    True  -> case zDrop half times of
      (_,t):_         -> return t
      _               -> Nothing
    False -> case zDrop (half - 1) times of
      (1,t1):(_,t2):_ -> return $ average t1 t2
      (_,t ):_        -> return t
      _               -> Nothing


average :: Time -> Time -> Time
average (Time t1) (Time t2) = Time $ (t1 `div` 2) + (t2 `div` 2) + (t1 .&. t2 .&. 1)

zDrop :: Integer -> [(Integer,a)] -> [(Integer,a)]
zDrop _ [] = []
zDrop 0 xs = xs
zDrop i ((n,x):xs)
  | i >= n    = zDrop (i - n) xs
  | otherwise = (n-i, x) : xs


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
    -- ^ Making proposal if node is proposed or receiving proposal
  | StepPrevote
    -- ^ Prevoting block or NIL and collecting prevotes from other nodes
  | StepPrecommit
    -- ^ Precommiting block or NIL and collecting precommits from
    --   other nodes
  | StepAwaitCommit !Round
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

data Timeout = Timeout !Height !Round !Step
  deriving (Show,Eq,Ord,Generic)
instance Serialise Timeout

-- | Proposal for new block. Proposal include only hash of block and
--   block itself is gossiped separately.
data Proposal alg a = Proposal
  { propHeight    :: !Height
    -- ^ Proposal height
  , propRound     :: !Round
    -- ^ Propoasl round
  , propTimestamp :: !Time
    -- ^ Time of proposal
  , propPOL       :: !(Maybe Round)
    -- ^ Proof of Lock for proposal
    --
    -- FIXME: why it's needed? How should it be used?
  , propBlockID   :: !(BlockID alg a)
    -- ^ Hash of proposed block
  }
  deriving (Show, Eq, Generic)
instance NFData        (Proposal alg a)
instance Serialise     (Proposal alg a)
instance CryptoHash alg => JSON.FromJSON (Proposal alg a)
instance CryptoHash alg => JSON.ToJSON   (Proposal alg a)

-- | Type of vote. Used for type-tagging of votes
data VoteType = PreVote
              | PreCommit
              deriving (Show,Eq,Generic)

instance Serialise     VoteType
instance JSON.FromJSON VoteType
instance JSON.ToJSON   VoteType

-- | Single vote cast validator. Type of vote is determined by its
--   type tag
data Vote (ty :: VoteType) alg a= Vote
  { voteHeight  :: !Height
  , voteRound   :: !Round
  , voteTime    :: !Time
  , voteBlockID :: !(Maybe (BlockID alg a))
  }
  deriving (Show,Eq,Ord,Generic)

instance NFData (Vote ty alg a)
instance Serialise (Vote 'PreVote alg a) where
    encode = encodeVote 0
    decode = decodeVote 0

instance Serialise (Vote 'PreCommit alg a) where
    encode = encodeVote 1
    decode = decodeVote 1

instance CryptoHash alg => JSON.FromJSON (Vote ty alg a)
instance CryptoHash alg => JSON.ToJSON   (Vote ty alg a)



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

----------------------------------------------------------------
-- Helping application be faster.
----------------------------------------------------------------

-- | Better signalling of the need to check signatures.
data CheckSignature = CheckSignature | AlreadyChecked
  deriving (Eq, Ord, Show)

derivingUnbox "Time"   [t| Time   -> Int64 |] [| coerce |] [| coerce |]
derivingUnbox "Height" [t| Height -> Int64 |] [| coerce |] [| coerce |]
derivingUnbox "Round"  [t| Round  -> Int64 |] [| coerce |] [| coerce |]

#ifdef INSTANCES_SQLITE
deriving instance SQL.FromField Height
deriving instance SQL.ToField   Height
deriving instance SQL.FromField Round
deriving instance SQL.ToField   Round
deriving instance SQL.FromField Time
deriving instance SQL.ToField   Time
#endif
