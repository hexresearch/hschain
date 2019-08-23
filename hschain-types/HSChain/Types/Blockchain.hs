{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
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
{-# LANGUAGE UndecidableInstances       #-}
-- |
-- Data types for implementation of consensus algorithm
module HSChain.Types.Blockchain (
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
  , BlockchainState(..)
    -- * Data types for establishing consensus
  , Step(..)
  , FullStep(..)
  , Timeout(..)
  , Proposal(..)
    -- ** Votes
  , VoteType(..)
  , Vote(..)
  , CheckSignature(..)
    -- * Signed data
  , Signed
  , signedValue
  , signedKeyInfo
  , signValue
  , verifySignature
  , unverifySignature
  ) where

import           Codec.Serialise
import           Codec.Serialise.Decoding
import           Codec.Serialise.Encoding
import           Control.DeepSeq
import           Control.Monad
import           Control.Monad.IO.Class   (MonadIO(..))
import qualified Data.Aeson               as JSON
import           Data.ByteString.Lazy     (toStrict)
import           Data.Bits                ((.&.))
import           Data.Coerce
import           Data.Int
import           Data.List                (sortBy)
import qualified Data.List.NonEmpty       as NE
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

import HSChain.Crypto
import HSChain.Types.Validators
import HSChain.Types.Merklized


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
  deriving stock   (Show, Read, Generic, Eq, Ord)
  deriving newtype (NFData, Serialise, JSON.ToJSON, JSON.FromJSON, Enum)

-- | Voting round
newtype Round = Round Int64
  deriving stock   (Show, Read, Generic, Eq, Ord)
  deriving newtype (NFData, Serialise, JSON.ToJSON, JSON.FromJSON, Enum)

-- | Time in milliseconds since UNIX epoch.
newtype Time = Time Int64
  deriving stock   (Show, Read, Generic, Eq, Ord)
  deriving newtype (NFData, Serialise, JSON.ToJSON, JSON.FromJSON, Enum)


-- | Get current time
getCurrentTime :: MonadIO m => m Time
getCurrentTime = do
  t <- liftIO getPOSIXTime
  return $! Time $ round $ 1000 * t

-- | Convert timestamp to UTCTime
timeToUTC :: Time -> UTCTime
timeToUTC (Time t) = posixSecondsToUTCTime (realToFrac t / 1000)


instance CryptoHash alg => MerkleValue alg Height where
  merkleHash = hash
instance CryptoHash alg => MerkleValue alg Round where
  merkleHash = hash
instance CryptoHash alg => MerkleValue alg Time where
  merkleHash = hash


----------------------------------------------------------------
-- Blocks
----------------------------------------------------------------

-- | Block identified by hash
data BlockID alg a = BlockID !(Hashed alg (Header alg a))
  deriving stock    (Show, Eq, Ord, Generic)
  deriving anyclass (NFData, Serialise, JSON.ToJSON, JSON.FromJSON)

blockHash
  :: (Crypto alg)
  => Block alg a
  -> BlockID alg a
blockHash b = BlockID (hashed (blockHeader b))


-- | Block of blockchain.
data Block alg a = Block
  { blockHeader     :: !(Header alg a)
    -- ^ Block header it contains height, time of the block and hashes
    --   of all other block fields. @BlockId@ is calculated from header
  , blockData       :: a
    -- ^ Payload of block. HSChain treats it completely opaque and
    --   rely on callback to do anything to it.
  , blockValChange  :: !(ValidatorChange alg)
    -- ^ Changes in set of validators as result of block
    --   evaluation. We store changes of validators in the block for
    --
    --    1. Audit. We record changes to validator set explicitly
    --
    --    2. Allow to verify blockchain integrity without interpreting
    --       transactions. Since we know change of validators we can
    --       infer validator set for next block. This is particularly
    --       important for light clients
    --
    --   Note that we store changes since validators change
    --   infrequently and storing same validator set again and again
    --   is not economical.
  , blockLastCommit :: !(Maybe (Commit alg a))
    -- ^ Commit information for previous block. Nothing iff block
    --   is a genesis block or block at height 1.
  , blockEvidence   :: [ByzantineEvidence alg a]
    -- ^ Evidence of byzantine behavior by nodes.
  }
  deriving stock    (Show, Generic)
  deriving anyclass (Serialise, JSON.ToJSON, JSON.FromJSON)
instance (NFData a, NFData (PublicKey alg))  => NFData (Block alg a)
deriving instance (Eq (PublicKey alg), Eq a) => Eq     (Block alg a)

-- | Genesis block has many field with predetermined content so this
--   is convenience function to create genesis block.
makeGenesis
  :: (Crypto alg, Serialise a)
  => Time                       -- ^ Time of genesis
  -> a                          -- ^ Block data
  -> ValidatorSet alg           -- ^ Set of validators for block 1
  -> Block alg a
makeGenesis t dat valSet = Block
  { blockHeader = Header
      { headerHeight         = Height 0
      , headerTime           = t
      , headerLastBlockID    = Nothing
      , headerValidatorsHash = hashed emptyValidatorSet
      , headerValChangeHash  = hashed delta
      , headerDataHash       = hashed dat
      , headerLastCommitHash = hashed Nothing
      , headerEvidenceHash   = hashed []
      }
  , blockData       = dat
  , blockValChange  = delta
  , blockLastCommit = Nothing
  , blockEvidence   = []
  }
  where
    delta = validatorsDifference emptyValidatorSet valSet


-- | Block header
data Header alg a = Header
  { headerHeight         :: !Height
    -- ^ Height of block
  , headerTime           :: !Time
    -- ^ Time of block creation
  , headerLastBlockID    :: !(Maybe (BlockID alg a))
    -- ^ Hash of previous block. Nothing iff block is a genesis block
  , headerValidatorsHash :: !(Hashed alg (ValidatorSet alg))
    -- ^ Hash of validators for current block.

  , headerDataHash       :: !(Hashed alg a)
    -- ^ Hash of block data
  , headerValChangeHash  :: !(Hashed alg (ValidatorChange alg))
    -- ^ Hash of change in validators set.
  , headerLastCommitHash :: !(Hashed alg (Maybe (Commit alg a)))
    -- ^ Hash of last commit
  , headerEvidenceHash   :: !(Hashed alg [ByzantineEvidence alg a])
    -- ^ Hash of evidence of byzantine behavior
  }
  deriving stock    (Show, Eq, Generic)
  deriving anyclass (NFData, Serialise, JSON.FromJSON, JSON.ToJSON)

-- | Evidence of byzantine behaviour by some node.
data ByzantineEvidence alg a
  = OutOfTurnProposal !(Signed 'Unverified alg (Proposal alg a))
    -- ^ Node made proposal out of turn
  | ConflictingPreVote
      !(Signed 'Unverified alg (Vote 'PreVote alg a))
      !(Signed 'Unverified alg (Vote 'PreVote alg a))
    -- ^ Node made conflicting prevotes in the same round
  | ConflictingPreCommit
      !(Signed 'Unverified alg (Vote 'PreCommit alg a))
      !(Signed 'Unverified alg (Vote 'PreCommit alg a))
    -- ^ Node made conflicting precommits in the same round
  deriving stock    (Show, Eq, Generic)
  deriving anyclass (NFData, Serialise, JSON.ToJSON, JSON.FromJSON)

instance (alg ~ alg', CryptoHash alg
         ) => MerkleValue alg' (ByzantineEvidence alg a) where
  merkleHash = hash

-- | Data justifying commit
data Commit alg a = Commit
  { commitBlockID    :: !(BlockID alg a)
    -- ^ Block for which commit is done
  , commitPrecommits :: !(NE.NonEmpty (Signed 'Unverified alg (Vote 'PreCommit alg a)))
    -- ^ List of precommits which justify commit
  }
  deriving stock    (Show, Eq, Generic)
  deriving anyclass (NFData, Serialise, JSON.ToJSON, JSON.FromJSON)

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
    val <- validatorByIndex vset (signedKeyInfo sv)
    return ( validatorVotingPower val
           , signedValue sv
           )
  -- Here we discard invalid votes and calculate median time
  let times    = sortBy (comparing snd)
               $ [ (w,voteTime) | (w,Vote{..}) <- NE.toList votes
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
class ( Serialise a
      , Serialise (TX a)
      , Serialise (InterpreterState a)
      ) => BlockData a where
  -- | Type of transaction used in blockchain
  type TX a
  -- | Part of state of blockchain defined by user. Complete state is
  --   @BlockchainState@
  type InterpreterState a
  -- | Return list of transaction in block
  blockTransactions :: a -> [TX a]
  -- | Collect information about block data for logging
  logBlockData      :: a -> JSON.Object

data BlockchainState alg a = BlockchainState
  { blockchainState :: !(InterpreterState a)
  , bChValidatorSet :: !(ValidatorSet alg)
  }
  deriving stock (Generic)
instance ( NFData (InterpreterState a)
         , NFData (PublicKey alg)
         ) => NFData (BlockchainState alg a)


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
  deriving stock    (Show, Eq, Ord, Generic)
  deriving anyclass (NFData, Serialise, JSON.ToJSON, JSON.FromJSON)

data FullStep = FullStep !Height !Round !Step
  deriving stock    (Show, Eq, Ord, Generic)
  deriving anyclass (NFData, Serialise)

data Timeout = Timeout !Height !Round !Step
  deriving stock    (Show, Eq, Ord, Generic)
  deriving anyclass (NFData, Serialise, JSON.ToJSON, JSON.FromJSON)

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
  deriving stock    (Show, Eq, Ord, Generic)
  deriving anyclass (NFData, Serialise, JSON.ToJSON, JSON.FromJSON)


-- | Type of vote. Used for type-tagging of votes
data VoteType
  = PreVote
  | PreCommit
  deriving stock    (Show, Eq, Ord, Generic)
  deriving anyclass (Serialise, JSON.FromJSON, JSON.ToJSON)

-- | Single vote cast validator. Type of vote is determined by its
--   type tag
data Vote (ty :: VoteType) alg a = Vote
  { voteHeight  :: !Height
  , voteRound   :: !Round
  , voteTime    :: !Time
  , voteBlockID :: !(Maybe (BlockID alg a))
  }
  deriving stock    (Show, Eq, Ord, Generic)
  deriving anyclass (NFData, JSON.ToJSON, JSON.FromJSON)

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


----------------------------------------------------------------
-- Signed values
----------------------------------------------------------------

-- | Signed value. It contains value itself, signature, and
--   information which could be used to indentify secret key which was
--   used for signing. It could be public key, hash of public key
--   (fingerprint) or anything else. Signature is computed for CBOR
--   encoding of value.
data Signed (sign :: SignedState) alg a
  = Signed !(ValidatorIdx alg) !(Signature alg) !a
  deriving stock (Generic, Eq, Show)

instance (NFData a) => NFData (Signed sign alg a) where
  rnf (Signed a s x) = rnf a `seq` rnf s `seq` rnf x

-- | Obtain underlying value
signedValue :: Signed sign alg a -> a
signedValue (Signed _ _ a) = a

-- | Obtain information about key used for signing. It could be public
--   key itself or any other information which allows to figure out
--   which key should be used to verify signature.
signedKeyInfo :: Signed sign alg a -> ValidatorIdx alg
signedKeyInfo (Signed a _ _) = a

-- | Sign value. Note that we can generate both verified and unverified
--   values this way.
signValue
  :: (Serialise a, CryptoSign alg)
  => ValidatorIdx alg           -- ^ Key identifier
  -> PrivKey alg                -- ^ Key for signing
  -> a                          -- ^ Value to sign
  -> Signed sign alg a
signValue key privK a
  = Signed key
           (signBlob privK $ toStrict $ serialise a)
           a

-- | Verify signature. It return Nothing if verification fails for any
--   reason. Note that since @Signed@ contain only fingerprint we need
--   to supply function for looking up public keys.
verifySignature
  :: (Serialise a, CryptoSign alg)
  => ValidatorSet alg
     -- ^ Set of validators corresponding to signed value
  -> Signed 'Unverified alg a
     -- ^ Value for verifying signature
  -> Maybe (Signed 'Verified alg a)
verifySignature valSet val@(Signed idx signature a) = do
  Validator pubK _ <- validatorByIndex valSet idx
  guard $ verifyCborSignature pubK a signature
  return $ coerce val

-- | Strip verification tag
unverifySignature :: Signed ty alg a -> Signed 'Unverified alg a
unverifySignature = coerce

instance (Serialise     a) => Serialise     (Signed 'Unverified alg a)
instance (JSON.FromJSON a) => JSON.FromJSON (Signed 'Unverified alg a)
instance (JSON.ToJSON   a) => JSON.ToJSON   (Signed sign        alg a)


----------------------------------------------------------------
-- Helping application be faster.
----------------------------------------------------------------

-- | Better signalling of the need to check signatures.
data CheckSignature = CheckSignature | AlreadyChecked
  deriving stock (Eq, Ord, Show)

derivingUnbox "Time"   [t| Time   -> Int64 |] [| coerce |] [| coerce |]
derivingUnbox "Height" [t| Height -> Int64 |] [| coerce |] [| coerce |]
derivingUnbox "Round"  [t| Round  -> Int64 |] [| coerce |] [| coerce |]

#ifdef INSTANCES_SQLITE
deriving newtype instance SQL.FromField Height
deriving newtype instance SQL.ToField   Height
deriving newtype instance SQL.FromField Round
deriving newtype instance SQL.ToField   Round
deriving newtype instance SQL.FromField Time
deriving newtype instance SQL.ToField   Time
#endif
