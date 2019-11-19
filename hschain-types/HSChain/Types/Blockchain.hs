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
{-# LANGUAGE ScopedTypeVariables        #-}
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
  , GBlock(..)
  , Block
  , Header
  , Commit(..)
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
import           Data.Coerce
import           Data.Int
import           Data.Functor.Classes
import qualified Data.List.NonEmpty       as NE
import           Data.Monoid              ((<>))
import           Data.Time.Clock          (UTCTime)
import           Data.Time.Clock.POSIX    (getPOSIXTime,posixSecondsToUTCTime)
import           Data.Vector.Unboxed.Deriving
import           GHC.Generics             (Generic)
#ifdef INSTANCES_SQLITE
import qualified Database.SQLite.Simple.FromField as SQL
import qualified Database.SQLite.Simple.ToField   as SQL
#endif

import HSChain.Crypto
import HSChain.Crypto.Classes.Hash
import HSChain.Types.Validators
import HSChain.Types.Merkle.Types


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
  deriving newtype (NFData, Serialise, JSON.ToJSON, JSON.FromJSON, Enum, CryptoHashable)

-- | Voting round
newtype Round = Round Int64
  deriving stock   (Show, Read, Generic, Eq, Ord)
  deriving newtype (NFData, Serialise, JSON.ToJSON, JSON.FromJSON, Enum, CryptoHashable)


-- | Time in milliseconds since UNIX epoch.
newtype Time = Time Int64
  deriving stock   (Show, Read, Generic, Eq, Ord)
  deriving newtype (NFData, Serialise, JSON.ToJSON, JSON.FromJSON, Enum, CryptoHashable)


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
newtype BlockID alg a = BlockID (Hashed alg (Block alg a))
  deriving stock    (Show, Eq, Ord, Generic)
  deriving anyclass (NFData, Serialise, JSON.ToJSON, JSON.FromJSON)
  deriving newtype  (CryptoHashable)

blockHash
  :: (Crypto alg)
  => Block alg a
  -> BlockID alg a
blockHash = BlockID . hashed


type Block  = GBlock IdNode
type Header = GBlock Hashed

-- | Block of blockchain.
data GBlock f alg a = Block
  { blockHeight           :: !Height
    -- ^ Height of a block
  , blockPrevBlockID      :: !(Maybe (BlockID alg a))
    -- ^ Hash of previous block. Nothing iff block is a genesis block
  , blockValidatorsHash   :: !(Hashed alg (ValidatorSet alg))
    -- ^ Set of validators used to create this block.
  , blockValChange        :: !(MerkleNode f alg (ValidatorChange alg))
    -- ^ Set of validators for the next block.
  , blockPrevCommit       :: !(Maybe (MerkleNode f alg (Commit alg a)))
    -- ^ Commit for previous block. Nothing iff block is a genesis
    --   block or block at height 1.
  , blockEvidence         :: !(MerkleNode f alg [ByzantineEvidence alg a])
    -- ^ Evidence of byzantine behavior by nodes.
  , blockData             :: !(MerkleNode f alg a)
    -- ^ Payload of block. HSChain treats it completely opaque and
    --   rely on callback to do anything to it.
  , blockStateHash        :: !(Hashed alg (InterpreterState a))
    -- ^ Hash of state after evaluation of this block.
  }
  deriving stock    (Show, Generic)
instance (IsMerkle f, CryptoHash alg) => CryptoHashable (GBlock f alg a) where
  hashStep = genericHashStep "hschain"

instance (Crypto alg, CryptoHashable a, Serialise     a, IsMerkle f)  => Serialise     (GBlock f alg a)
instance (Crypto alg, CryptoHashable a, JSON.FromJSON a, IsMerkle f)  => JSON.FromJSON (GBlock f alg a)
instance (Crypto alg, JSON.ToJSON a, IsMerkle f)                      => JSON.ToJSON   (GBlock f alg a)
instance (NFData a, NFData1 (f alg), NFData (PublicKey alg))          => NFData (GBlock f alg a)
deriving instance (Eq (PublicKey alg), IsMerkle f, Eq1 (f alg), Eq a) => Eq (GBlock f alg a)



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
instance (Crypto alg) => CryptoHashable (ByzantineEvidence alg a) where
  hashStep = genericHashStep "hschain"

-- | Data justifying commit
data Commit alg a = Commit
  { commitBlockID    :: !(BlockID alg a)
    -- ^ Block for which commit is done
  , commitPrecommits :: !(NE.NonEmpty (Signed 'Unverified alg (Vote 'PreCommit alg a)))
    -- ^ List of precommits which justify commit
  }
  deriving stock    (Show, Eq, Generic)
  deriving anyclass (NFData, Serialise, JSON.ToJSON, JSON.FromJSON)
instance (Crypto alg) => CryptoHashable (Commit alg a) where
  hashStep = genericHashStep "hschain"


-- | Type class for data which could be put into block
class ( Serialise a
      , Serialise (TX a)
      , Serialise (InterpreterState a)
      , CryptoHashable a
      , CryptoHashable (TX a)
      , CryptoHashable (InterpreterState a)
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
  = StepNewHeight !Int
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
  , propBlockID   :: !(BlockID alg a)
    -- ^ Hash of proposed block
  }
  deriving stock    (Show, Eq, Ord, Generic)
  deriving anyclass (NFData, Serialise, JSON.ToJSON, JSON.FromJSON)
instance (CryptoHash alg) => CryptoHashable (Proposal alg a) where
  hashStep = genericHashStep "hschain"

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
  deriving anyclass (NFData, JSON.ToJSON, JSON.FromJSON, Serialise)

instance (CryptoHash alg) => CryptoHashable (Vote 'PreVote alg a) where
  hashStep Vote{..}
    = hashStep (UserType "hschain" "Vote@PreVote")
   <> hashStep voteHeight
   <> hashStep voteRound
   <> hashStep voteTime
   <> hashStep voteBlockID

instance (CryptoHash alg) => CryptoHashable (Vote 'PreCommit alg a) where
  hashStep Vote{..}
    = hashStep (UserType "hschain" "Vote@PreCommit")
   <> hashStep voteHeight
   <> hashStep voteRound
   <> hashStep voteTime
   <> hashStep voteBlockID


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
  deriving stock (Generic, Eq, Ord, Show)

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
  :: forall sign alg a. (Crypto alg, CryptoHashable a)
  => ValidatorIdx alg           -- ^ Key identifier
  -> PrivKey alg                -- ^ Key for signing
  -> a                          -- ^ Value to sign
  -> Signed sign alg a
signValue key privK a
  = Signed key (signBlob privK h) a
  where
    Hash h = hash a :: Hash alg

-- | Verify signature. It return Nothing if verification fails for any
--   reason. Note that since @Signed@ contain only fingerprint we need
--   to supply function for looking up public keys.
verifySignature
  :: forall alg a. (Crypto alg, CryptoHashable a)
  => ValidatorSet alg
     -- ^ Set of validators corresponding to signed value
  -> Signed 'Unverified alg a
     -- ^ Value for verifying signature
  -> Maybe (Signed 'Verified alg a)
verifySignature valSet val@(Signed idx sig a) = do
  Validator pubK _ <- validatorByIndex valSet idx
  guard $ verifyBlobSignature pubK h sig
  return $ coerce val
  where
    Hash h = hash a :: Hash alg

-- | Strip verification tag
unverifySignature :: Signed ty alg a -> Signed 'Unverified alg a
unverifySignature = coerce

instance (Serialise      a) => Serialise      (Signed 'Unverified alg a)
instance (JSON.FromJSON  a) => JSON.FromJSON  (Signed 'Unverified alg a)
instance (JSON.ToJSON    a) => JSON.ToJSON    (Signed sign        alg a)
instance (CryptoHashable a, CryptoAsymmetric alg) => CryptoHashable (Signed sign alg a) where
  hashStep = genericHashStep "hschain"

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
