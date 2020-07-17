{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
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
  , toHeader
  , Commit(..)
  , ByzantineEvidence(..)
  , ProposerSelection(..)
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
    -- * Blockchain logic
    -- ** Evaluation context
  , BChEval(..)
  , Genesis
  , BlockValidation
  , ValidatedBlock
  , EvaluationResult
  , ProposedBlock
    -- ** Logic of blockchain
  , NewBlock(..)
  , BChLogic(..)
    -- ** Type class for hoisting function
  , HoistDict(..)
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
import           Control.Exception        (Exception)
import           Control.Monad
import           Control.Monad.IO.Class   (MonadIO(..))
import qualified Data.Aeson               as JSON
import           Data.Coerce
import           Data.Int
import           Data.Functor.Classes
import qualified Data.List.NonEmpty       as NE
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
--
----------------------------------------------------------------

-- | Apply natural transformation to data type. Just a type class for
--   dictionaries of functions with common kind.
class HoistDict dct where
  hoistDict :: (forall x. m x -> n x) -> dct m a -> dct n a


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
newtype BlockID a = BlockID (Hashed (Alg a) (Block a))
  deriving stock    (Show, Eq, Ord, Generic)
  deriving anyclass (NFData, Serialise, JSON.ToJSON, JSON.FromJSON)

deriving newtype instance CryptoHash (Alg a) => CryptoHashable (BlockID a)

blockHash
  :: (Crypto (Alg a))
  => Block   a
  -> BlockID a
blockHash = BlockID . hashed


type Block  = GBlock Identity
type Header = GBlock Proxy

-- | Block of blockchain.
data GBlock f a = Block
  { blockHeight           :: !Height
    -- ^ Height of a block
  , blockPrevBlockID      :: !(Maybe (BlockID a))
    -- ^ Hash of previous block. Nothing iff block is a genesis block
  , blockValidators       :: !(Hashed (Alg a) (ValidatorSet (Alg a)))
    -- ^ Set of validators used to create this block.
  , blockNewValidators    :: !(Hashed (Alg a) (ValidatorSet (Alg a)))
    -- ^ Set of validators for the next block.
  , blockPrevCommit       :: !(Maybe (MerkleNode f (Alg a) (Commit a)))
    -- ^ Commit for previous block. Nothing iff block is a genesis
    --   block or block at height 1.
  , blockEvidence         :: !(MerkleNode f (Alg a) [ByzantineEvidence a])
    -- ^ Evidence of byzantine behavior by nodes.
  , blockData             :: !(MerkleNode f (Alg a) a)
    -- ^ Payload of block. HSChain treats it completely opaque and
    --   rely on callback to do anything to it.
  , blockStateHash        :: !(Hashed (Alg a) (BlockchainState a))
    -- ^ Hash of state after evaluation of this block.
  }
  deriving stock    (Generic)
instance (CryptoHash (Alg a)) => CryptoHashable (GBlock f a) where
  hashStep = genericHashStep "hschain"

toHeader :: GBlock f a-> Header a
toHeader Block{..} = Block
  { blockData       = toHashedNode     blockData
  , blockPrevCommit = toHashedNode <$> blockPrevCommit
  , blockEvidence   = toHashedNode     blockEvidence
  , ..
  }

instance (IsMerkle f, Crypto (Alg a), CryptoHashable a, Serialise     a) => Serialise     (GBlock f a)
instance (IsMerkle f, Crypto (Alg a), CryptoHashable a, JSON.FromJSON a) => JSON.FromJSON (GBlock f a)
instance (IsMerkle f, Crypto (Alg a),                   JSON.ToJSON   a) => JSON.ToJSON   (GBlock f a)
instance (NFData a, NFData1 f, NFData (PublicKey (Alg a)))     => NFData (GBlock f a)
deriving instance (CryptoHash (Alg a), Show1 f, Show a) => Show (GBlock f a)
deriving instance (Eq (PublicKey (Alg a)), IsMerkle f, Eq1 f, Eq a) => Eq (GBlock f a)



-- | Evidence of byzantine behaviour by some node.
data ByzantineEvidence a
  = OutOfTurnProposal !(Signed 'Unverified (Alg a) (Proposal a))
    -- ^ Node made proposal out of turn
  | ConflictingPreVote
      !(Signed 'Unverified (Alg a) (Vote 'PreVote a))
      !(Signed 'Unverified (Alg a) (Vote 'PreVote a))
    -- ^ Node made conflicting prevotes in the same round
  | ConflictingPreCommit
      !(Signed 'Unverified (Alg a) (Vote 'PreCommit a))
      !(Signed 'Unverified (Alg a) (Vote 'PreCommit a))
    -- ^ Node made conflicting precommits in the same round
  deriving stock    (Show, Eq, Generic)
  deriving anyclass (NFData, Serialise, JSON.ToJSON, JSON.FromJSON)
instance (Crypto (Alg a)) => CryptoHashable (ByzantineEvidence a) where
  hashStep = genericHashStep "hschain"

-- | Data justifying commit
data Commit a = Commit
  { commitBlockID    :: !(BlockID a)
    -- ^ Block for which commit is done
  , commitPrecommits :: !(NE.NonEmpty (Signed 'Unverified (Alg a) (Vote 'PreCommit a)))
    -- ^ List of precommits which justify commit
  }
  deriving stock    (Show, Eq, Generic)
  deriving anyclass (NFData, Serialise, JSON.ToJSON, JSON.FromJSON)
instance (Crypto (Alg a)) => CryptoHashable (Commit a) where
  hashStep = genericHashStep "hschain"


-- | Newtype wrapper for function on how to compute selection of
--   proposer. We need this wrapper in order to allow to dispatch over
--   block data type since it only enters via non-injective type family
newtype ProposerSelection a = ProposerSelection
  { selectProposer :: ValidatorSet (Alg a) -> Height -> Round -> ValidatorIdx (Alg a)
  }

-- | Type class for data which could be put into blockchain's
--   block. Related types are defined as associated types
class ( Serialise a
      , Serialise (TX a)
      , Serialise (BlockchainState a)
      , CryptoHashable a
      , CryptoHashable (TX a)
      , CryptoHashable (BlockchainState a)
      , Exception      (BChError a)
      , Crypto (Alg a)
      ) => BlockData a where
  -- | Type of transaction used in blockchain. More precisely it's
  --   type of transaction which is submitted by user and stored in
  --   mempool.
  type TX a
  -- | Part of state of blockchain defined by user. Set of validators
  --   is passed separately as part of 'BChEval'.
  type BlockchainState a
  -- | Reason or rejection of transaction\/block.
  type BChError a
  -- | Monad used for evaluation of blockchain
  type BChMonad a :: * -> *
  -- | Crypto algorithms used by blockchain validators. Note that
  --   transactions doesn't necessarily need not to use same
  --   cryptography.
  type Alg a  
  -- | Logic of blockchain. It describes how to process block and
  --   validate transactions in mempool and how to generate new blocks.
  bchLogic          :: BChLogic (BChMonad a) a
  -- | Describe how to select proposer for given height and round.
  proposerSelection :: ProposerSelection a
  -- | Information about block that is written to block
  logBlockData :: a -> JSON.Object
  logBlockData = mempty

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
data Proposal a = Proposal
  { propHeight    :: !Height
    -- ^ Proposal height
  , propRound     :: !Round
    -- ^ Propoasl round
  , propTimestamp :: !Time
    -- ^ Time of proposal
  , propPOL       :: !(Maybe Round)
    -- ^ Proof of Lock for proposal
  , propBlockID   :: !(BlockID a)
    -- ^ Hash of proposed block
  }
  deriving stock    (Show, Eq, Ord, Generic)
  deriving anyclass (NFData, Serialise, JSON.ToJSON, JSON.FromJSON)
instance (CryptoHash (Alg a)) => CryptoHashable (Proposal a) where
  hashStep = genericHashStep "hschain"

-- | Type of vote. Used for type-tagging of votes
data VoteType
  = PreVote
  | PreCommit
  deriving stock    (Show, Eq, Ord, Generic)
  deriving anyclass (Serialise, JSON.FromJSON, JSON.ToJSON)

-- | Single vote cast validator. Type of vote is determined by its
--   type tag
data Vote (ty :: VoteType) a = Vote
  { voteHeight  :: !Height
  , voteRound   :: !Round
  , voteTime    :: !Time
  , voteBlockID :: !(Maybe (BlockID a))
  }
  deriving stock    (Show, Eq, Ord, Generic)
  deriving anyclass (NFData, JSON.ToJSON, JSON.FromJSON)

instance Serialise (Vote 'PreVote a) where
  encode = encodeVote 0
  decode = decodeVote 0

instance Serialise (Vote 'PreCommit a) where
  encode = encodeVote 1
  decode = decodeVote 1

encodeVote :: Word -> Vote ty a -> Encoding
encodeVote tag Vote{..}
  = encodeListLen 5
 <> encodeWord tag
 <> encode voteHeight
 <> encode voteRound
 <> encode voteTime
 <> encode voteBlockID

decodeVote :: Word -> Decoder s (Vote ty a)
decodeVote expectedTag = do
  len <- decodeListLen
  tag <- decodeWord
  case len of
    5 | tag == expectedTag -> Vote <$> decode <*> decode <*> decode <*> decode
      | otherwise -> fail ("Invalid Vote tag, expected: " ++ show expectedTag
                           ++ ", actual: " ++ show tag)
    _ -> fail "Invalid Vote encoding"


instance (CryptoHash (Alg a)) => CryptoHashable (Vote 'PreVote a) where
  hashStep Vote{..}
    = hashStep (UserType "hschain" "Vote@PreVote")
   <> hashStep voteHeight
   <> hashStep voteRound
   <> hashStep voteTime
   <> hashStep voteBlockID

instance (CryptoHash (Alg a)) => CryptoHashable (Vote 'PreCommit a) where
  hashStep Vote{..}
    = hashStep (UserType "hschain" "Vote@PreCommit")
   <> hashStep voteHeight
   <> hashStep voteRound
   <> hashStep voteTime
   <> hashStep voteBlockID



----------------------------------------------------------------
-- Blockchain logic
----------------------------------------------------------------

-- | Context for evaluation for blockchain. It's simply triple of
--   blockchain state, set of validators and something else.
data BChEval a x = BChEval
  { bchValue        :: !x
  , validatorSet    :: !(MerkleNode Identity (Alg a) (ValidatorSet (Alg a)))
  , blockchainState :: !(MerkleNode Identity (Alg a) (BlockchainState a))
  }
  deriving stock (Generic, Functor)

-- | Blockchain genesis. That is block at H=0, validator set for that
--   block and initial state.
type Genesis         a = BChEval a (Block a)

-- | Same as genesis but is used for validation of blocks at
--   H>0. Created as documentation. Validator set and state correspond to state 
type BlockValidation a = BChEval a (Block a)

-- | Block which is already validated. It uses same type as
--   'BlockValidation' but validator set and state correspons to the
--   state _after_ evaluation.
type ValidatedBlock  a = BChEval a (Block a)

-- | Result of block evaluation. We don't have any information beyond.
type EvaluationResult a = BChEval a ()

-- | Block proposal returned by callback. Note that block itself is
--   assmebled by consensus engine so we return only block payload.
type ProposedBlock   a = BChEval a a




deriving stock    instance (Show x, Crypto (Alg a), Show (PublicKey (Alg a)), Show (BlockchainState a)
                           ) => Show (BChEval a x)
deriving stock    instance (Eq x, Eq (PublicKey (Alg a)), Eq (BlockchainState a)
                           ) => Eq (BChEval a x)
deriving anyclass instance (NFData x, NFData (PublicKey (Alg a)), NFData (BlockchainState a)
                           ) => NFData (BChEval a x)
deriving anyclass instance (Crypto (Alg a), JSON.ToJSON x, JSON.ToJSON (BlockchainState a)
                           ) => JSON.ToJSON (BChEval a x)
deriving anyclass instance (Crypto (Alg a)
                           , JSON.FromJSON x
                           , JSON.FromJSON  (BlockchainState a)
                           , CryptoHashable (BlockchainState a)
                           ) => JSON.FromJSON (BChEval a x)
deriving anyclass instance ( Crypto (Alg a)
                           , Serialise x
                           , Serialise      (BlockchainState a)
                           , CryptoHashable (BlockchainState a)
                           ) => Serialise (BChEval a x)


-- | Parameters supplied by consensus engine for block generation
data NewBlock a = NewBlock
  { newBlockHeight   :: !Height
  , newBlockLastBID  :: !(BlockID a)
  , newBlockCommit   :: !(Maybe (Commit a))
  , newBlockEvidence :: ![ByzantineEvidence a]
  , newBlockState    :: !(MerkleNode Identity (Alg a) (BlockchainState a))
  , newBlockValSet   :: !(ValidatorSet (Alg a))
  }

-- | Description of interpretation of blockchain state. Evaluation of
--   blocks and transactions are done in the monad @q@ which is
--   assumed to provide access to current state of blockchain
data BChLogic m a = BChLogic
  { processTx     :: BChEval a (TX a) -> m ()
    -- ^ Process single transactions. Used only for validator of
    --   transactions in mempool.
  , processBlock  :: BlockValidation a -> m (EvaluationResult a)
    -- ^ Process and validate complete block.
  , generateBlock :: NewBlock a -> [TX a] -> m (ProposedBlock a)
    -- ^ Generate block from list of transactions.
  }

instance HoistDict BChLogic where
  hoistDict fun BChLogic{..} = BChLogic
    { processTx     = fmap fun processTx
    , processBlock  = fmap fun processBlock
    , generateBlock = (fmap . fmap) fun generateBlock
    }

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
  :: (Crypto alg, CryptoHashable a)
  => ValidatorIdx alg           -- ^ Key identifier
  -> PrivKey alg                -- ^ Key for signing
  -> a                          -- ^ Value to sign
  -> Signed sign alg a
signValue key privK a = Signed key (signHashed privK a) a

-- | Verify signature. It return Nothing if verification fails for any
--   reason. Note that since @Signed@ contain only fingerprint we need
--   to supply function for looking up public keys.
verifySignature
  :: (Crypto alg, CryptoHashable a)
  => ValidatorSet alg
     -- ^ Set of validators corresponding to signed value
  -> Signed 'Unverified alg a
     -- ^ Value for verifying signature
  -> Maybe (Signed 'Verified alg a)
verifySignature valSet val@(Signed idx sig a) = do
  Validator pubK _ <- validatorByIndex valSet idx
  guard  $ verifySignatureHashed pubK a sig
  return $ coerce val

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
