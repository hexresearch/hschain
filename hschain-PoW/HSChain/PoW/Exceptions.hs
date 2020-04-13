{-# LANGUAGE DeriveAnyClass            #-}
{-# LANGUAGE DerivingStrategies        #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE StandaloneDeriving        #-}
-- |
module HSChain.PoW.Exceptions where

import Codec.Serialise  (DeserialiseFailure)
import Control.Exception
import Data.Text       (Text,unpack)
import HSChain.PoW.Types
import HSChain.Crypto  (Hash,CryptoHash)

----------------------------------------------------------------
--
----------------------------------------------------------------

-- | Exception is thrown when peer deviates from protocol
data PeerError
  = InvalidMessage  !DeserialiseFailure
  -- ^ Peer's message could not be decoded
  | UnrequestedResponce
  -- ^ Peer sends response without our request
  | InvalidResponce
  -- ^ Peer's response is invalid
  | HandshakeError

  | ProtocolError
  deriving stock    (Show)
  deriving anyclass (Exception)


----------------------------------------------------------------
--
----------------------------------------------------------------

-- | Database is corruped
data CorruptedDBError
  = DBMissingGenesis              -- ^ Genesis is missing
  | DBInvalidBlock                -- ^ Unable to deserialise the block.
  deriving stock    (Show)
  deriving anyclass (Exception)

-- | Internal error in algorithm. Could only occure either because of
--   bug or due to data corruption.
data InternalError
  = BlockchainStateUnavalable
  -- ^ Program is unsable to retrive blockchain state
  | UnableToCommit
  -- ^ Program fails to write block to disk due to invalid query
  --   (write transaction was rolled back)
  | UnexpectedRollback
  -- ^ Transaction which shouldn't be rolled back was rolled back
  | forall a. Exception a => InvalidBlockInWAL !a
  -- ^ Invalid block is stored in WAL
  | forall a. Exception a => InvalidBlockGenerated !a
  -- ^ Genration of block ended in failure
  | forall a. Exception a => CannotRewindState !a
  -- ^ State transitions already record in blockchain are not valid.
  | forall a. Exception a => TryingToCommitInvalidBlock !a
  -- ^ Engine tries to commit invalid block. That it we have commit
  --   for block that doesn't pass validation. That could happen due
  --   to bug in validation code or due to byzantine behavior of +2\/3
  --   validators
  | forall alg. CryptoHash alg => InconsistenceWhenRewinding !Height !Text !(Mismatch (Hash alg))
  -- ^ Incinsistency encountered when rewinding
deriving stock instance Show InternalError

-- | Data type which is used to display mismatch between expected
--   value and actual value.
data Mismatch a = Mismatch
  { expectedValue :: !a
  , actualValue   :: !a
  }
  deriving stock Show


instance Exception InternalError where
  displayException = \case
    BlockchainStateUnavalable -> "BlockchainStateUnavalable :: InternalError"
    UnableToCommit            -> "UnableToCommit :: InternalError"
    UnexpectedRollback        -> "UnexpectedRollback :: InternalError"
    InvalidBlockInWAL e -> unlines
      [ "InvalidBlockInWAL :: InternalError"
      , displayException e
      ]
    InvalidBlockGenerated e -> unlines
      [ "InvalidBlockGenerated :: InternalError"
      , displayException e
      ]
    CannotRewindState e -> unlines
      [ "CannotRewindState :: InternalError"
      , displayException e
      ]
    TryingToCommitInvalidBlock e -> unlines
      [ "TryingToCommitInvalidBlock :: InternalError"
      , displayException e
      ]
    InconsistenceWhenRewinding h t Mismatch{..} -> unlines
      [ "InconsistenceWhenRewinding :: InternalError"
      , "  H of block that caused problem:"
      , "    " ++ show h
      , "  Problem:"
      , "    " ++ unpack t
      , "  Expected hash (one that stored in block):"
      , "    " ++ show expectedValue
      , "  Hash of computed value:"
      , "    " ++ show actualValue
      ]
