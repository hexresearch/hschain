{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE QuantifiedConstraints      #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeFamilies               #-}
-- |
-- Basic data types for PoW blockchain
module HSChain.PoW.Types where

import Control.DeepSeq
import Control.Monad.IO.Class
import Data.Time.Clock          (UTCTime)
import Data.Time.Clock.POSIX    (getPOSIXTime,posixSecondsToUTCTime)
import Data.Int
import Data.Word
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Aeson           as JSON
import qualified Codec.Serialise      as CBOR
import GHC.Generics (Generic)

import HSChain.Crypto
import HSChain.Crypto.Classes.Hash
import HSChain.Types.Merkle.Types

----------------------------------------------------------------
-- Primitives
----------------------------------------------------------------

-- | Height of block in blockchain. That is 
newtype Height = Height Int32
  deriving stock   (Show, Read, Generic, Eq, Ord)
  deriving newtype (NFData, CBOR.Serialise, JSON.ToJSON, JSON.FromJSON, Enum, CryptoHashable)

-- | Time in milliseconds since UNIX epoch.
newtype Time = Time Int64
  deriving stock   (Show, Read, Generic, Eq, Ord)
  deriving newtype (NFData, CBOR.Serialise, JSON.ToJSON, JSON.FromJSON, Enum, CryptoHashable)


-- | Get current time
getCurrentTime :: MonadIO m => m Time
getCurrentTime = do
  t <- liftIO getPOSIXTime
  return $! Time $ round $ 1000 * t

-- | Convert timestamp to UTCTime
timeToUTC :: Time -> UTCTime
timeToUTC (Time t) = posixSecondsToUTCTime (realToFrac t / 1000)


----------------------------------------------------------------
-- Block
----------------------------------------------------------------

-- | Core of blockchain implementation.
class ( Show   (Work b)
      , Ord    (Work b)
      , Monoid (Work b)
      , Show (BlockID b)
      , Ord  (BlockID b)
      , Transaction (Tx b)
      , MerkleMap b
      ) => BlockData b where
  -- | ID of block. Usually it should be just a hash but we want to
  --   leave some representation leeway for implementations. 
  data BlockID b
  -- | Measure of work performed for creation of block or chain of
  --   blocks. Monoid instance should represent addition
  data Work b

  -- | Transactions inside the block.
  data Tx b

  -- | Compute block ID out of block using only header.
  blockID :: IsMerkle f => GBlock b f -> BlockID b
  -- | Context free validation of header. It's mostly sanity check on
  --   header. 
  validateHeader :: Header b -> Bool
  validateBlock  :: Block  b -> Bool
  blockWork :: GBlock b f -> Work b

-- |Specification of a transaction in a block.
--
-- We may have to change their ordering within the block and
-- we have to check it is safe to do so. We'll do that by
-- computing what transactions are consuming and producing
-- and then topologically sort them into fronts we can reorder
-- individually.
class Transaction tx where

  -- |A set of resources transaction gets to consume and produce.
  -- We represent them as bytestrings for now.
  txConsumeProduceSets :: Ord a => tx -> ([a], [a])


-- | Generic block. This is just spine of blockchain, that is height
--   of block, hash of previous block and
data GBlock b f = GBlock
  { blockHeight :: !Height
  , prevBlock   :: !(Maybe (BlockID b))
  , blockData   :: !(b f)
  }
  deriving (Generic)

deriving stock instance (Show (BlockID b), Show (b f)) => Show (GBlock b f)

toHeader :: MerkleMap b => Block b -> Header b
toHeader = merkleMap merkleHashed

instance ( forall g. IsMerkle g => CryptoHashable (b g)
         , IsMerkle f
         , CryptoHashable (BlockID b)
         ) => CryptoHashable (GBlock b f) where
  hashStep = genericHashStep "hschain"

instance MerkleMap b => MerkleMap (GBlock b) where
  merkleMap f GBlock{..} = GBlock { blockData = merkleMap f blockData
                                  , ..
                                  }


type Header b = GBlock b Hashed
type Block  b = GBlock b IdNode


-- |The specification of proof-of-work algorithms for blockchains (the @bc@ type).
--
-- PoW algorithm for some blockchain has a type @Solution@ of a header-specific puzzle
-- associated with it.
-- The solution is a part of a block.
--
--
class Optimizable (PowParameters bc) => ProofOfWork bc where

  -- |The type of a puzzle solution.
  data Puzzle bc

  -- |The parameters of a PoW algorithm - it may be how work is split between
  -- parts of an algorithm, subalgorithms choice, etc.
  --
  -- See @Optimizable@ class.
  data PoWParameters bc

-- |How to estimate good parameters.
--
-- We use bayesian inference for that.
class Optimizable p where
  -- |Optimizable parameters should be representable as a vector of doubles
  -- Thus we need a projection that tells us 

  -- |Optimizable things can be split

----------------------------------------
-- instances
----------------------------------------

instance JSON.FromJSON (BlockID b) where
  parseJSON = undefined

instance JSON.ToJSON (BlockID b) where
  toJSON = undefined

instance ( IsMerkle f
         , CBOR.Serialise (BlockID b)
         , CBOR.Serialise (b f)
         ) => CBOR.Serialise (GBlock b f)

instance ( IsMerkle f
         , forall g. IsMerkle g => JSON.ToJSON (b g)
         ) => JSON.ToJSON (GBlock b f)

instance ( IsMerkle f
         , forall g. IsMerkle g => JSON.FromJSON (b g)
         ) => JSON.FromJSON (GBlock b f)
