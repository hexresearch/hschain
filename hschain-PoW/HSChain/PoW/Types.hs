{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE QuantifiedConstraints      #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}
-- |
-- Basic data types for PoW blockchain
module HSChain.PoW.Types where

import Codec.Serialise          (Serialise)
import Control.DeepSeq
import Control.Monad.IO.Class
import Data.Monoid              (Sum(..))
import Data.Time.Clock          (UTCTime)
import Data.Time.Clock.POSIX    (getPOSIXTime,posixSecondsToUTCTime)
import Data.Int
import Numeric.Natural
import qualified Data.Aeson      as JSON
import qualified Codec.Serialise as CBOR
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

-- | Measure of work performed for creation of block or chain of
--   blocks. Monoid instance should represent addition
newtype Work = Work Natural
  deriving stock   (Show,Eq,Ord)
  deriving newtype (CryptoHashable,Serialise)
  deriving         (Semigroup,Monoid) via (Sum Natural)


-- | Core of blockchain implementation.
class ( Show      (BlockID b)
      , Ord       (BlockID b)
      , Serialise (BlockID b)
      , JSON.ToJSON (BlockID b)
      , JSON.FromJSON (BlockID b)
      , MerkleMap b
      ) => BlockData b where
  -- | ID of block. Usually it should be just a hash but we want to
  --   leave some representation leeway for implementations. 
  data BlockID b
  -- | Configuration of chain
  data ChainConfig b
  -- | Compute block ID out of block using only header.
  blockID :: IsMerkle f => GBlock b f -> BlockID b
  -- | Validate header. Chain ending with parent block and current
  --   time are provided as parameters.
  validateHeader :: MonadIO m => ChainConfig b -> BH b -> Time -> Header b -> m Bool
  -- | Context free validation of block which doesn't have access to
  --   state of blockchain. It should perform sanity checks.
  validateBlock  :: MonadIO m => ChainConfig b -> Block  b -> m Bool
  -- | Amount of work in the block
  blockWork      :: GBlock b f -> Work

-- | Generic block. This is just spine of blockchain, that is height
--   of block, hash of previous block and a "block data" - application
--   of type functor to get some information about actual data, from
--   just Merkle tree root to... Merkle tree itself?
--
--   (usually block is Merkle tree of some transactions)
data GBlock b f = GBlock
  { blockHeight   :: !Height
  , blockTime   :: !Time
  , prevBlock     :: !(Maybe (BlockID b))
  , blockData     :: !(b f)
  }
  deriving (Generic)

deriving stock instance (Eq (BlockID b), Eq (b f)) => Eq (GBlock b f)
deriving stock instance (Show (BlockID b), Show (b f)) => Show (GBlock b f)


-- | Unpacked header for storage in block index. We use this data type
--   instead of @[(BlockID, Header b)]@ in order to reduce memory use
--   since we'll keep many thousands on these values in memory.
data BH b = BH
  { bhHeight   :: !Height         --
  , bhTime     :: !Time
  , bhBID      :: !(BlockID b)    --
  , bhWork     :: !Work           --
  , bhPrevious :: !(Maybe (BH b)) --
  , bhData     :: !(b Proxy)      --
  }

asHeader :: BH b -> Header b
asHeader bh = GBlock
  { blockHeight = bhHeight bh
  , blockTime   = bhTime bh
  , prevBlock   = bhBID <$> bhPrevious bh
  , blockData   = bhData bh
  }

deriving instance (Show (BlockID b), Show (b Proxy)) => Show (BH b)

instance BlockData b => Eq (BH b) where
  a == b = bhBID a == bhBID b



toHeader :: MerkleMap b => Block b -> Header b
toHeader = merkleMap (const Proxy)

instance ( forall g. IsMerkle g => CryptoHashable (b g)
         , IsMerkle f
         , CryptoHashable (BlockID b)
         ) => CryptoHashable (GBlock b f) where
  hashStep = genericHashStep "hschain"

instance MerkleMap b => MerkleMap (GBlock b) where
  merkleMap f GBlock{..} = GBlock { blockData = merkleMap f blockData
                                  , ..
                                  }


type Header b = GBlock b Proxy
type Block  b = GBlock b Identity


data Locator b = Locator [BlockID b]
  deriving stock (Generic)
deriving stock instance Eq   (BlockID b) => Eq   (Locator b)
deriving stock instance Show (BlockID b) => Show (Locator b)

instance (Serialise (BlockID b)) => Serialise (Locator b)

----------------------------------------
-- instances
----------------------------------------

instance ( IsMerkle f
         , CBOR.Serialise (BlockID b)
         , CBOR.Serialise (b f)
         ) => CBOR.Serialise (GBlock b f)

instance ( IsMerkle f
         , JSON.ToJSON (BlockID b)
         , forall g. IsMerkle g => JSON.ToJSON (b g)
         ) => JSON.ToJSON (GBlock b f)

instance ( IsMerkle f
         , JSON.FromJSON (BlockID b)
         , forall g. IsMerkle g => JSON.FromJSON (b g)
         ) => JSON.FromJSON (GBlock b f)
