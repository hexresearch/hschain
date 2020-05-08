{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE QuantifiedConstraints      #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}
-- |
-- Basic data types for PoW blockchain
module HSChain.PoW.Types where

import Codec.Serialise          (Serialise)
import Control.DeepSeq
import Control.Monad            (forever)
import Control.Monad.IO.Class
import Data.Bits
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
  deriving newtype ( NFData, Num, Real, Integral
                   , CBOR.Serialise, JSON.ToJSON, JSON.FromJSON, Enum, CryptoHashable)

-- | Time in milliseconds since UNIX epoch.
newtype Time = Time Int64
  deriving stock   (Read, Generic, Eq, Ord)
  deriving newtype (NFData, CBOR.Serialise, JSON.ToJSON, JSON.FromJSON, Enum, CryptoHashable)

-- | Useful constant to calculate durations.
timeSecond :: Time
timeSecond = Time 1000

instance Show Time where
  show = show . timeToUTC

-- | Get current time
getCurrentTime :: MonadIO m => m Time
getCurrentTime = do
  t <- liftIO getPOSIXTime
  return $! Time $ round $ 1000 * t

-- | Convert timestamp to UTCTime
timeToUTC :: Time -> UTCTime
timeToUTC (Time t) = posixSecondsToUTCTime (realToFrac t / 1000)

-- | Multiply time (usually duration) by some integer constant.
scaleTime :: Int64 -> Time -> Time
scaleTime i (Time y) = Time (i * y)


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

  -- | Compute block ID out of block using only header.
  blockID :: IsMerkle f => GBlock b f -> BlockID b

  -- | Validate header. Chain ending with parent block and current
  --   time are provided as parameters.
  validateHeader :: MonadIO m => BH b -> Time -> Header b -> m Bool

  -- | Context free validation of block which doesn't have access to
  --   state of blockchain. It should perform sanity checks.
  validateBlock  :: MonadIO m => Block  b -> m Bool

  -- | Amount of work in the block
  blockWork      :: GBlock b f -> Work

  -- | Target value of a block.
  blockTargetThreshold :: GBlock b f -> Target

  -- | How work difficulty should be adjusted.
  -- First part of tuple is the block interval, second is seconds
  -- this interval should have.
  targetAdjustmentInfo :: BH b -> (Height, Time)
  targetAdjustmentInfo _ = let n = 1024 in (Height n, scaleTime (120 * fromIntegral n) timeSecond)

  -- |Perform retargeting with rounding.
  --
  -- We provide default implementation similar to one in Bitcoin,
  -- except we treat mantissa as unsigned.
  thresholdRetarget :: BH b -> Target -> Time -> Target
  thresholdRetarget bh (Target currentThreshold) (Time delta') =
    Target roundedNewThreshold
    where
      (_, Time targetTimeDelta') = targetAdjustmentInfo bh
      delta = fromIntegral delta'
      targetTimeDelta = fromIntegral targetTimeDelta'
      -- new threshold = ceil $ current threshold * current time delta / target time delta
      -- we use Integers, thus div and addition.
      newThreshold = div (currentThreshold * delta + targetTimeDelta - 1) targetTimeDelta
      roundedNewThreshold = roundToThreeBytes (31 - 3)
      roundToThreeBytes 0 = newThreshold -- too low, can go with "denormalized" variant
      roundToThreeBytes n
        | shifted < 2 ^ (24 :: Int) = newThreshold .|. ones
        | otherwise = roundToThreeBytes (n - 1)
        where
          shifted = shiftR newThreshold (n * 8)
          ones = shiftL (1 :: Integer) (n * 8) - 1

-- |Parts of mining process.
--
-- We may see mining process as a way to make block header
-- valid. That means that block given for mining may not have
-- a valid header, but the block we have successfully mined should
-- have a valid header. We are talking about validity of a puzzle's
-- answer, as other header fields like timestamp, target, etc are
-- somewhat out of our control.
--
-- Mining is relatively time-consuming process, the search for
-- an answer of a puzzle may take time from tenth of second to
-- several seconds. This part is also heavily compute-intensive
-- and should not be interrupted all too often. These two
-- requirements gave us the following plan: we fetch a block to
-- mine (header may be invalid) and work on it for some time, reporting
-- back in case of success.
--
-- The monad @m@ implements a way to report blocks mined and
-- a way to get block to mine. Thus it must have access to mempool.
--
-- Please note that you can implement caching in @fetchBlock@ if current
-- chain leader has not changed. You can tweak only the "coinbase"-like
-- transaction in the block.
class (MonadIO m, BlockData b) => Mineable m b where

  -- | Tell the world we have a block!
  reportMiningSuccess :: Block b -> m ()

  -- | Fetch the block to mine. Remember, you do not need a header
  -- with valid puzzle answer here. Other parts of header like
  -- target value must be correct.
  fetchBlock :: m (Block b)

  -- | Adjust puzzle's answer. The adjustment process tries to
  -- find right puzzle answer that is smallest possible or below
  -- given threshold. The return value is a block with any answer
  -- if found, the result may not pass under threshold.
  --
  -- Actual hash, converted to @Target@ is second part of tuple.
  --
  -- One can use second part in a parameter search process. It is a
  -- proxy to how many hash attempts are done during search.
  adjustPuzzle :: Target -> Block b -> m (Maybe (Block b, Target))


-- | The mining loop.
miningLoop :: forall b m . Mineable m b => m ()
miningLoop = forever $ do
  (toMine :: Block b) <- fetchBlock
  let threshold = blockTargetThreshold toMine
  maybeAdjusted <- adjustPuzzle threshold toMine
  case maybeAdjusted of
    Just (minedBlock, hashAsTarget)
      | hashAsTarget <= threshold -> reportMiningSuccess minedBlock
    _ -> return ()

-- |Target - value computed during proof-of-work test must be lower
-- than this threshold. Target can be and must be rounded.
-- It is guaranteed to not to exceed some constant value.
newtype Target = Target { targetInteger :: Integer }
  deriving newtype (Eq, Ord, Show)

-- |Difficulty - how many (in average) computations are needed to
-- achieve the target.
newtype Difficulty = Difficulty { difficultyInteger :: Integer }

-- | Generic block. This is just spine of blockchain, that is height
--   of block, hash of previous block and a "block data" - application
--   of type functor to get some information about actual data, from
--   just Merkle tree root to... Merkle tree itself?
--
--   (usually block is Merkle tree of some transactions)
data GBlock b f = GBlock
  { blockHeight   :: !Height
  , blockTime     :: !Time
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
