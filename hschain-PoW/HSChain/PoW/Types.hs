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
import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import Data.Bits
import qualified Data.ByteString as BS
import Data.Monoid              (Sum(..))
import Data.Typeable            (Typeable)
import Data.Time.Clock          (UTCTime)
import Data.Time.Clock.POSIX    (getPOSIXTime,posixSecondsToUTCTime)
import Data.Int
import qualified Database.SQLite.Simple.ToField   as SQL
import qualified Database.SQLite.Simple.FromField as SQL
import Numeric.Natural
import qualified Data.Aeson      as JSON
import qualified Codec.Serialise as CBOR
import GHC.Generics (Generic)
import Text.Printf (printf)

import HSChain.Crypto
import HSChain.Crypto.SHA
import HSChain.Crypto.Classes.Hash
import HSChain.Types.Merkle.Types

----------------------------------------------------------------
-- Primitives
----------------------------------------------------------------

-- | Height of block in blockchain.
newtype Height = Height Int32
  deriving stock   (Show, Read, Generic, Eq, Ord)
  deriving newtype ( NFData, Num, Real, Integral, Enum
                   , CBOR.Serialise, JSON.ToJSON, JSON.FromJSON, CryptoHashable
                   , SQL.FromField, SQL.ToField)

-- | Time in milliseconds since UNIX epoch.
newtype Time = Time Int64
  deriving stock   (Read, Generic, Eq, Ord)
  deriving newtype (NFData, Enum
                   , CBOR.Serialise, JSON.ToJSON, JSON.FromJSON, CryptoHashable
                   , SQL.FromField, SQL.ToField)

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
class ( Show (BlockID b), Ord (BlockID b), Serialise (BlockID b)
      , Show (TxID    b), Ord (TxID    b), Serialise (TxID    b)
      , Show (Tx b)
      , JSON.ToJSON (BlockID b), JSON.FromJSON (BlockID b)
      , JSON.ToJSON (TxID    b), JSON.FromJSON (TxID    b)
      , MerkleMap b, Typeable b
      , Exception (BlockException b), JSON.ToJSON (BlockException b)
      ) => BlockData b where

  -- | ID of block. Usually it should be just a hash but we want to
  --   leave some representation leeway for implementations.
  data BlockID b

  -- | Transaction ID. Again it's usually some hash of transaction.
  data TxID b

  -- | Error during evaluation of block. It's mostly to produce
  --   reasonable informative error messages.
  type BlockException b

  -- | Transactions that constitute block.
  type Tx b

  -- | Compute block ID out of block using only header.
  blockID :: IsMerkle f => GBlock b f -> BlockID b
  -- | Compute ID of a transaction.
  txID :: Tx b -> TxID b
  -- | List of transactions in block
  blockTransactions :: Block b -> [Tx b]


  -- | Context free validation of a transaction. It should perform all
  --   validations that are possible given only transacion.
  validateTxContextFree :: Tx b -> Either (BlockException b) ()

  -- | Validate header. Chain ending with parent block and current
  --   time are provided as parameters.
  validateHeader :: MonadIO m => BH b -> Time -> Header b -> m (Either (BlockException b) ())

  -- | Context free validation of block which doesn't have access to
  --   state of blockchain. It should perform sanity checks.
  validateBlock  :: MonadIO m => Block  b -> m (Either (BlockException b) ())

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
class BlockData b => Mineable b where

  -- | Adjust puzzle's answer. The adjustment process tries to
  -- find right puzzle answer that is smallest possible or below
  -- given threshold. The return value is a block with any answer
  -- if found, the result may not pass under threshold.
  --
  -- Actual hash, converted to @Target@ is second part of tuple.
  --
  -- One can use second part in a parameter search process. It is a
  -- proxy to how many hash attempts are done during search.
  adjustPuzzle :: MonadIO m => Block b -> m (Maybe (Block b), Target)


-- |Target - value computed during proof-of-work test must be lower
-- than this threshold. Target can be and must be rounded.
-- It is guaranteed to not to exceed some constant value.
newtype Target = Target { targetInteger :: Integer }
  deriving newtype (Eq, Ord)
  deriving newtype (CryptoHashable, Serialise)

instance Show Target where
  show (Target i) = printf "%064x" i

-- |Difficulty - how many (in average) computations are needed to
-- achieve the target.
newtype Difficulty = Difficulty { difficultyInteger :: Integer }
  deriving newtype (Eq, Ord, Show)
  deriving newtype CryptoHashable

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

---------------------------------------
-- Handy utilities.
---------------------------------------

-- FIXME: correctly compute rertargeting
retarget :: BlockData b => BH b -> Target
retarget bh
  -- Retarget
  | bhHeight bh `mod` adjustInterval == 0
  , Just old <- goBack adjustInterval bh
  , bhHeight old /= 0
  =   let Time t1 = bhTime old
          Time t2 = bhTime bh
          tgt     = targetInteger oldTarget
          tgt'    = (tgt * fromIntegral (t2 - t1)) `div` (fromIntegral adjustInterval * fromIntegral seconds)
      in Target tgt'
  | otherwise
    = oldTarget
  where
    oldTarget = blockTargetThreshold $ asHeader bh
    (adjustInterval, Time seconds) = targetAdjustmentInfo bh

hash256AsTarget :: CryptoHashable a => a -> Target
hash256AsTarget a
  = Target $ BS.foldl' (\i w -> (i `shiftL` 8) + fromIntegral  w) 0 bs
  where
    Hash bs = hash a :: Hash SHA256

goBack :: Height -> BH b -> Maybe (BH b)
goBack (Height 0) = Just
goBack h          = goBack (pred h) <=< bhPrevious
