{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE NumDecimals           #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}
-- |
-- Basic data types for PoW blockchain
module HSChain.PoW.Types
  ( -- * Primitives
    Height(..)
    -- * Time units
  , Time(..)
  , DTime(..)
  , getCurrentTime
  , timeToUTC
  , (.-.)
  , (.+)
  , (.-)
  , timeSeconds
  , timeMinutes
  , timeHours
  , timeDays
    -- * Work measure
  , Work(..)
  , Target(..)
    -- * Block & BlockData
  , Header
  , Block
  , GBlock(..)
  , toHeader
  , BlockData(..)
  , Mineable(..)
  , retarget
    -- * Block index
  , BH(..)
  , asHeader
  , Locator(..)
    -- * General utils
  , hash256AsTarget
    -- * Lenses
  , blockHeightL, blockTimeL, prevBlockL, blockDataL
  ) where

import Codec.Serialise          (Serialise)
import Control.Lens             (lens,Lens,Lens',(%~))
import Control.DeepSeq
import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import Data.Bits
import qualified Data.ByteString as BS
import Data.Monoid              (Sum(..))
import Data.Semigroup           (Semigroup(..))
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
  deriving stock   ( Generic, Eq, Ord)
  deriving newtype ( NFData, CryptoHashable
                   , CBOR.Serialise, JSON.ToJSON, JSON.FromJSON
                   , SQL.FromField, SQL.ToField)

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

-- | Time difference in milliseconds.
newtype DTime = DTime Int64
  deriving stock   ( Generic, Show, Read, Eq, Ord)
  deriving (Semigroup, Monoid) via Sum Int64

-- | Compute difference between two timestamps
(.-.) :: Time -> Time -> DTime
Time t1 .-. Time t2 = DTime (t2 - t1)

-- | Add difference to a time stamp
(.+) :: Time -> DTime -> Time
Time t .+ DTime dt = Time (t + dt)

-- | Subtract difference to a time stamp
(.-) :: Time -> DTime -> Time
Time t .- DTime dt = Time (t - dt)

timeSeconds,timeMinutes,timeHours,timeDays :: Int -> DTime
timeSeconds = DTime . (*    1e3) . fromIntegral
timeMinutes = DTime . (*   60e3) . fromIntegral
timeHours   = DTime . (* 3600e3) . fromIntegral
timeDays    = DTime . (*86400e3) . fromIntegral


-- | Measure of work performed for creation of block or chain of
--   blocks. Monoid instance should represent addition
newtype Work = Work Natural
  deriving stock   (Show,Eq,Ord)
  deriving newtype (CryptoHashable,Serialise)
  deriving         (Semigroup,Monoid) via (Sum Natural)


-- |Target - value computed during proof-of-work test must be lower
-- than this threshold. Target can be and must be rounded.
-- It is guaranteed to not to exceed some constant value.
newtype Target = Target { targetInteger :: Integer }
  deriving newtype (Eq, Ord)
  deriving newtype (CryptoHashable, Serialise)

instance Show Target where
  show (Target i) = printf "%064x" i


----------------------------------------------------------------
-- Block
----------------------------------------------------------------

-- | Full block
type Block  b = GBlock b Identity

-- | Header. Block without transactions etc
type Header b = GBlock b Proxy

-- | Generic block. This is just spine of blockchain, that is height
--   of block, hash of previous block and a "block data" - application
--   of type functor to get some information about actual data, from
--   just Merkle tree root to... Merkle tree itself?
--
--   (usually block is Merkle tree of some transactions)
data GBlock b (f :: (* -> *)) = Block
  { blockHeight   :: !Height
  , blockTime     :: !Time
  , prevBlock     :: !(Maybe (BlockID b))
  , blockData     :: !(b f)
  }
  deriving (Generic)

-- | Convert block to header
toHeader :: MerkleMap b => Block b -> Header b
toHeader = merkleMap (const Proxy)

deriving stock instance (Eq   (BlockID b), Eq   (b f)) => Eq   (GBlock b f)
deriving stock instance (Show (BlockID b), Show (b f)) => Show (GBlock b f)

instance ( forall g. IsMerkle g => CryptoHashable (b g)
         , IsMerkle f
         , CryptoHashable (BlockID b)
         ) => CryptoHashable (GBlock b f) where
  hashStep = genericHashStep "hschain"

instance MerkleMap b => MerkleMap (GBlock b) where
  merkleMap f = blockDataL %~ merkleMap f

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


----------------------------------------------------------------
-- BlockData type class
----------------------------------------------------------------

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
  targetAdjustmentInfo :: BH b -> (Height, DTime)
  targetAdjustmentInfo _
    = ( Height n
      , stimes n (timeMinutes 1)
      )
    where
      n = 1024

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


-- | Unpacked header for storage in block index. We use this data type
--   instead of @[(BlockID, Header b)]@ in order to reduce memory use
--   since we'll keep many thousands on these values in memory.
data BH b = BH
  { bhHeight   :: !Height         -- ^ Height of block
  , bhTime     :: !Time           -- ^ Time of block
  , bhBID      :: !(BlockID b)    -- ^ Cached block ID
  , bhWork     :: !Work           -- ^ Amount of work in block
  , bhPrevious :: !(Maybe (BH b)) -- ^ Previous block
  , bhData     :: !(b Proxy)      -- ^ Header part of user-defined data
  }

-- | Convert 'BH' to proper header.
asHeader :: BH b -> Header b
asHeader bh = Block
  { blockHeight = bhHeight bh
  , blockTime   = bhTime bh
  , prevBlock   = bhBID <$> bhPrevious bh
  , blockData   = bhData bh
  }

deriving instance (Show (BlockID b), Show (b Proxy)) => Show (BH b)

instance BlockData b => Eq (BH b) where
  a == b = bhBID a == bhBID b


data Locator b = Locator [BlockID b]
  deriving stock (Generic)
deriving stock instance Eq   (BlockID b) => Eq   (Locator b)
deriving stock instance Show (BlockID b) => Show (Locator b)

instance (Serialise (BlockID b)) => Serialise (Locator b)


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
    = let DTime actualInterval  = bhTime bh .-. bhTime old
          DTime desiredInterval = stimes adjustInterval blockInterval
          Target tgt            = oldTarget
      in Target $ (tgt * fromIntegral actualInterval)
            `div` (fromIntegral desiredInterval)
  | otherwise
    = oldTarget
  where
    oldTarget = blockTargetThreshold $ asHeader bh
    (adjustInterval, blockInterval) = targetAdjustmentInfo bh

hash256AsTarget :: CryptoHashable a => a -> Target
hash256AsTarget a
  = Target $ BS.foldl' (\i w -> (i `shiftL` 8) + fromIntegral  w) 0 bs
  where
    Hash bs = hash a :: Hash SHA256

goBack :: Height -> BH b -> Maybe (BH b)
goBack (Height 0) = Just
goBack h          = goBack (pred h) <=< bhPrevious


----------------------------------------------------------------
-- Lens
----------------------------------------------------------------

blockHeightL :: Lens' (GBlock b f) Height
blockHeightL = lens blockHeight (\b x -> b { blockHeight = x })

blockTimeL :: Lens' (GBlock b f) Time
blockTimeL = lens blockTime (\b x -> b { blockTime = x })

prevBlockL :: Lens' (GBlock b f) (Maybe (BlockID b))
prevBlockL = lens prevBlock (\b x -> b { prevBlock = x })

blockDataL :: Lens (GBlock b f) (GBlock b g) (b f) (b g)
blockDataL = lens blockData (\b x -> b { blockData = x })
