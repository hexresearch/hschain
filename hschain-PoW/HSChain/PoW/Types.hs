{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
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
import Data.Time.Clock          (UTCTime)
import Data.Time.Clock.POSIX    (getPOSIXTime,posixSecondsToUTCTime)
import Data.Int
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

-- | Core of blockchain implementation.
class ( Show   (Work b)
      , Ord    (Work b)
      , Monoid (Work b)
      , Show      (BlockID b)
      , Ord       (BlockID b)
      , Serialise (BlockID b)
      , MerkleMap b
      ) => BlockData b where
  -- | ID of block. Usually it should be just a hash but we want to
  --   leave some representation leeway for implementations. 
  data BlockID b
  -- | Measure of work performed for creation of block or chain of
  --   blocks. Monoid instance should represent addition
  data Work b
  -- | Compute block ID out of block using only header.
  blockID :: IsMerkle f => GBlock b f -> BlockID b
  -- | Context free validation of header. It's mostly sanity check on
  --   header. 
  validateHeader :: Header b -> Bool
  validateBlock  :: Block  b -> Bool
  blockWork :: GBlock b f -> Work b

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


data Locator b = Locator [BlockID b]
  deriving stock (Generic)
deriving stock instance Show (BlockID b) => Show (Locator b)

instance (Serialise (BlockID b)) => Serialise (Locator b)

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
