{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}
-- |
-- Simple block which implements write only key-value storage. It's
-- only use is to test and debug PoW algorithms.
module HSChain.Examples.Simple
  ( KV(..)
  , KVConfig(..)
  , retarget
  , mine
  ) where

import Codec.Serialise      (Serialise, serialise)
import Control.Monad.IO.Class
import Control.Applicative
import Control.Monad
import Data.Bits
import Data.Functor.Classes (Show1)
import Data.List            (find)
import Data.Word
import qualified Data.Aeson           as JSON
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString      as BS
import Numeric.Natural
import GHC.Generics         (Generic)

import HSChain.Crypto
import HSChain.Crypto.Classes.Hash
import HSChain.Crypto.SHA
import HSChain.Types.Merkle.Types
import HSChain.PoW.Types
import qualified HSChain.POW as POWFunc

import Debug.Trace

----------------------------------------------------------------
--
----------------------------------------------------------------

-- | Simple block which contains key-value pairs. Work function is
--   simple SHA256 a la bitcoin
data KV cfg f = KV
  { kvData       :: !(MerkleNode f SHA256 [(Int,String)])
  -- ^ List of key-value pairs
  , kvTarget     :: !Target
  -- ^ Nonce which is used to get
  , kvNonce      :: !BS.ByteString
  -- ^ Current difficulty of mining. It means a complicated thing
  -- right now.
  }
  deriving stock (Generic)
deriving stock instance Show1    f => Show (KV cfg f)
deriving stock instance IsMerkle f => Eq   (KV cfg f)
instance Serialise (KV cfg Identity)
instance Serialise (KV cfg Proxy)

blockWithoutNonce :: GBlock (KV cfg) f -> GBlock (KV cfg) f
blockWithoutNonce block@GBlock{..} =
  block { blockData = blockData { kvNonce = BS.empty } }

instance IsMerkle f => CryptoHashable (KV cfg f) where
  hashStep = genericHashStep "hschain"

instance MerkleMap (KV cfg) where
  merkleMap f KV{..} = KV { kvData = mapMerkleNode f kvData
                          , ..
                          }

-- | We may need multiple chains (main chain, test chain(s)) which may
--   use different difficulty adjustment algorithms etc.
class KVConfig cfg where
  -- | Difficulty adjustment is performed every N of blocks
  kvAdjustInterval :: Const Height  cfg
  -- | Expected interval between blocks in milliseconds
  kvBlockInterval  :: Const Natural cfg


instance KVConfig cfg => BlockData (KV cfg) where
  newtype BlockID (KV cfg) = KV'BID (Hash SHA256)
    deriving newtype (Show,Eq,Ord,CryptoHashable,Serialise, JSON.ToJSON, JSON.FromJSON)
  blockID b = let Hashed h = hashed b in KV'BID h
  validateHeader bh (Time now) header
    | blockHeight header == 0 = return True -- skip genesis check.
    | otherwise = do
      answerIsGood <- liftIO $ POWFunc.check onlyHeader answer hashOfSum powCfg
      return
        $ and
              [ answerIsGood
              , kvTarget (blockData header) == retarget bh
              -- Time checks
              , t <= now + (2*60*60*1000)
              -- FIXME: Check that we're ahead of median time of N prev block
              ]
    where
      powCfg = POWFunc.defaultPOWConfig
                       { POWFunc.powCfgTarget = targetInteger tgt }
      tgt = blockTargetThreshold header
      onlyHeader = LBS.toStrict $ serialise $ blockWithoutNonce header
      answer = kvNonce $ blockData header
      Hash hashOfSum = hash headerAndAnswer :: Hash SHA256
      headerAndAnswer = BS.concat [onlyHeader, answer]
      Time t = blockTime header
  --
  validateBlock  _ = return True
  blockWork      b = Work $ fromIntegral $ ((2^(256 :: Int)) `div`)
                          $ targetInteger $ kvTarget $ blockData b
  blockTargetThreshold b = Target $ fromIntegral $ 2^(256::Int) `div` targetInteger (kvTarget (blockData b))



-- FIXME: correctly compute rertargeting
retarget :: KVConfig cfg => BH (KV cfg) -> Target
retarget bh
  -- Retarget
  | bhHeight bh `mod` adjustInterval == 0
  , Just old <- goBack adjustInterval bh
  , bhHeight old /= 0
  =   let Time t1 = bhTime old
          Time t2 = bhTime bh
          tgt     = targetInteger oldTarget
          tgt'    = (tgt * fromIntegral (t2 - t1)) `div` (fromIntegral adjustInterval * fromIntegral seconds)
      in traceShowId $ Target tgt'
  | otherwise
    = oldTarget
  where
    oldTarget = kvTarget $ bhData bh
    (adjustInterval, Time seconds) = targetAdjustmentInfo bh

hash256AsTarget :: CryptoHashable a => a -> Target
hash256AsTarget a
  = Target $ BS.foldl' (\i w -> (i `shiftL` 8) + fromIntegral  w) 0 bs
  where
    Hash bs = hash a :: Hash SHA256

mine :: KVConfig cfg => Block (KV cfg) -> IO (Maybe (Block (KV cfg)))
mine b0@GBlock {..} = do
  maybeAnswerHash <- POWFunc.solve [LBS.toStrict $ serialise $ blockWithoutNonce b0] powCfg
  case maybeAnswerHash of
    Nothing -> return Nothing
    Just (answer, _hash) -> return $ Just $ b0 { blockData = blockData { kvNonce = answer } }
  where
    powCfg = POWFunc.defaultPOWConfig
                     { POWFunc.powCfgTarget = targetInteger tgt }
    tgt = blockTargetThreshold b0


goBack :: Height -> BH b -> Maybe (BH b)
goBack (Height 0) = Just
goBack h          = goBack (pred h) <=< bhPrevious
