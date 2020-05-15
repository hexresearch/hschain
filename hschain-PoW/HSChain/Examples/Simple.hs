{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}
-- |
--
module HSChain.Examples.Simple where

import Codec.Serialise      (Serialise, serialise)
import Control.Monad.IO.Class
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

data KV f = KV
  { kvData       :: !(MerkleNode f SHA256 [(Int,String)])
  , kvTarget     :: !Target
  , kvNonce      :: !BS.ByteString
  }
  deriving stock (Generic)
deriving stock instance Show1    f => Show (KV f)
deriving stock instance IsMerkle f => Eq   (KV f)
instance Serialise (KV Identity)
instance Serialise (KV Proxy)

blockWithoutNonce :: GBlock KV f -> GBlock KV f
blockWithoutNonce block@GBlock{..} =
  block { blockData = blockData { kvNonce = BS.empty } }

instance IsMerkle f => CryptoHashable (KV f) where
  hashStep = genericHashStep "hschain"

instance MerkleMap KV where
  merkleMap f KV{..} = KV { kvData = mapMerkleNode f kvData
                          , ..
                          }

instance BlockData KV where
  newtype BlockID KV = KV'BID (Hash SHA256)
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
retarget :: BH KV -> Target
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

mine :: Block KV -> IO (Maybe (Block KV))
mine b0@GBlock {..} = do
  maybeAnswerHash <- POWFunc.solve [LBS.toStrict $ serialise $ blockWithoutNonce b0] powCfg
  case maybeAnswerHash of
    Nothing -> return Nothing
    Just (answer, _hash) -> return $ Just $ b0 { blockData = blockData { kvNonce = answer } }
  
{-
  find (\b -> hash256AsTarget b <= tgt)
  [ let GBlock{..} = b0
    in  GBlock{ blockData = blockData { kvNonce = nonce }
              , ..
              }
  | nonce <- [minBound .. maxBound]
  ]
-}
  where
    powCfg = POWFunc.defaultPOWConfig
                     { POWFunc.powCfgTarget = targetInteger tgt }
    tgt = blockTargetThreshold b0


goBack :: Height -> BH b -> Maybe (BH b)
goBack (Height 0) bh = Just bh
goBack h          bh = goBack (pred h) =<< bhPrevious bh

