{-# LANGUAGE RecordWildCards #-}
-- |
-- BFT time estimators
module HSChain.Types.BFTTime (
    commitTime
  ) where

import Control.Monad
import Data.Bits
import Data.List
import Data.Ord
import Data.Word
import qualified Data.List.NonEmpty as NE

import HSChain.Types.Blockchain
import HSChain.Types.Validators


-- | Calculate time of commit as median of time of votes where votes
--   are weighted according to voting power of corresponding
--   validators.
commitTime
  :: ValidatorSet alg -- ^ Set of validators for commit
  -> Time             -- ^ Time of previous block. Votes that aren't
                      --   cast later that that are discarded.
  -> Commit alg a     -- ^ Commit to calculate time
  -> Maybe Time
commitTime vset t0 Commit{..} = do
  votes <- forM commitPrecommits $ \sv -> do
    val <- validatorByIndex vset (signedKeyInfo sv)
    return ( validatorVotingPower val
           , signedValue sv
           )
  -- Here we discard invalid votes and calculate median time
  let times    = sortBy (comparing snd)
               $ [ (w,voteTime) | (w,Vote{..}) <- NE.toList votes
                                , voteTime > t0
                                , voteBlockID == Just commitBlockID
                                ]
      totPower = sum (fst <$> times)
      half     = fromIntegral $ totPower `div` 2
  case odd totPower of
    True  -> case zDrop half times of
      (_,t):_         -> return t
      _               -> Nothing
    False -> case zDrop (half - 1) times of
      (1,t1):(_,t2):_ -> return $ average t1 t2
      (_,t ):_        -> return t
      _               -> Nothing


average :: Time -> Time -> Time
average (Time t1) (Time t2) = Time $ (t1 `div` 2) + (t2 `div` 2) + (t1 .&. t2 .&. 1)

zDrop :: Word64 -> [(Word64,a)] -> [(Word64,a)]
zDrop _ [] = []
zDrop 0 xs = xs
zDrop i ((n,x):xs)
  | i >= n    = zDrop (i - n) xs
  | otherwise = (n-i, x) : xs
