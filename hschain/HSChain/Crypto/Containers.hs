{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiWayIf                 #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE UndecidableInstances       #-}
-- |
-- Data types for
module HSChain.Crypto.Containers (
    -- * Sets of signed values
    SignedSet
  , InsertResult(..)
  , emptySignedSet
  , insertSigned
  , majority23
  , any23
    -- *
  , SignedSetMap
  , emptySignedSetMap
  , addSignedValue
  , majority23at
  , any23at
  , valuesAtR
  , toPlainMap
  ) where

import Control.Monad
import           Data.Foldable
import           Data.List         (find)
import           Data.Maybe        (fromMaybe)
import           Data.Word
import qualified Data.Map.Strict    as Map
import           Data.Map.Strict      (Map)

import HSChain.Crypto (SignedState(..))
import HSChain.Types
import qualified HSChain.Data.CIntMap as CIMap
import           HSChain.Data.CIntMap   (CIntMap)

----------------------------------------------------------------
-- Set of signed values
----------------------------------------------------------------

-- | Collection of signed values. It's intended to hold votes so only
--   one value per signature is allowed. Lookup is supported both by
--   values and by signer's fingerprint.
data SignedSet alg a k = SignedSet
  { vsetAddrMap    :: !(CIntMap (ValidatorIdx alg) (Signed 'Verified alg a))
    -- Set of all votes
  , vsetValMap     :: !(Map k (VoteGroup alg))
    -- Reverse mapping from 
  , vsetAccPower   :: !Integer
  , vsetValidators :: !(ValidatorSet alg)
  , vsetToKey      :: !(a -> k)
  }

data VoteGroup alg = VoteGroup
  { accOK     :: !Integer       -- Accumulated weight of good votes
  , votersOK  :: !ValidatorISet -- Set of voters with good votes
  }

instance (Show a) => Show (SignedSet alg a k) where
  showsPrec n = showsPrec n . vsetAddrMap

-- | Result of insertion into 'SignedSet'
data InsertResult b a
  = InsertOK       !a            -- ^ Insert is successful
  | InsertDup                    -- ^ Duplicate value. No change
  | InsertConflict !b            -- ^ Conflict during insertion
  | InsertUnknown                -- ^ Value is signed by unknown validator
  deriving (Show,Functor)

instance Applicative (InsertResult b) where
  pure  = InsertOK
  (<*>) = ap

instance Monad (InsertResult b) where
  return = InsertOK
  InsertOK a       >>= f = f a
  InsertDup        >>= _ = InsertDup
  InsertConflict b >>= _ = InsertConflict b
  InsertUnknown    >>= _ = InsertUnknown

-- | Create set of signed values
emptySignedSet
  :: ValidatorSet alg           -- ^ Set of validators
  -> (a -> k)                   -- ^ Key for grouping votes
  -> SignedSet alg a k
emptySignedSet = SignedSet CIMap.empty Map.empty 0

-- | Insert value into set of votes
insertSigned
  :: (Ord a, Ord k)
  => Signed 'Verified alg a
  -> SignedSet alg a k
  -> InsertResult (Signed 'Verified alg a) (SignedSet alg a k)
insertSigned sval SignedSet{..} =
  case validatorByIndex vsetValidators idx of
    -- We trying to insert value signed by unknown key
    Nothing -> InsertUnknown
    Just Validator{validatorVotingPower}
      -- We already have value signed by that key
      | Just v <- idx `CIMap.lookup` vsetAddrMap -> if
          | signedValue v == val -> InsertDup
          | otherwise            -> InsertConflict v
      -- OK insert value then
      | otherwise -> InsertOK SignedSet
          { vsetAddrMap  = CIMap.insert idx sval vsetAddrMap
          , vsetAccPower = vsetAccPower + validatorVotingPower
          , vsetValMap   = Map.alter (Just . updateGrp . fromMaybe nullVote) k vsetValMap
          , ..
          }
      where
        updateGrp VoteGroup{..} = VoteGroup
          { accOK    = accOK + validatorVotingPower
          , votersOK = insertValidatorIdx idx votersOK
          }
        nullVote = VoteGroup 0 $ emptyValidatorISet vsetValidators
  where
    idx = signedKeyInfo sval
    val = signedValue   sval
    k   = vsetToKey      val


-- | Returns whether we have +2\/3 majority of votes for some value.
majority23
  :: SignedSet alg a k
  -> Maybe k
majority23 SignedSet{..} = do
  (k,_) <- find maj23 $ Map.toList vsetValMap
  return k
  where
    maj23 (_, VoteGroup{..}) = accOK >= quorum
    quorum = 2 * totalVotingPower vsetValidators `div` 3 + 1

-- | Whether we have +2\/3 of votes in total which are distributed in
--   any manner. They need not to vote for the same value.
any23
  :: SignedSet alg a k
  -> Bool
any23 SignedSet{..}
  = vsetAccPower >= quorum
  where
    quorum = 2 * totalVotingPower vsetValidators `div` 3 + 1



----------------------------------------------------------------
--
----------------------------------------------------------------


-- | Map from @r@ to @SignedSet ty alg a@. It maintains invariant that
--   all submaps have same distribution of voting power
data SignedSetMap r alg a k = SignedSetMap
  { vmapSubmaps    :: !(Map r (SignedSet alg a k))
  , vmapValidators :: !(ValidatorSet alg)
  , vmapToKey      :: !(a -> k)
  }

instance (Show a, Show r) => Show (SignedSetMap r alg a k) where
  showsPrec n = showsPrec n . vmapSubmaps

emptySignedSetMap
  :: ValidatorSet alg
  -> (a -> k)
  -> SignedSetMap r alg a k
emptySignedSetMap = SignedSetMap Map.empty

-- | Convert collection of signed values to plain map
toPlainMap
  :: SignedSetMap r alg a k
  -> Map r (CIntMap (ValidatorIdx alg) (Signed 'Verified alg a))
toPlainMap = fmap vsetAddrMap . vmapSubmaps

addSignedValue
  :: (Ord r, Ord a, Ord k)
  => r
  -> Signed 'Verified alg a
  -> SignedSetMap r alg a k
  -> InsertResult (Signed 'Verified alg a) (SignedSetMap r alg a k)
addSignedValue r a sm@SignedSetMap{..} = do
  m <- insertSigned a
     $ fromMaybe (emptySignedSet vmapValidators vmapToKey)
     $ Map.lookup r vmapSubmaps
  return sm { vmapSubmaps = Map.insert r m vmapSubmaps }

majority23at
  :: (Ord r)
  => r
  -> SignedSetMap r alg a k
  -> Maybe k
majority23at r SignedSetMap{..}
  = majority23 =<< Map.lookup r vmapSubmaps

any23at
  :: (Ord r)
  => r
  -> SignedSetMap r alg a k
  -> Bool
any23at r SignedSetMap{..}
  = maybe False any23 $ Map.lookup r vmapSubmaps

valuesAtR
  :: (Ord r)
  => r
  -> SignedSetMap r alg a k
  -> [Signed 'Verified alg a]
valuesAtR r SignedSetMap{..} =
  case Map.lookup r vmapSubmaps of
    Nothing            -> []
    Just SignedSet{..} -> toList vsetAddrMap
