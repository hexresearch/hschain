{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiWayIf                 #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE UndecidableInstances       #-}
-- |
-- Data types for
module Thundermint.Crypto.Containers (
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
import qualified Data.Map.Strict    as Map
import           Data.Map.Strict      (Map)
import qualified Data.Set           as Set
import           Data.Set             (Set)

import Thundermint.Types
import qualified Thundermint.Data.CIntMap as CIMap
import           Thundermint.Data.CIntMap   (CIntMap)

----------------------------------------------------------------
-- Set of signed values
----------------------------------------------------------------

-- | Collection of signed values. It's intended to hold votes so only
--   one value per signature is allowed. Lookup is supported both by
--   values and by signer's fingerprint.
data SignedSet ty alg a k = SignedSet
  { vsetAddrMap    :: !(CIntMap (ValidatorIdx alg) (Signed (ValidatorIdx alg) ty alg a))
    -- Set of all votes
  , vsetValMap     :: !(Map k (VoteGroup alg))
    -- Reverse mapping from 
  , vsetAccPower   :: !Integer
  , vsetValidators :: !(ValidatorSet alg)
  , vsetToKey      :: !(a -> k)
  , vsetValueOK    :: !(a -> Bool)
  }

data VoteGroup alg = VoteGroup
  { accOK     :: !Integer             -- Accumulated weight of good votes
  , accBad    :: !Integer             -- Accumulated weight of invalid votes
  , votersOK  :: !(Set (ValidatorIdx alg)) -- Set of voters with good votes
  , votersBad :: !(Set (ValidatorIdx alg)) -- Set of voters with invalid votes
  }

instance (Show a) => Show (SignedSet ty alg a k) where
  showsPrec n = showsPrec n . vsetAddrMap

-- | Result of insertion into 'SignedSet'
data InsertResult b a
  = InsertOK       !a            -- ^ Insert is successful
  | InsertDup                    -- ^ Duplicate value. No change
  | InsertConflict !b            -- ^ Conflict during insertion
  | InsertUnknown  !b            -- ^ Value is signed by unknown validator
  deriving (Show,Functor)

instance Applicative (InsertResult b) where
  pure  = InsertOK
  (<*>) = ap

instance Monad (InsertResult b) where
  return = InsertOK
  InsertOK a       >>= f = f a
  InsertDup        >>= _ = InsertDup
  InsertConflict b >>= _ = InsertConflict b
  InsertUnknown  b >>= _ = InsertUnknown  b

-- | Create set of signed values
emptySignedSet
  :: ValidatorSet alg           -- ^ Set of validators
  -> (a -> k)                   -- ^ Key for grouping votes
  -> (a -> Bool)                -- ^ Additional validation for vote
  -> SignedSet ty alg a k
emptySignedSet = SignedSet CIMap.empty Map.empty 0

-- | Insert value into set of votes
insertSigned
  :: (Ord a, Ord k)
  => Signed (ValidatorIdx alg) ty alg a
  -> SignedSet ty alg a k
  -> InsertResult (Signed (ValidatorIdx alg) ty alg a) (SignedSet ty alg a k)
insertSigned sval SignedSet{..} =
  case validatorByIndex vsetValidators idx of
    -- We trying to insert value signed by unknown key
    Nothing -> InsertUnknown sval
    Just validator
      -- We already have value signed by that key
      | Just v <- idx `CIMap.lookup` vsetAddrMap -> if
           | signedValue v == val -> InsertDup
           | otherwise            -> InsertConflict sval
      -- OK insert value then
      | otherwise -> InsertOK SignedSet
          { vsetAddrMap  = CIMap.insert idx sval vsetAddrMap
          , vsetAccPower = vsetAccPower + validatorVotingPower validator
          , vsetValMap   =
              let upd VoteGroup{..}
                    | vsetValueOK val = Just VoteGroup
                                        { accOK    = accOK + validatorVotingPower validator
                                        , votersOK = Set.insert idx votersOK
                                        , ..
                                        }
                    | otherwise       = Just VoteGroup
                                        { accBad    = accBad + validatorVotingPower validator
                                        , votersBad = Set.insert idx votersBad
                                        , ..
                                        }
              in Map.alter (upd . fromMaybe nullVote) k vsetValMap
          , ..
          }
  where
    idx      = signedKeyInfo sval
    val      = signedValue   sval
    k        = vsetToKey      val
    nullVote = VoteGroup 0 0 Set.empty Set.empty

-- | We have +2\/3 majority of votes return vote for
majority23
  :: SignedSet ty alg a k
  -> Maybe k
majority23 SignedSet{..} = do
  (k,_) <- find maj23 $ Map.toList vsetValMap
  return k
  where
    maj23 (_, VoteGroup{..}) = accOK >= quorum
    quorum = 2 * totalVotingPower vsetValidators `div` 3 + 1

-- | We have +2\/3 of votes which are distributed in any manner
any23
  :: SignedSet ty alg a k
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
data SignedSetMap r ty alg a k = SignedSetMap
  { vmapSubmaps    :: !(Map r (SignedSet ty alg a k))
  , vmapValidators :: !(ValidatorSet alg)
  , vmapToKey      :: !(a -> k)
  , vmapValueOk    :: !(a -> Bool)
  }

instance (Show a, Show r) => Show (SignedSetMap r ty alg a k) where
  showsPrec n = showsPrec n . vmapSubmaps

emptySignedSetMap
  :: ValidatorSet alg
  -> (a -> k)
  -> (a -> Bool)
  -> SignedSetMap r ty alg a k
emptySignedSetMap = SignedSetMap Map.empty

-- | Convert collection of signed values to plain map
toPlainMap
  :: SignedSetMap r ty alg a k
  -> Map r (CIntMap (ValidatorIdx alg) (Signed (ValidatorIdx alg) ty alg a))
toPlainMap = fmap vsetAddrMap . vmapSubmaps

addSignedValue
  :: (Ord r, Ord a, Ord k)
  => r
  -> Signed (ValidatorIdx alg) ty alg a
  -> SignedSetMap r ty alg a k
  -> InsertResult (Signed (ValidatorIdx alg) ty alg a) (SignedSetMap r ty alg a k)
addSignedValue r a sm@SignedSetMap{..} = do
  m <- insertSigned a
     $ fromMaybe (emptySignedSet vmapValidators vmapToKey vmapValueOk)
     $ Map.lookup r vmapSubmaps
  return sm { vmapSubmaps = Map.insert r m vmapSubmaps }

majority23at
  :: (Ord r)
  => r
  -> SignedSetMap r ty alg a k
  -> Maybe k
majority23at r SignedSetMap{..}
  = majority23 =<< Map.lookup r vmapSubmaps

any23at
  :: (Ord r)
  => r
  -> SignedSetMap r ty alg a k
  -> Bool
any23at r SignedSetMap{..}
  = maybe False any23 $ Map.lookup r vmapSubmaps

valuesAtR
  :: (Ord r)
  => r
  -> SignedSetMap r ty alg a k
  -> [Signed (ValidatorIdx alg) ty alg a]
valuesAtR r SignedSetMap{..} =
  case Map.lookup r vmapSubmaps of
    Nothing            -> []
    Just SignedSet{..} -> toList vsetAddrMap
