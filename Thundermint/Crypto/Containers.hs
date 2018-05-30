{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase #-}
-- |
-- Data types for
module Thundermint.Crypto.Containers (
    -- * Validators
    Validator(..)
    -- ** Validator sets
  , ValidatorSet
  , makeValidatorSet
  , totalVotingPower
  , validatorSetSize
  , validatorByAddr
  , validatorByIndex
    -- ** Indexed validator sets
  , ValidatorIdx(..)
  , ValidatorISet
  , emptyValidatorISet
  , dupEmptyValidatorISet
    -- * Sets of signed values
  , SignedSet
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
import           Data.Maybe        (fromMaybe)
import           Data.Monoid       (Sum(..))
import qualified Data.Map        as Map
import           Data.Map          (Map)
import qualified Data.Set        as Set
import           Data.Set          (Set)
import qualified Data.IntSet as ISet
import           Data.IntSet   (IntSet)


import Thundermint.Crypto


----------------------------------------------------------------
-- Validators sets
----------------------------------------------------------------

-- | Information about remote validator
data Validator alg = Validator
  { validatorPubKey      :: PublicKey alg
  , validatorVotingPower :: Integer
  }

-- | Set of all known validators for given height
data ValidatorSet alg = ValidatorSet
  { vsValidators :: !(Map (Address alg) (Validator alg))
    --
  , vsTotPower   :: !Integer
    --
  }

-- | Create set of validators. Return @Left addr@ if list contains
--   multiple validators with same public keys
makeValidatorSet
  :: (Crypto alg, Foldable f)
  => f (Validator alg)
  -> Either (Address alg) (ValidatorSet alg)
makeValidatorSet vals = do
  vmap <- sequence
        $ Map.fromListWithKey (\k _ _ -> Left k)
          [ ( address (validatorPubKey v), Right v) | v <- toList vals ]
  return ValidatorSet
    { vsValidators = vmap
    , vsTotPower   = sum $ map validatorVotingPower $ toList vals
    }

-- | Return total voting power of all validators
totalVotingPower :: ValidatorSet alg -> Integer
totalVotingPower = vsTotPower

-- | Get validator by its address
validatorByAddr :: ValidatorSet alg -> Address alg -> Maybe (Validator alg)
validatorByAddr vs addr = addr `Map.lookup` vsValidators vs

-- | Get validator by its address
validatorByIndex :: ValidatorSet alg -> ValidatorIdx alg -> Maybe (Validator alg)
validatorByIndex vs (ValidatorIdx i)
  | i < 0                    = Nothing
  | i >= validatorSetSize vs = Nothing
  | otherwise                = Just (toList (vsValidators vs) !! i)

-- | Number of validators in set
validatorSetSize :: ValidatorSet alg -> Int
validatorSetSize = Map.size  . vsValidators




-- | Since all nodes agree on set of validators for given height they
--   could be identified simply by number where validators public keys
--   are lexicographically sorted.
--
--   This for example allows to represent validators as bit arrays.
newtype ValidatorIdx alg = ValidatorIdx Int


-- | Set of validators where they are represented by their index.
data ValidatorISet = ValidatorISet !Int !IntSet

-- | Create empty validator set of given size
emptyValidatorISet :: Int -> ValidatorISet
emptyValidatorISet n
  | n < 0     = error "Negative size"
  | otherwise = ValidatorISet n ISet.empty

-- | Create empty validator set of same size as argument.
dupEmptyValidatorISet :: ValidatorISet -> ValidatorISet
dupEmptyValidatorISet (ValidatorISet n _) = ValidatorISet n ISet.empty



----------------------------------------------------------------
-- Set of signed values
----------------------------------------------------------------

-- | Collection of signed values. It's intended to hold votes so only
--   one value per signature is allowed. Lookup is supported both by
--   values and by signer's address.
data SignedSet ty alg a = SignedSet
  { vsetAddrMap    :: Map (Address alg) (Signed ty alg a)
  , vsetValMap     :: Map a (Set (Address alg))
  , vsetValidators :: ValidatorSet alg
  }

instance (Show a) => Show (SignedSet ty alg a) where
  showsPrec n = showsPrec n . vsetAddrMap

-- | Result of insertion into 'SignedSet'
data InsertResult b a
  = InsertOK       a            -- ^ Insert is successful
  | InsertDup                   -- ^ Duplicate value. No change
  | InsertConflict b            -- ^ Conflict during insertion
  deriving (Show,Functor)

instance Applicative (InsertResult b) where
  pure  = InsertOK
  (<*>) = ap

instance Monad (InsertResult b) where
  return = InsertOK
  InsertOK a       >>= f = f a
  InsertDup        >>= _ = InsertDup
  InsertConflict b >>= _ = InsertConflict b

emptySignedSet :: ValidatorSet alg -> SignedSet ty alg a
emptySignedSet = SignedSet Map.empty Map.empty

-- | Insert value into set of votes
insertSigned
  :: (Crypto alg, Ord a)
  => Signed ty alg a
  -> SignedSet ty alg a
  -> InsertResult (Signed ty alg a) (SignedSet ty alg a)
insertSigned sval (SignedSet mAddr mVal valSet) =
  case addr `Map.lookup` mAddr of
    Just v
      | signedValue v == val -> InsertDup
      | otherwise            -> InsertConflict sval
    Nothing                  -> InsertOK SignedSet
      { vsetAddrMap = Map.insert addr sval mAddr
      , vsetValMap  = Map.alter
          (Just . \case
              Nothing        -> Set.singleton addr
              Just addresses -> addr `Set.insert` addresses
          ) val mVal
      , vsetValidators = valSet
      }
  where
    addr = signedAddr  sval
    val  = signedValue sval

-- | We have +2\/3 majority of votes return vote for
majority23
  :: (Crypto alg, Ord a)
  => SignedSet ty alg a
  -> Maybe a
majority23 SignedSet{..} =
  case values of
    []  -> Nothing
    a:_ -> Just a
  where
    power  = maybe 0 validatorVotingPower
           . validatorByAddr vsetValidators
    values = [ a
             | (a, addrs) <- Map.toList vsetValMap
             , let Sum p = foldMap (Sum . power) addrs
             , p >= quorum
             ]
    quorum = 2 * totalVotingPower vsetValidators `div` 3 + 1

-- | We have +2\/3 of votes which are distributed in any manner
any23
  :: (Crypto alg)
  => SignedSet ty alg a
  -> Bool
any23 SignedSet{..}
  = tot >= quorum
  where
    power  = maybe 0 validatorVotingPower
           . validatorByAddr vsetValidators
    tot    = sum [ power a | a <- Map.keys vsetAddrMap ]
    quorum = 2 * totalVotingPower vsetValidators `div` 3 + 1



----------------------------------------------------------------
--
----------------------------------------------------------------


-- | Map from @r@ to @SignedSet ty alg a@. It maintains invariant that
--   all submaps have same distribution of voting power
data SignedSetMap r ty alg a = SignedSetMap
  { vmapSubmaps    :: Map r (SignedSet ty alg a)
  , vmapValidators :: ValidatorSet alg
  }

instance (Show a, Show r) => Show (SignedSetMap r ty alg a) where
  showsPrec n = showsPrec n . vmapSubmaps

emptySignedSetMap
  :: ValidatorSet alg
  -> SignedSetMap r ty alg a
emptySignedSetMap = SignedSetMap Map.empty

-- | Convert collection of signed values to plain map
toPlainMap
  :: SignedSetMap r ty alg a
  -> Map r (Map (Address alg) (Signed ty alg a))
toPlainMap = fmap vsetAddrMap . vmapSubmaps

addSignedValue
  :: (Ord r, Crypto alg, Ord a)
  => r
  -> Signed ty alg a
  -> SignedSetMap r ty alg a
  -> InsertResult (Signed ty alg a) (SignedSetMap r ty alg a)
addSignedValue r a sm@SignedSetMap{..} = do
  m <- insertSigned a
     $ fromMaybe (emptySignedSet vmapValidators)
     $ Map.lookup r vmapSubmaps
  return sm { vmapSubmaps = Map.insert r m vmapSubmaps }

majority23at
  :: (Ord r, Crypto alg, Ord a)
  => r
  -> SignedSetMap r ty alg a
  -> Maybe a
majority23at r SignedSetMap{..}
  = majority23 =<< Map.lookup r vmapSubmaps

any23at
  :: (Ord r, Crypto alg)
  => r
  -> SignedSetMap r ty alg a
  -> Bool
any23at r SignedSetMap{..}
  = maybe False any23 $ Map.lookup r vmapSubmaps

valuesAtR
  :: (Ord r)
  => r
  -> SignedSetMap r ty alg a
  -> [Signed ty alg a]
valuesAtR r SignedSetMap{..} =
  case Map.lookup r vmapSubmaps of
    Nothing            -> []
    Just SignedSet{..} -> toList vsetAddrMap
