{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE UndecidableInstances       #-}
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
  , indexByValidator
    -- ** Indexed validator sets
  , ValidatorIdx(..)
  , ValidatorISet
  , getValidatorIntSet
  , insertValidatorIdx
  , emptyValidatorISet
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

import qualified Codec.Serialise as CBOR
import Control.Monad
import           Data.Foldable
import           Data.List         (find)
import           Data.Maybe        (fromMaybe)
import           Data.Monoid       (Sum(..))
import qualified Data.Map        as Map
import           Data.Map          (Map)
import qualified Data.Set        as Set
import           Data.Set          (Set)
import qualified Data.IntSet as ISet
import           Data.IntSet   (IntSet)
import           GHC.Generics  (Generic)

import Thundermint.Crypto


----------------------------------------------------------------
-- Validators sets
----------------------------------------------------------------

-- | Information about remote validator
data Validator alg = Validator
  { validatorPubKey      :: !(PublicKey alg)
  , validatorVotingPower :: !Integer
  }
  deriving (Generic)
deriving instance Eq (PublicKey alg) => Eq (Validator alg)
instance Crypto alg => CBOR.Serialise (Validator alg)

-- | Set of all known validators for given height
data ValidatorSet alg = ValidatorSet
  { vsValidators :: !(Map (Address alg) (Validator    alg))
  , vsIndexes    :: !(Map (Address alg) (ValidatorIdx alg))
  , vsTotPower   :: !Integer
  }
  deriving (Generic)
deriving instance Eq (PublicKey alg) => Eq (ValidatorSet alg)

instance (Crypto alg) => CBOR.Serialise (ValidatorSet alg) where
  encode = CBOR.encode . toList . vsValidators
  decode = fmap (makeValidatorSet . asList) CBOR.decode >>= \case
    Left  e -> fail (show e)
    Right a -> return a
    where
      asList :: [a] -> [a]
      asList = id

-- | Create set of validators. Return @Left (Just addr)@ if list
--   contains multiple validators with same public keys, or @Left
--   Nothing@ if list is empty.
makeValidatorSet
  :: (Crypto alg, Foldable f)
  => f (Validator alg)
  -> Either (Maybe (Address alg)) (ValidatorSet alg)
makeValidatorSet vals = do
  when (null vals) $ Left Nothing
  vmap <- sequence
        $ Map.fromListWithKey (\k _ _ -> Left (Just k))
          [ ( address (validatorPubKey v), Right v) | v <- toList vals ]
  return ValidatorSet
    { vsValidators = vmap
    , vsIndexes    = Map.fromList $ Map.keys vmap `zip` map ValidatorIdx [0..]
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

-- | Get index of validator in set of validators
indexByValidator :: ValidatorSet alg -> Address alg -> Maybe (ValidatorIdx alg)
indexByValidator vs addr = addr `Map.lookup` vsIndexes vs

-- | Number of validators in set
validatorSetSize :: ValidatorSet alg -> Int
validatorSetSize = Map.size  . vsValidators




-- | Since all nodes agree on set of validators for given height they
--   could be identified simply by number where validators public keys
--   are lexicographically sorted.
--
--   This for example allows to represent validators as bit arrays.
newtype ValidatorIdx alg = ValidatorIdx Int
  deriving (Show, Eq, CBOR.Serialise)

-- | Set of validators where they are represented by their index.
data ValidatorISet = ValidatorISet !Int !IntSet

getValidatorIntSet :: ValidatorISet -> [ValidatorIdx alg]
getValidatorIntSet (ValidatorISet _ iset)
  = [ValidatorIdx i | i <- ISet.toList iset]

insertValidatorIdx :: ValidatorIdx alg -> ValidatorISet -> ValidatorISet
insertValidatorIdx (ValidatorIdx i) vset@(ValidatorISet n iset)
  | i < 0     = vset
  | i >= n    = vset
  | otherwise = ValidatorISet n (ISet.insert i iset)

-- | Create empty validator set of given size
emptyValidatorISet :: Int -> ValidatorISet
emptyValidatorISet n
  | n < 0     = error "Negative size"
  | otherwise = ValidatorISet n ISet.empty



----------------------------------------------------------------
-- Set of signed values
----------------------------------------------------------------

-- | Collection of signed values. It's intended to hold votes so only
--   one value per signature is allowed. Lookup is supported both by
--   values and by signer's address.
data SignedSet ty alg a k = SignedSet
  { vsetAddrMap    :: !(Map (Address alg) (Signed ty alg a))
  , vsetValMap     :: !(Map k (VoteGroup alg))
  , vsetAccPower   :: !Integer
  , vsetValidators :: !(ValidatorSet alg)
  , vsetToKey      :: !(a -> k)
  , vsetValueOK    :: !(a -> Bool)
  }

data VoteGroup alg = VoteGroup
  { accOK     :: !Integer             -- Accumulated weight of good votes
  , accBad    :: !Integer             -- Accumulated weight of invalid votes
  , votersOK  :: !(Set (Address alg)) -- Set of voters with good votes
  , votersBad :: !(Set (Address alg)) -- Set of voters with invalid votes
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
emptySignedSet = SignedSet Map.empty Map.empty 0

-- | Insert value into set of votes
insertSigned
  :: (Ord a, Ord k)
  => Signed ty alg a
  -> SignedSet ty alg a k
  -> InsertResult (Signed ty alg a) (SignedSet ty alg a k)
insertSigned sval SignedSet{..} =
  case validatorByAddr vsetValidators (signedAddr sval) of
    -- We trying to insert value signed by unknown key
    Nothing -> InsertUnknown sval
    Just validator
      -- We already have value signed by that key
      | Just v <- addr `Map.lookup` vsetAddrMap -> case () of
          _| signedValue v == val -> InsertDup
           | otherwise            -> InsertConflict sval
      -- OK insert value then
      | otherwise -> InsertOK SignedSet
          { vsetAddrMap  = Map.insert addr sval vsetAddrMap
          , vsetAccPower = vsetAccPower + validatorVotingPower validator
          , vsetValMap   =
              let upd VoteGroup{..}
                    | vsetValueOK val = Just VoteGroup
                                        { accOK    = accOK + validatorVotingPower validator
                                        , votersOK = Set.insert addr votersOK
                                        , ..
                                        }
                    | otherwise       = Just VoteGroup
                                        { accBad    = accBad + validatorVotingPower validator
                                        , votersBad = Set.insert addr votersBad
                                        , ..
                                        }
              in Map.alter (upd . fromMaybe nullVote) k vsetValMap
          , ..
          }
  where
    addr     = signedAddr  sval
    val      = signedValue sval
    k        = vsetToKey    val
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
  -> Map r (Map (Address alg) (Signed ty alg a))
toPlainMap = fmap vsetAddrMap . vmapSubmaps

addSignedValue
  :: (Ord r, Ord a, Ord k)
  => r
  -> Signed ty alg a
  -> SignedSetMap r ty alg a k
  -> InsertResult (Signed ty alg a) (SignedSetMap r ty alg a k)
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
  -> [Signed ty alg a]
valuesAtR r SignedSetMap{..} =
  case Map.lookup r vmapSubmaps of
    Nothing            -> []
    Just SignedSet{..} -> toList vsetAddrMap
