{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}
-- |
-- Data types for
module HSChain.Types.Validators (
    -- * Validators
    Validator(..)
    -- ** Validator sets
  , ValidatorSet
  , emptyValidatorSet
  , makeValidatorSet
  , totalVotingPower
  , validatorSetSize
  , validatorByIndex
  , asValidatorList
  , indexByValidator
    -- ** Indexed validator sets
  , ValidatorIdx(..)
  , ValidatorISet
  , getValidatorIntSet
  , insertValidatorIdx
  , emptyValidatorISet
  , emptyValidatorISetFromSize
    -- * Change of validator sets
  , ValidatorChange(..)
  , changeValidators
  , validatorsDifference
  ) where

import Control.DeepSeq
import Data.Foldable

import Data.Coerce
import Data.Ord     (comparing)
import Data.List    (sortBy)
import Data.IntSet  (IntSet)
import Data.Map     (Map)
import GHC.Generics (Generic, Generic1)

import qualified Codec.Serialise       as CBOR
import qualified Data.Aeson            as JSON
import qualified Data.IntSet           as ISet
import qualified Data.Map.Strict       as Map
import qualified Data.Map.Merge.Strict as Map
import qualified Data.Vector           as V

import HSChain.Crypto
import HSChain.Types.Merklized


----------------------------------------------------------------
-- Validators sets
----------------------------------------------------------------

-- | Information about remote validator
data Validator alg = Validator
  { validatorPubKey      :: !(PublicKey alg)
  , validatorVotingPower :: !Integer
  }
  deriving stock    (Generic, Show)
  deriving anyclass (CBOR.Serialise, JSON.ToJSON, JSON.FromJSON)
instance NFData (PublicKey alg) => NFData (Validator alg)
deriving instance Eq   (PublicKey alg) => Eq   (Validator alg)
deriving instance Ord  (PublicKey alg) => Ord  (Validator alg)

instance (Crypto alg, alg' ~ alg) => MerkleValue alg (Validator alg) where
  merkleHash = hash

-- | Set of all known validators for given height
data ValidatorSet alg = ValidatorSet
  { vsValidators :: !(V.Vector (Validator alg))
  , vsTotPower   :: !Integer
  }
  deriving (Generic, Show)
instance NFData (PublicKey alg) => NFData (ValidatorSet alg)
deriving instance Eq   (PublicKey alg) => Eq  (ValidatorSet alg)
deriving instance Ord  (PublicKey alg) => Ord (ValidatorSet alg)

instance (Crypto alg, alg' ~ alg) => MerkleValue alg (ValidatorSet alg) where
  merkleHash = hash

emptyValidatorSet :: ValidatorSet alg
emptyValidatorSet = ValidatorSet V.empty 0

-- | Get list of all validators included into set
asValidatorList :: ValidatorSet alg -> [Validator alg]
asValidatorList = V.toList . vsValidators

instance (Crypto alg) => CBOR.Serialise (ValidatorSet alg) where
  encode = CBOR.encode . toList . vsValidators
  decode = fmap (makeValidatorSet . asList) CBOR.decode >>= \case
    Left  e -> fail (show e)
    Right a -> return a
    where
      asList :: [a] -> [a]
      asList = id

instance CryptoAsymmetric alg => JSON.ToJSON (ValidatorSet alg) where
  toJSON    = JSON.toJSON . vsValidators
instance Crypto alg => JSON.FromJSON (ValidatorSet alg) where
  parseJSON o = do
    vals <- JSON.parseJSON o
    case makeValidatorSet (vals :: [Validator alg]) of
      Right a -> return a
      Left  e -> fail (show e)

-- | Create set of validators. Return @Left (Just addr)@ if list
--   contains multiple validators with same public keys, or @Left
--   Nothing@ if list is empty.
makeValidatorSet
  :: (Crypto alg, Foldable f)
  => f (Validator alg)
  -> Either (PublicKey alg) (ValidatorSet alg)
makeValidatorSet vals = do
  let vlist = sortBy (comparing validatorPubKey)
            $ toList vals
  check vlist
  return ValidatorSet { vsValidators = V.fromList vlist
                      , vsTotPower   = sum $ map validatorVotingPower vlist
                      }
  where
    check (Validator k1 _ : rest@(Validator k2 _ : _))
      | k1 == k2  = Left k1
      | otherwise = check rest
    check _       = return ()

-- | Return total voting power of all validators
totalVotingPower :: ValidatorSet alg -> Integer
totalVotingPower = vsTotPower

-- | Get validator by its fingerprint
validatorByIndex :: ValidatorSet alg -> ValidatorIdx alg -> Maybe (Validator alg)
validatorByIndex vs (ValidatorIdx i)
  = vsValidators vs V.!? i

-- | Get index of validator in set of validators
indexByValidator :: (Eq (PublicKey alg)) => ValidatorSet alg -> PublicKey alg -> Maybe (ValidatorIdx alg)
indexByValidator (ValidatorSet vs _) key
  = coerce $ V.findIndex ((==key) . validatorPubKey) vs

-- | Number of validators in set
validatorSetSize :: ValidatorSet alg -> Int
validatorSetSize = V.length . vsValidators




-- | Since all nodes agree on set of validators for given height they
--   could be identified simply by number where validators public keys
--   are lexicographically sorted.
--
--   This for example allows to represent validators as bit arrays.
newtype ValidatorIdx alg = ValidatorIdx Int
  deriving stock    (Show, Eq, Ord, Generic, Generic1)
  deriving anyclass (NFData, CBOR.Serialise)
  deriving newtype  (JSON.ToJSON, JSON.FromJSON, JSON.FromJSONKey, JSON.ToJSONKey)

-- | Set of validators where they are represented by their index.
data ValidatorISet = ValidatorISet !Int !IntSet deriving (Show, Eq)

instance NFData ValidatorISet where
  rnf (ValidatorISet a b) = rnf a `seq` rnf b

getValidatorIntSet :: ValidatorISet -> IntSet
getValidatorIntSet (ValidatorISet _ iset) = iset

insertValidatorIdx :: ValidatorIdx alg -> ValidatorISet -> ValidatorISet
insertValidatorIdx (ValidatorIdx i) vset@(ValidatorISet n iset)
  | i < 0     = vset
  | i >= n    = vset
  | otherwise = ValidatorISet n (ISet.insert i iset)

-- | Create empty validator set of given size
emptyValidatorISet :: ValidatorSet alg -> ValidatorISet
emptyValidatorISet valSet = ValidatorISet (validatorSetSize valSet) ISet.empty

emptyValidatorISetFromSize :: Int -> ValidatorISet
emptyValidatorISetFromSize n
  | n < 0     = error "Negative size"
  | otherwise = ValidatorISet n ISet.empty


----------------------------------------------------------------
-- Changes in validator sets
----------------------------------------------------------------

-- | Change of validators. If voting power of validator is changed to
--   zero it's removed from set.
newtype ValidatorChange alg = ValidatorChange (Map (PublicKey alg) Integer)
  deriving stock    (Show, Generic)
  deriving newtype  (JSON.ToJSON, JSON.FromJSON)
  deriving anyclass (CBOR.Serialise)

deriving newtype instance Eq     (PublicKey alg) => Eq     (ValidatorChange alg)
deriving newtype instance Ord    (PublicKey alg) => Ord    (ValidatorChange alg)
deriving newtype instance NFData (PublicKey alg) => NFData (ValidatorChange alg)


instance (Ord (PublicKey alg)) => Semigroup (ValidatorChange alg) where
  ValidatorChange c1 <> ValidatorChange c2 = ValidatorChange (c2 <> c1) 

instance (Ord (PublicKey alg)) => Monoid (ValidatorChange alg) where
  mempty  = ValidatorChange mempty
  mappend = (<>)


-- | Compute difference between sets of validators 
validatorsDifference
  :: CryptoSign alg
  => ValidatorSet alg -> ValidatorSet alg -> ValidatorChange alg
validatorsDifference (ValidatorSet vsOld _) (ValidatorSet vsNew _)
  = ValidatorChange change
  where
    change = Map.merge
      (Map.traverseMissing $ \_ _ -> pure 0)
      (Map.traverseMissing $ \_ n -> pure n)
      (Map.zipWithMaybeAMatched match)
      vmapOld vmapNew
    match _ old new | old == new = pure Nothing
                    | otherwise  = pure (Just new)
    --
    vmapOld   = toVMap vsOld
    vmapNew   = toVMap vsNew
    toVMap vs = Map.fromList [ (k, p) | Validator k p <- toList vs ]

-- | Update set of validators according to diff. Function is rather
--   restrictive. If any of following conditions is violated @Nothing@
--   is returned.
--
--    * Voting power in @ChangeValidator@ must be positive.
--
--    * Noop changes are disallowed. Primarily to ensure that won't
--      end up storing useless data.
changeValidators
  :: (Crypto alg)
  => ValidatorChange alg -> ValidatorSet alg -> Maybe (ValidatorSet alg)
changeValidators (ValidatorChange delta) (ValidatorSet vset _)
  =   either (const Nothing) Just
  .   makeValidatorSet
  .   map (uncurry Validator)
  .   Map.toList
  =<< Map.mergeA (Map.traverseMissing      (\_ n -> Just n))
                 (Map.traverseMaybeMissing addNew)
                 (Map.zipWithMaybeAMatched update)
                 vmapOld delta
  where
    -- In order to allow working composition of changes we have to
    -- noop deletions
    addNew  _ n | n < 0     = Nothing
                | n == 0    = Just Nothing
                | otherwise = Just (Just n)
    -- Update existing validator
    update  _ _ 0             = Just Nothing
    update  _ n m | n == m    = Nothing
                  | m <  0    = Nothing
                  | otherwise = Just (Just m)
    --
    vmapOld = Map.fromList [ (k, p) | Validator k p <- toList vset ]

