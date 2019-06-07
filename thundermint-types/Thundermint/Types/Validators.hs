{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE UndecidableInstances       #-}
-- |
-- Data types for
module Thundermint.Types.Validators (
    -- * Validators
    Validator(..)
    -- ** Validator sets
  , ValidatorSet
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
    -- * Change of validator sets
  , ValidatorChange(..)
  , changeValidators
  ) where

import Control.DeepSeq
import Control.Monad
import Data.Foldable

import Data.IntSet  (IntSet)
import Data.Map     (Map)
import GHC.Generics (Generic, Generic1)

import qualified Codec.Serialise as CBOR
import qualified Data.Aeson      as JSON
import qualified Data.IntSet     as ISet
import qualified Data.Map        as Map
import qualified Data.Set        as Set

import Thundermint.Crypto


----------------------------------------------------------------
-- Validators sets
----------------------------------------------------------------

-- | Information about remote validator
data Validator alg = Validator
  { validatorPubKey      :: !(PublicKey alg)
  , validatorVotingPower :: !Integer
  }
  deriving stock    (Generic, Show)
  deriving anyclass (CBOR.Serialise)
instance NFData (PublicKey alg) => NFData (Validator alg)
deriving instance Eq   (PublicKey alg) => Eq   (Validator alg)
deriving instance Ord  (PublicKey alg) => Ord  (Validator alg)

-- | Set of all known validators for given height
data ValidatorSet alg = ValidatorSet
  { vsValidators :: !(Map (Fingerprint alg) (Validator    alg))
  , vsIndexes    :: !(Map (Fingerprint alg) (ValidatorIdx alg))
  , vsTotPower   :: !Integer
  }
  deriving (Generic, Show)
instance NFData (PublicKey alg) => NFData (ValidatorSet alg)
deriving instance Eq   (PublicKey alg) => Eq   (ValidatorSet alg)

-- | Get list of all validators included into set
asValidatorList :: ValidatorSet alg -> [Validator alg]
asValidatorList = toList . vsValidators

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
  -> Either (Maybe (Fingerprint alg)) (ValidatorSet alg)
makeValidatorSet vals = do
  when (null vals) $ Left Nothing
  vmap <- sequence
        $ Map.fromListWithKey (\k _ _ -> Left (Just k))
          [ ( fingerprint (validatorPubKey v), Right v) | v <- toList vals ]
  return ValidatorSet
    { vsValidators = vmap
    , vsIndexes    = Map.fromList $ Map.keys vmap `zip` map ValidatorIdx [0..]
    , vsTotPower   = sum $ map validatorVotingPower $ toList vals
    }

-- | Return total voting power of all validators
totalVotingPower :: ValidatorSet alg -> Integer
totalVotingPower = vsTotPower

-- | Get validator by its fingerprint
validatorByIndex :: ValidatorSet alg -> ValidatorIdx alg -> Maybe (Validator alg)
validatorByIndex vs (ValidatorIdx i)
  | i < 0                    = Nothing
  | i >= validatorSetSize vs = Nothing
  | otherwise                = Just (toList (vsValidators vs) !! i)

-- | Get index of validator in set of validators
indexByValidator :: ValidatorSet alg -> Fingerprint alg -> Maybe (ValidatorIdx alg)
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
  deriving stock    (Show, Eq, Ord, Generic, Generic1)
  deriving anyclass (NFData, CBOR.Serialise)
  deriving newtype  (JSON.ToJSON, JSON.FromJSON, JSON.FromJSONKey, JSON.ToJSONKey)

-- | Set of validators where they are represented by their index.
data ValidatorISet = ValidatorISet !Int !IntSet

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
emptyValidatorISet :: Int -> ValidatorISet
emptyValidatorISet n
  | n < 0     = error "Negative size"
  | otherwise = ValidatorISet n ISet.empty



----------------------------------------------------------------
-- Changes in validator sets
----------------------------------------------------------------

-- | Change in set of validators
data ValidatorChange alg
  = RemoveValidator !(PublicKey alg)
  -- ^ Remove validator from set
  | ChangeValidator !(PublicKey alg) !Integer
  -- ^ Change validator voting power or add new validator
  deriving (Show,Generic)

deriving instance (Eq (PublicKey alg)) => Eq (ValidatorChange alg)
instance NFData (PublicKey alg) => NFData (ValidatorChange alg)
instance Crypto alg => CBOR.Serialise (ValidatorChange alg)
instance Crypto alg => JSON.ToJSON    (ValidatorChange alg)
instance Crypto alg => JSON.FromJSON  (ValidatorChange alg)

-- | Update set of validators according to diff. Function is rather
--   restrictive. If any of following conditions is violated @Nothing@
--   is returned.
--
--    * Voting power in @ChangeValidator@ must be positive.
--
--    * Validator which is not in the set cannot be removed.
--
--    * Same validator could not be changed twice
--
--    * Noop changes are disallowed. Primarily to ensure that won't
--      end up storing useless data.
changeValidators
  :: (Crypto alg)
  => [ValidatorChange alg] -> ValidatorSet alg -> Maybe (ValidatorSet alg)
changeValidators []      vset = Just vset
changeValidators changes ValidatorSet{..} = do
  -- No duplicate references to validator
  guard $ length changes == Set.size (Set.fromList keys)
  -- Update validator set
  newVals      <- foldM step vsValidators changes
  Right  vset' <- return $ makeValidatorSet newVals
  return vset'
  where
    step vals (RemoveValidator pk) = do
      let addr = fingerprint pk
      guard  $  Map.member addr vals
      return $! Map.delete addr vals
    step vals (ChangeValidator pk pwr)
      | pwr <= 0 = Nothing
      | isNoop addr pwr vals = Nothing
      | otherwise            = return $! Map.insert addr (Validator pk pwr) vals
      where
        addr = fingerprint pk
    --
    isNoop addr pwr vals = case Map.lookup addr vals of
      Just (Validator _ p) -> pwr == p
      _                    -> False
    --
    keys = [ fingerprint $ case c of RemoveValidator pk   -> pk
                                     ChangeValidator pk _ -> pk
           | c <- changes
           ]
