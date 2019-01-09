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
module Thundermint.Types.Validators (
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
  ) where

import qualified Codec.Serialise as CBOR
import Control.Monad
import           Data.Foldable
import qualified Data.Map        as Map
import           Data.Map          (Map)
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
  deriving (Show, Eq, Ord, CBOR.Serialise)

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
