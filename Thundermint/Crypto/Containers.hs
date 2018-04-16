{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase #-}
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
  ) where

import Control.Monad
import           Data.Monoid       (Sum(..))
import qualified Data.Map        as Map
import           Data.Map          (Map)
import qualified Data.Set        as Set
import           Data.Set          (Set)


import Thundermint.Crypto



----------------------------------------------------------------
-- Set of signed values
----------------------------------------------------------------

-- | Collection of signed values. It's intended to hold votes so only
--   one value per signature is allowed. Lookup is supported both by
--   values and by signer's address.
data SignedSet ty alg a = SignedSet
  { vsetAddrMap  :: Map (Address alg) (Signed ty alg a)
  , vsetValMap   :: Map a (Set (Address alg))
  , vsetPower    :: Address alg -> Integer
    -- Voting power for given address
  , vsetTotPower :: Integer
    -- Total voting power
  }

-- | Result of insertion into 'SignedSet'
data InsertResult b a
  = InsertOK       a            -- ^ Insert is successful
  | InsertDup                   -- ^ Duplicate value. No change
  | InsertConflict b            -- ^ Conflict during insertion
  deriving (Show,Functor)

instance Applicative (InsertResult b) where
  pure  = return
  (<*>) = ap

instance Monad (InsertResult b) where
  return = pure
  InsertOK a       >>= f = f a
  InsertDup        >>= _ = InsertDup
  InsertConflict b >>= _ = InsertConflict b

emptySignedSet :: (Address alg -> Integer) -> Integer -> SignedSet ty alg a
emptySignedSet = SignedSet Map.empty Map.empty

-- | Insert value into set of votes
insertSigned
  :: (Crypto alg, Ord (Address alg), Ord a)
  => Signed ty alg a
  -> SignedSet ty alg a
  -> InsertResult (Signed ty alg a) (SignedSet ty alg a)
insertSigned sval (SignedSet mAddr mVal power tot) =
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
      , vsetPower    = power
      , vsetTotPower = tot
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
    values = [ a
             | (a, addrs) <- Map.toList vsetValMap
             , let Sum p = foldMap (Sum . vsetPower) addrs
             , p >= quorum
             ]
    quorum = 2 * vsetTotPower `div` 3 + 1

-- | We have +2\/3 of votes which are distributed in any manner
any23
  :: (Crypto alg, Ord (Address a))
  => SignedSet ty alg a
  -> Bool
any23 SignedSet{..}
  = tot >= quorum
  where
    tot    = sum [ vsetPower a | a <- Map.keys vsetAddrMap ]
    quorum = 2 * vsetTotPower `div` 3 + 1



----------------------------------------------------------------
-- 
----------------------------------------------------------------


-- | Map from @r@ to @SignedSet ty alg a@. It maintains invariant that
--   all submaps have same distribution of voting power
data SignedSetMap r ty alg a = SignedSetMap
  { vmapSubmaps  :: Map r (SignedSet ty alg a)
  , vmapPower    :: Address alg -> Integer
  , vmapTotPower :: Integer
  }

emptySignedSetMap
  :: (Address alg -> Integer)
  -> Integer
  -> SignedSetMap r ty alg a
emptySignedSetMap = SignedSetMap Map.empty

addSignedValue
  :: (Ord r, Crypto alg, Ord (Address alg), Ord a)
  => r
  -> Signed ty alg a
  -> SignedSetMap r ty alg a
  -> InsertResult (Signed ty alg a) (SignedSetMap r ty alg a)
addSignedValue r a sm@SignedSetMap{..} = do
  m' <- case Map.lookup r vmapSubmaps of
    Nothing -> insertSigned a $ emptySignedSet vmapPower vmapTotPower
    Just m  -> return m
  return sm { vmapSubmaps = Map.insert r m' vmapSubmaps }

majority23at
  :: (Ord r, Crypto alg, Ord a)
  => r
  -> SignedSetMap r ty alg a
  -> Maybe a
majority23at r SignedSetMap{..}
  = majority23 =<< Map.lookup r vmapSubmaps

any23at
  :: (Ord r, Crypto alg, Ord (Address a))
  => r
  -> SignedSetMap r ty alg a
  -> Bool
any23at r SignedSetMap{..}
  = maybe False any23 $ Map.lookup r vmapSubmaps
