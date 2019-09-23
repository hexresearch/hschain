{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}
-- |
module HSChain.Data.CIntMap where

import Data.Coerce
import qualified Data.IntMap.Strict as IMap
import           Data.IntMap.Strict   (IntMap)
import           Data.IntSet          (IntSet)


-- | IntMap variant which allows keys which are coercible to 'Int'
newtype CIntMap k a = CIntMap (IntMap a)
  deriving newtype (Show, Eq, Ord, Foldable)

empty :: CIntMap k a
empty = CIntMap IMap.empty

size :: forall k a. CIntMap k a -> Int
size = coerce (IMap.size @a)

null :: forall k a. CIntMap k a -> Bool
null = coerce (IMap.null @a)

insert :: forall k a. (Coercible k Int)
       => k -> a -> CIntMap k a -> CIntMap k a
insert = coerce (IMap.insert @a)

lookup :: forall k a. (Coercible k Int)
       => k -> CIntMap k a -> Maybe a
lookup = coerce (IMap.lookup @a)

difference :: forall k a b. CIntMap k a -> CIntMap k b -> CIntMap k a
difference = coerce (IMap.difference @a @b)

fromSet :: forall k a. (Coercible k Int)
        => (k -> a) -> IntSet -> CIntMap k a
fromSet = coerce (IMap.fromSet @a)
