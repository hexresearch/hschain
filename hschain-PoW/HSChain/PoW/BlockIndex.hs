-- |
-- Block index is data structure which holds headers for all blocks in
-- blockchain in memory. This module provides data structure and set
-- of common operations for working with it.
{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts  #-}
module HSChain.PoW.BlockIndex
  ( -- * Block index
    BlockIndex(..)
  , lookupIdx
  , insertIdx
  -- * Traversals
  , traverseBlockIndex
  , traverseBlockIndexM
  , traverseBlockIndexM_
  -- ** Path in index
  , BlockIndexPath(..)
  , makeBlockIndexPath
  , makeBlockIndexPathM
  -- * Construction of index
  , blockIndexHeads
  , blockIndexFromGenesis
  ) where

import Control.Monad
import Data.Foldable
import Data.Functor.Identity
import Data.Map              (Map,(!))
import qualified Data.Map        as Map
import qualified Data.Set        as Set

import HSChain.PoW.Types
import HSChain.Types.Merkle.Types


----------------------------------------------------------------
-- Block index
----------------------------------------------------------------

-- | Tree of block headers. It's append-only data supports which
--   supports looking up header by block ID and 'BH' contain reference
--   to previous block. When working with @BlockIndex@ one should take
--   care to maximize sharing.
newtype BlockIndex b = BlockIndex
  -- FIXME: We'll want to add blocks incrementally into compact to
  --        make things easier for GC
  { _blockIDMap :: Map (BlockID b) (BH b)
  }

-- | Find header in block index using it's ID.
lookupIdx :: (Ord (BlockID b)) => BlockID b -> BlockIndex b -> Maybe (BH b)
lookupIdx bid (BlockIndex idx) = Map.lookup bid idx

-- | Add header to block index. @bhPrevious@ field of header should
--   point to value already in the index.
insertIdx :: (Ord (BlockID b)) => BH b -> BlockIndex b -> BlockIndex b
insertIdx bh (BlockIndex idx) = BlockIndex $ Map.insert (bhBID bh) bh idx


-- | Find path between nodes in block index
traverseBlockIndex
  :: (BlockData b)
  => (BH b -> a -> a)         -- ^ Rollback block
  -> (BH b -> a -> a)         -- ^ Update update block
  -> BH b                     -- ^ Traverse from block
  -> BH b                     -- ^ Traverse to block
  -> (a -> a)
traverseBlockIndex rollback update fromB toB
  = runIdentity
  . traverseBlockIndexM
    (\b -> Identity . rollback b)
    (\b -> Identity . update   b)
    fromB toB

-- | Find path between nodes in block index and build monadic
--   function. Actions are performed in following order: first block
--   are rolled back, then applied.
traverseBlockIndexM
  :: (Monad m, BlockData b)
  => (BH b -> a -> m a)         -- ^ Rollback block
  -> (BH b -> a -> m a)         -- ^ Update update block
  -> BH b                       -- ^ Traverse from block
  -> BH b                       -- ^ Traverse to block
  -> (a -> m a)
traverseBlockIndexM rollback update = go
  where
    go fromB toB = case bhHeight fromB `compare` bhHeight toB of
      GT                -> rollback fromB >=> go (prev fromB) toB
      LT                ->                    go fromB        (prev toB) >=> update toB
      EQ | toB /= fromB -> rollback fromB >=> go (prev fromB) (prev toB) >=> update toB
         | otherwise    -> return
    prev b = case bhPrevious b of
      Just b' -> b'
      Nothing -> error "Internal error"

-- | Find path between nodes and execute monadic action for each node.
traverseBlockIndexM_
  :: (Monad m, BlockData b)
  => (BH b -> m ())         -- ^ Rollback block
  -> (BH b -> m ())         -- ^ Update update block
  -> BH b                   -- ^ Traverse from block
  -> BH b                   -- ^ Traverse to block
  -> m ()
traverseBlockIndexM_ rollback update from to
  = traverseBlockIndexM
    (\bh () -> rollback bh) (\bh () -> update bh) from to ()

-- | Find all heads from index: blocks which aren't parents of some
--   other blocks. Requires complete traversal of index.
blockIndexHeads :: (Ord (BlockID b)) => BlockIndex b -> [BH b]
blockIndexHeads (BlockIndex bMap)
  = fmap (\bid -> bMap ! bid)
  $ toList
  $ foldl' remove (Map.keysSet bMap) (Map.toList bMap)
  where
    remove bids (bid, BH{bhPrevious = Nothing}) = Set.delete bid bids
    remove bids (_  , BH{bhPrevious = Just bh}) = Set.delete (bhBID bh) bids

-- | Create block index which contains only genesis
blockIndexFromGenesis :: (IsMerkle f, BlockData b) => GBlock b f -> BlockIndex b
blockIndexFromGenesis genesis
  | blockHeight genesis /= Height 0 = error "Genesis must be H=0"
  | otherwise                       = BlockIndex $ Map.singleton bid bh
  where
    bid = blockID genesis
    bh  = BH { bhHeight   = Height 0
             , bhTime     = blockTime genesis
             , bhBID      = bid
             , bhWork     = blockWork genesis
             , bhPrevious = Nothing
             , bhData     = merkleMap (const Proxy) $ blockData genesis
             }


----------------------------------------------------------------
-- Materialized path
----------------------------------------------------------------

-- | Path between blocks in block index
data BlockIndexPath a
  = RevertBlock !a (BlockIndexPath a)
  | ApplyBlock  !a (BlockIndexPath a)
  | NoChange
  deriving (Show, Eq, Functor, Foldable, Traversable)

makeBlockIndexPath
  :: BlockData b => (BH b -> a) -> BH b -> BH b -> BlockIndexPath a
makeBlockIndexPath f from to = traverseBlockIndex
  (\bh -> RevertBlock (f bh))
  (\bh -> ApplyBlock  (f bh))
  from to
  NoChange

makeBlockIndexPathM
  :: (Monad m, BlockData b)
  => (BH b -> m a) -> BH b -> BH b -> m (BlockIndexPath a)
makeBlockIndexPathM f from to = traverseBlockIndexM
  (\bh p -> do { a <- f bh; return $ RevertBlock a p })
  (\bh p -> do { a <- f bh; return $ ApplyBlock  a p })
  from to
  NoChange
