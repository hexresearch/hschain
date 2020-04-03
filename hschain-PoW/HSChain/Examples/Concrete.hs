{-# LANGUAGE ConstrainedClassMethods    #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternGuards              #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}
-- |Concrete.hs
--
-- Concrete implementation of a PoW-based blockchain with UTXO.
--

module HSChain.Examples.Concrete where

import Codec.Serialise

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import Data.Set (Set)
import qualified Data.Set as Set

--import Data.Monoid (Sum(..))
--import Data.Functor.Classes (Show1)
--import GHC.Generics (Generic)

import HSChain.Crypto
--import HSChain.Crypto.Classes.Hash
--import HSChain.Crypto.SHA
--import HSChain.Types.Merkle.Types


----------------------------------------------------------------
-- The tree.

data MerkleTree container alg a =
  Node (Hashed alg a)
       (container (Either
                            (MerkleTree container alg a, MerkleTree container alg a)
                            a
                    ))

-- |Obtain a hash of a node.
getHash :: MerkleTree container alg a -> Hashed alg a
getHash (Node thash _) = thash

instance Eq (Hashed alg a) => Eq (MerkleTree container alg a) where
  a == b = getHash a == getHash b

instance Ord (Hashed alg a) => Ord (MerkleTree container alg a) where
  compare a b = compare (getHash a) (getHash b)

data None a = None
data One a = One a

-- |A tree that knows nothing but the hash of the root.
type MerkleRoot alg a = MerkleTree None alg a

-- |A partially complete tree.
type PartialTree alg a = MerkleTree Maybe alg a

-- |A completely built tree.
type CompleteTree alg a = MerkleTree One alg a

-- |A conversion function from partially built tree into one that is complete.
toCompleteTree :: PartialTree alg a -> Maybe (CompleteTree alg a)
toCompleteTree (Node nhash (Just (Left (l, r)))) =
  (\l' r' -> Node nhash $ One $ Left (l', r')) <$> toCompleteTree l <*> toCompleteTree r
toCompleteTree (Node nhash (Just (Right a))) =
  Just $ Node nhash $ One $ Right $ a
toCompleteTree (Node _hash Nothing) = Nothing

-------------------------------------------------------------------------------
-- Tree (block) database.

-- |A database of trees.
--
-- There are a map from hashes to complete trees and map of incomplete trees
-- to the hashes of their children.
data TreeDB alg a = TreeDB
  { treeDBCompleted     :: Map.Map (MerkleRoot alg a) (CompleteTree alg a)
    -- ^ A list of completed trees. Only grows.
  , treeDBPartials      :: Map.Map (MerkleRoot alg a) (PartialTree alg a)
    -- ^ A list of partial trees.
  , treeDBOpenPartials  :: Map.Map (MerkleRoot alg a) ()
    -- ^ A list of partials that are still open (have Nothing in the container part).
  }

emptyTreeDB :: TreeDB alg a
emptyTreeDB = TreeDB
  { treeDBCompleted    = Map.empty
  , treeDBPartials     = Map.empty
  , treeDBOpenPartials = Map.empty
  }


-- |Convert to tree root.
toTreeRoot :: MerkleTree container alg a -> MerkleRoot alg a
toTreeRoot (Node rhash _) = Node rhash None

-- |Add a set of complete trees into database and propagate completeness
-- upward.
addCompleted :: TreeDB alg a -> Map.Map (MerkleRoot alg a) (CompleteTree alg a) -> TreeDB alg a
addCompleted treeDB newCompleteMap
  | Map.null unseenCompleteMap = treeDB -- nothing had changed.
  | otherwise = addCompleted updatedDB nextCompleted
  where
    unseenCompleteMap = Map.difference newCompleteMap nextCompleted
    updatedDB = treeDB
                  { treeDBCompleted = Map.unions
                                      [ treeDBCompleted treeDB
                                      , newCompleteMap
                                      ]
                  , treeDBPartials  = Map.difference (treeDBPartials treeDB) newCompleteMap
                  , treeDBOpenPartials = Map.difference (treeDBOpenPartials treeDB) newCompleteMap
                  }
    movePartials partial completed = case partial of
        Node phash (Just (Left (l, r))) -- we lift only inner nodes here.
          | Just ln <- Map.lookup (toTreeRoot l) newCompleteMap
          , Just rn <- Map.lookup (toTreeRoot r) newCompleteMap
          -> Map.insert
                   (toTreeRoot partial)
                   (Node phash (One (Left (ln, rn))))
                   completed
        _ -> completed
    nextCompleted = Map.foldr movePartials Map.empty $ treeDBPartials treeDB

addPartials :: TreeDB alg a -> [PartialTree alg a] -> TreeDB alg a
addPartials treeDB partials = addCompleted updatedTreeDB completed
  where
    expand this@(Node _ (Just inner)) = Map.insert (toTreeRoot this) this $ case inner of
      Left (l, r) -> Map.union (expand l) (expand r)
      Right _     -> Map.empty
    expand openNode = Map.singleton (toTreeRoot openNode) openNode
    allTrees = Map.unions $ map expand partials
    isOpen (Node _ Nothing) = True
    isOpen _                = False
    (open', notOpen) = Map.partition isOpen allTrees
    open = Map.map (const ()) open'
    isCompleteLeaf (Node _ (Just (Right _))) = True
    isCompleteLeaf _                         = False
    (completed', realPartials) = Map.partition isCompleteLeaf notOpen
    completed = Map.map (\(Node h (Just (Right a))) -> Node h $ One $ Right a) completed'
    updatedTreeDB = treeDB
                      { treeDBPartials = Map.union (treeDBPartials treeDB) realPartials
                      , treeDBOpenPartials = Map.union (treeDBOpenPartials treeDB) open
                      }

-------------------------------------------------------------------------------
-- An interface for transactions.

-- |Transaction must be:
-- - representable in bytestrings,
-- - allow us to create a special mining transaction,
-- - they must provide us with the fee we can spend,
-- - they must answer whether they will play with a set of available UTXOs.
class Serialise tx => Transaction tx where
  -- |A type of UTXO. It may differ between implementations.
  -- One good choice is Hashed alg Something.
  type UTXO tx

  -- |The fee transaction provides.
  transactionFee :: tx -> Integer

  -- |Will transaction play out?
  canTransactionPlay :: Ord (UTXO tx) => Set.Set (UTXO tx) -> tx -> Maybe (Set.Set (UTXO tx))

  -- |Create mining transaction.
  createMiningTransaction :: UTXO tx -> String -> Integer -> Maybe tx


-------------------------------------------------------------------------------
-- Mining pool.

data MiningConfig = MiningConfig
                  { miningConfigFeeMultipler         :: Integer
                  , miningConfigSizeMultiplier       :: Integer
                  }

data Assessment alg = Assessment
                { assessmentComplete                 :: Integer
                -- ^we sort primarily on this field which is linear composition of fee and size.
                , assessmentFee                      :: Integer
                -- ^we descend on fee, because greater fees are better.
                , assessmentSize                     :: Int
                -- ^and ascend on size because lesser sizes are better.
                , assessmentTieBreaker               :: Hash alg
                }
                deriving (Eq, Show)

instance Ord (Assessment alg) where
  compare a1 a2 = on assessmentComplete `thenOn` (negate . assessmentFee) `thenOn` assessmentSize
    where
      on :: forall x . Ord x => (Assessment alg -> x) -> Ordering
      on f = compare (f a1) (f a2)
      thenOn :: forall x . Ord x => Ordering -> (Assessment alg -> x) -> Ordering
      thenOn r f = if r == EQ then on f else r

data MiningPool alg tx = MiningPool
                       { miningPoolConfig            :: MiningConfig
                       -- ^Mining configuration: assessment multiplers, etc.
                       , miningPoolBestAssessedTxs   :: Map (Assessment alg) tx
                       -- ^To select transactions for mining, perform minViewWithKeys.
                       -- We use Map here as a priority queue, it is good enough for that.
                       , miningPoolBestAssessedSize  :: Int
                       -- ^Size in bytes of all assessed transactions.
                       , miningPoolAllAssessedTxs    :: Set tx
                       -- ^these transactions are unassessed - they might be not
                       --  worth mining right now, etc.
                       --  They are kept in Set for better strictness. For now.
                       }

-- |Create a pool.
newMiningPool :: MiningConfig -> MiningPool alg tx
newMiningPool cfg = MiningPool cfg Map.empty 0 Set.empty

-- |Add a transaction into a pool.
receiveTransaction :: Transaction tx => MiningPool alg tx -> tx -> MiningPool alg tx
receiveTransaction miningPool tx = undefined
  where
    serialisedTx = encode tx

main :: IO ()
main = return ()

