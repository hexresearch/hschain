{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE MultiWayIf           #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE UndecidableInstances #-}
-- |
module HSChain.PoW.Consensus
  ( -- * Block index
    BlockIndex(..)
  , BH(..)
  , insertIdx
  , lookupIdx
  , blockIndexFromGenesis
  , traverseBlockIndex
  , traverseBlockIndexM
    -- *
  , StateView(..)
  , BlockDB(..)
  , Consensus(..)
  , blockIndex
  , bestHead
  , candidateHeads
  , badBlocks
  , requiredBlocks
  , blockDB
    --
  , consensusGenesis
  , Head(..)
  , processHeader
  , processBlock
  , HeaderError(..)
  , BlockError(..)
  ) where

import Control.Category ((>>>))
import Control.Monad
import Control.Monad.Except
import Control.Monad.State.Strict
import Data.List          (sortOn)
import Data.Functor.Identity
import Data.Map           (Map)
import Data.Maybe
import Data.Sequence      (Seq(Empty,(:<|),(:|>)),(|>))
import Data.Set           (Set)
import Data.Ord           (Down(..))
import qualified Data.Map        as Map
import qualified Data.Set        as Set
import qualified Data.Sequence   as Seq
import Lens.Micro
import Lens.Micro.Mtl
import Lens.Micro.TH

import HSChain.Types.Merkle.Types
import HSChain.Crypto (Hashed)
import HSChain.PoW.Types


----------------------------------------------------------------
-- Block index
----------------------------------------------------------------

-- | Index of all blocks in chain or rather tree. It's append only
--   structure and we never remove headers from it.
--
--   Implementation note. In order to keep memory use to a minimum we
--   try to share everything and ensure that no 'bhBID' points to
--   value that isn't one that part of underlying map.
data BlockIndex b = BlockIndex
  -- FIXME: We'll want to add blocks incrementally into compact to
  --        make things easier for GC
  { _blockIDMap :: Map (BlockID b) (BH b)
  }

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

-- | Find path between nodes in block index
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

blockIndexFromGenesis :: (IsMerkle f, BlockData b) => GBlock b f -> BlockIndex b
blockIndexFromGenesis genesis
  | blockHeight genesis /= Height 0 = error "Genesis must be H=0"
  | otherwise                       = BlockIndex $ Map.singleton bid bh
  where
    bid = blockID genesis
    bh  = BH { bhHeight   = Height 0
             , bhBID      = bid
             , bhWork     = blockWork genesis
             , bhPrevious = Nothing
             , bhData     = merkleMap merkleHashed $ blockData genesis
             }

lookupIdx :: (Ord (BlockID b)) => BlockID b -> BlockIndex b -> Maybe (BH b)
lookupIdx bid (BlockIndex idx) = Map.lookup bid idx

insertIdx :: (Ord (BlockID b)) => BH b -> BlockIndex b -> BlockIndex b
insertIdx bh (BlockIndex idx) = BlockIndex $ Map.insert (bhBID bh) bh idx

-- | Unpacked header for storage in block index. We use this data type
--   instead of @[(BlockID, Header b)]@ in order to reduce memory use
--   since we'll keep many thousands on these values in memory.
data BH b = BH
  { bhHeight   :: !Height         --
  , bhBID      :: !(BlockID b)    --
  , bhWork     :: !(Work b)       --
  , bhPrevious :: !(Maybe (BH b)) --
  , bhData     :: !(b Hashed)     --
  }

deriving instance (Show (Work b), Show (BlockID b), Show (b Hashed)) => Show (BH b)

instance BlockData b => Eq (BH b) where
  a == b = bhBID a == bhBID b



----------------------------------------------------------------
-- Blockchain state handling
----------------------------------------------------------------

data BlockDB m b = BlockDB
  { storeBlock     :: Block b -> m ()
  , retrieveBlock  :: BlockID b -> m (Maybe (Block  b))
  , retrieveHeader :: BlockID b -> m (Maybe (Header b))
  }

-- | View on state of blockchain. It's expected that store is backed
--   by database and in-memory overlay is used to work with multiple
--   heads of blockchain. One important constraint is that view should
--   remain valid even if underlying database gets updated.
data StateView m b = StateView
  { stateBID    :: BlockID b
    -- ^ Hash of block for which state is calculated
  , applyBlock  :: Block b -> m (Maybe (StateView m b))
    -- ^ Apply block on top of current state. Function should throw
    --   exception if supplied block is not child block of current
    --   head. Function should return @Nothing@ if block is not valid
  , revertBlock :: m (StateView m b)
    -- ^ Revert block. Underlying implementation should maintain
    --   enough information to allow rollbacks of reasonable depth.
    --   It's acceptable to fail for too deep reorganizations.
  , flushState  :: m ()
    -- ^ Persist snapshot in the database.
  }


----------------------------------------------------------------
-- Tracking of PoW consensus
----------------------------------------------------------------

-- | Complete description of PoW consensus
data Consensus m b = Consensus
  { _blockIndex     :: BlockIndex b
    -- ^ Index of all known headers that have enough work in them and
    --   otherwise valid. Note that it may include headers of blocks
    --   which turned out to be invalid.
  , _bestHead       :: (BH b, StateView m b)
    -- ^ Best head of blockchain. It's validated block with most work
    --   in it
  , _candidateHeads :: [Head b]
    -- ^ List of candidate heads. It's heads which have more work than
    --   current best head but we don't have all blocks in chain so we
    --   can't rewind to this state.
  , _badBlocks      :: Set (BlockID b)
    -- ^ Set of blocks that failed validation and some of descendant
    --   blocks. When block fails validation we add its BID
    --   here. However we only add its descendants when
  , _requiredBlocks :: Set (BlockID b)
    -- ^ Set of blocks that we need to fetch from other ndoes.
  , _blockDB        :: BlockDB m b
  }

consensusGenesis
  :: (Monad m, BlockData b)
  => Block b
  -> StateView m b
  -> BlockDB   m b
  -> Consensus m b
consensusGenesis genesis sview db = Consensus
  { _blockIndex     = idx
  , _bestHead       = (bh, sview)
  , _candidateHeads = []
  , _badBlocks      = Set.empty
  , _requiredBlocks = Set.empty
  , _blockDB        = db
  }
  where
    bid = blockID genesis
    idx = blockIndexFromGenesis genesis
    Just bh = lookupIdx bid idx

-- | Candidate head of blockchain. We maintain both head and sequence
--   of blocks which are not reachable. Head should be also viable
--   that is it shouldn't descend from any known bad blocks. So
--   following invariants should be maintained:
--
--   > last bchMissing == bchHead
--   > head bchMissing ∈ requiredBlocks
--   > none of bchMissing ∈ badBlocks
data Head b = Head
  { bchHead    :: BH b
  , bchMissing :: Seq (BH b)
  }

makeLenses ''Consensus

----------------------------------------------------------------
-- Transitions
----------------------------------------------------------------

-- | List of reasons to reject header
data HeaderError
  = ErrH'KnownHeader
    -- ^ Not strictly an error. We have this header already
  | ErrH'HeightMismatch
    -- ^ Block has invalid height
  | ErrH'UnknownParent
    -- ^ We don't have parent block
  | ErrH'ValidationFailure
  | ErrH'BadParent
  deriving (Show,Eq)

data BlockError
  = ErrB'UnknownBlock
  | ErrB'InvalidBlock
  deriving (Show,Eq)


----------------------------------------------------------------
--
----------------------------------------------------------------

-- | Add new header. Header is only accepted only if we already have
--   header for previous block and header is otherwise valid.
processHeader
  :: (BlockData b, Monad m)
  => Header b
  -> ExceptT HeaderError (StateT (Consensus m b) m) ()
-- FIXME: Decide what to do with time?
-- FIXME: Decide how to track difficulty adjustment
processHeader header = do
  index    <- use blockIndex
  -- If we already have header do nothing
  case bid `lookupIdx` index of
    Just _  -> throwError ErrH'KnownHeader
    Nothing -> return ()
  -- Check that we have parent block
  parent <- maybe (throwError ErrH'UnknownParent) return
          $ flip lookupIdx index =<< prevBlock header
  -- Reject if header is descentdant of known bad block
  do bad <- use badBlocks
     when (bhBID parent `Set.member` bad) $ throwError ErrH'BadParent
  -- Perform header validations
  unless (succ (bhHeight parent) == blockHeight header)
    $ throwError ErrH'HeightMismatch
  unless (validateHeader header)
    $ throwError ErrH'ValidationFailure
  -- Create new index entry
  let work = bhWork parent <> blockWork header
      bh   = BH { bhHeight   = blockHeight header
                , bhBID      = bid
                , bhWork     = work
                , bhPrevious = Just parent
                , bhData     = blockData header
                }
  ----------------------------------------
  -- Update candidate heads
  candidates <- do
    bestWork   <- use $ bestHead . _1 . to bhWork
    candidates <- use candidateHeads
       -- If new header doesn't have more work than current head no
       -- adjustment is needed
    if | work <= bestWork                 -> return candidates
       -- Otherwise we try to append it to existing candidate head
       | Just c <- growHead bh candidates -> return c
       -- Failing that we try grow completely new head and find which
       -- blocks are not available yet.
       --
       -- This step could fail if we encounter known bad block. In
       -- this case we mark its every descendant as bad too,
       | otherwise                        -> do
           c <- growNewHead bh
           return $ c : candidates
  -----------------------------------------
  -- Finish
  candidateHeads .= candidates
  blockIndex     %= insertIdx  bh
  requiredBlocks %= Set.insert bid
  where
    bid = blockID header


-- | Try to append new header to list of candidates. It returns new
--   list if proposed head is child of any existing heads. Otherwise
--   it return @Nothing@ to signal that new head should be created. We
--   don't try to it here since logic is much more complicated
growHead :: (BlockData b) => BH b -> [Head b] -> Maybe [Head b]
growHead bh (b:bs)
  | bhPrevious bh == Just (bchHead b) = Just $ Head { bchHead    = bh
                                                    , bchMissing = bchMissing b |> bh
                                                    }
                                             : bs
  | otherwise                         = (b :) <$> growHead bh bs
growHead _ [] = Nothing


-- | Grow new head.
growNewHead
  :: (BlockData b, Monad m)
  => BH b
  -> ExceptT HeaderError (StateT (Consensus m b) m) (Head b)
growNewHead bh = do
  best       <- use $ bestHead . _1
  bad        <- use badBlocks
  missing    <- use requiredBlocks
  --
  let updates   = traverseBlockIndex (const id) (flip (|>)) best bh Empty
      exclude b = b /= bh
               && bhBID b `Set.notMember` missing
  case Seq.dropWhileL (\b -> bhBID b `Set.notMember` bad) updates of
    Empty -> return $ Head
               { bchHead    = bh
               , bchMissing = Seq.dropWhileL exclude updates
               }
    _ :<| bads -> do
      forM_ bads $ \b -> badBlocks %= Set.insert (bhBID b)
      throwError ErrH'BadParent


----------------------------------------------------------------
--
----------------------------------------------------------------

-- | Add new block. We only accept block if we already have valid header. Note
processBlock
  :: (BlockData b, Monad m)
  => Block b
  -> ExceptT BlockError (StateT (Consensus m b) m) ()
processBlock block = do
  index <- use blockIndex
  _bh   <- maybe (throwError ErrB'UnknownBlock) return
         $ bid `lookupIdx` index
  -- Since we got block we should remove it from set of block we want
  -- to download regardless of validation results
  requiredBlocks %= Set.delete bid
  -- Perform context free validation of block. If we validation fails
  -- we will add it to set of bad block and won't write on disk
  case validateBlock block of
    False -> do invalidateBlock bid
                throwError ErrB'InvalidBlock
    True  -> do db <- use blockDB
                lift $ lift $ storeBlock db block
  -- Now we want to find candidate heads that are better than current
  -- head and reachable at the same time.
  --
  -- Note that candidate head could be only partially reachable and
  -- still better than current head:
  --
  --   A -> B -> C -> D -> E -> F
  --             ^    ^    ^    ^ we only have header
  --             |    |    |- next best head
  --             |    |- block we're processing
  --             |- current best head
  bestCandidate
  where
    bid          = blockID block


-- | Invalidate block. We need to truncate all candidate heads that
--   contains it.
invalidateBlock
  :: (BlockData b, MonadState (Consensus n b) m)
  => BlockID b
  -> m ()
invalidateBlock bid = do
  bestWork <- use $ bestHead . _1 . to bhWork
  candidateHeads %= mapMaybe (truncateBch bestWork)
  badBlocks      %= Set.insert bid
  where
    truncateBch work Head{..} =
      case Seq.takeWhileL (\b -> bhBID b /= bid) bchMissing of
        Empty               -> Nothing
        bs@(_ :|> b)
          | bhWork b > work -> Just Head { bchHead    = b
                                         , bchMissing = bs
                                         }
          | otherwise       -> Nothing


-- | Find best reachable candidate block.
bestCandidate
  :: (BlockData b, Monad m)
  => ExceptT BlockError (StateT (Consensus m b) m) ()
bestCandidate = do
  db       <- use blockDB
  bestWork <- use $ bestHead . _1 . to bhWork
  missing  <- use requiredBlocks
  heads    <- use $ candidateHeads . to (  mapMaybe (findBest missing)
                                       >>> filter (\b -> bhWork b > bestWork)
                                       >>> sortOn (Down . bhWork)
                                        )
  case heads of
    []  -> cleanCandidates
    h:_ -> do
      (best,st) <- use bestHead
      let rollback _    = lift . revertBlock
          update   bh s = do
            block <- lift (retrieveBlock db $ bhBID bh) >>= \case
              Nothing -> error "CANT retrieveBlock"
              Just b  -> return b
            lift (applyBlock s block) >>= \case
              Nothing -> throwError (bhBID bh)
              Just b  -> return b
      state' <- lift $ lift
              $ runExceptT
              $ traverseBlockIndexM rollback update best h st
      -- traceShow ("BEST HEAD:",bhBID h) $ return ()
      case state' of
        Left  bid -> invalidateBlock bid >> bestCandidate
        Right s   -> bestHead .= (h,s)   >> cleanCandidates
  where
    findBest missing Head{..} =
      case Seq.takeWhileL (\b -> bhBID b `Set.notMember` missing) bchMissing of
        Empty   -> Nothing
        _ :|> b -> Just b

cleanCandidates :: (BlockData b, MonadState (Consensus n b) m) => m ()
cleanCandidates = do
  bestWork <- use $ bestHead . _1 . to bhWork
  missing  <- use requiredBlocks
  let truncateBch Head{..}
        | bhWork bchHead <= bestWork = Nothing
        | otherwise                  = Just Head
            { bchHead    = bchHead
            , bchMissing = Seq.dropWhileL (\b -> bhBID b `Set.notMember` missing) bchMissing
            }
  candidateHeads %= mapMaybe truncateBch
