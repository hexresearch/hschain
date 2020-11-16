{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE PatternSynonyms        #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE UndecidableInstances   #-}
-- |
module HSChain.PoW.Consensus
  ( -- * Block index
    BlockIndex(..)
  , BH(..)
  , asHeader
  , insertIdx
  , lookupIdx
  , blockIndexFromGenesis
  , blockIndexHeads
  , traverseBlockIndex
  , traverseBlockIndexM
  , makeLocator
    -- *
  , StateView(..)
  -- , createCandidateBlock
  , BlockDB(..)
  , buildBlockIndex
  , Consensus(..)
  , blockIndex
  , bestHead
  , candidateHeads
  , badBlocks
  , requiredBlocks
    --
  , consensusGenesis
  , createConsensus
  , Head(..)
  , processHeader
  , processBlock
  , HeaderError(..)
  , BlockError(..)
  ) where

import Control.Category ((>>>))
import Control.Exception
import Control.Lens     hiding (pattern Empty, (|>), index)
import Control.Monad
import Control.Monad.Except
import Control.Monad.State.Strict
import Data.List          (sortOn)
import Data.Typeable      (Typeable)
import Data.Maybe
import Data.Sequence      (Seq(Empty,(:<|),(:|>)),(|>))
import Data.Set           (Set)
import Data.Ord           (Down(..))
import qualified Data.Aeson      as JSON
import qualified Data.Set        as Set
import qualified Data.Sequence   as Seq
import GHC.Generics (Generic)
import Katip (sl)

import HSChain.Logger
import HSChain.PoW.Types
import HSChain.PoW.BlockIndex
import HSChain.PoW.Store


----------------------------------------------------------------
-- Block index
----------------------------------------------------------------

makeLocator :: BH b -> Locator b
makeLocator  = Locator . takeH 10 . Just
  where
    --
    takeH :: Int -> Maybe (BH b) -> [BlockID b]
    takeH !_ Nothing   = []
    takeH  0 (Just bh) = bhBID bh : case bhPrevious bh of
      Nothing  -> []
      Just bh' -> backoff 2 2 bh'
    takeH  n (Just bh) = bhBID bh : takeH (n-1) (bhPrevious bh)
    --
    backoff :: Int -> Int -> BH b -> [BlockID b]
    backoff !n 1 bh = bhBID bh : case bhPrevious bh of
      Nothing  -> []
      Just bh' -> backoff (n*2) (2*n) bh'
    backoff  n  k bh = case bhPrevious bh of
      Nothing  -> [bhBID bh]
      Just bh' -> backoff n (k-1) bh'


----------------------------------------------------------------
-- Blockchain state handling
----------------------------------------------------------------

-- | View on state of blockchain. Normally it's backed by database
--   with in-memory overlay. Latter is needed in order to work with
--   multiple heads of blockchain. One important constraint is that
--   view should remain valid even if underlying database gets
--   updated.
class BlockData b => StateView view m b | view -> b, view -> m where
  -- | Hash of block for which state is calculated
  stateBID :: view -> BlockID b
  -- | Apply block on top of current state. Function should return
  --   @Nothing@ if block is not valid. It's always called with @BH@
  --   corresponding to given block. If it's not the case it's
  --   caused by bug inc consensus state machine.
  applyBlock :: view -> BlockIndex b -> BH b -> Block b -> m (Either (BlockException b) view)
  -- | Revert block. Underlying implementation should maintain
  --   enough information to allow rollbacks of reasonable depth.
  --   It's acceptable to fail for too deep reorganizations.
  revertBlock :: view -> m view
  -- | Persist snapshot in the database.
  flushState  :: view -> m view
  -- | Check that transaction is valid. Needed for checking
  --   transaction in the mempool.
  checkTx :: view -> Tx b -> m (Either (BlockException b) ())

--   , createCandidateBlockData
--       :: BH b
--       -> Time
--       -> [Tx b]
--       -> m (b Identity)
--     -- ^ Create candidate block out of list of transactions. It won't
--     --   have enough work in it but should be valid otherwise.
--   }

-- createCandidateBlock
--   :: (Monad m)
--   => StateView m b
--   -> BH b
--   -> Time
--   -> [Tx b]
--   -> m (Block b)
-- createCandidateBlock sv bh t txs = do
--   b <- createCandidateBlockData sv bh t txs
--   return Block { blockHeight = succ (bhHeight bh)
--                , blockTime   = t
--                , prevBlock   = Just $ bhBID bh
--                , blockData   = b
--                }


----------------------------------------------------------------
-- Tracking of PoW consensus
----------------------------------------------------------------

-- | Complete description of PoW consensus
data Consensus view m b = Consensus
  { _blockIndex     :: BlockIndex b
    -- ^ Index of all known headers that have enough work in them and
    --   otherwise valid. Note that it may include headers of blocks
    --   which turned out to be invalid.
  , _bestHead       :: (BH b, view, Locator b)
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
  }

consensusGenesis
  :: (Monad m, StateView view m b)
  => Block b
  -> view
  -> Consensus view m b
consensusGenesis genesis sview = Consensus
  { _blockIndex     = idx
  , _bestHead       = (bh, sview, makeLocator bh)
  , _candidateHeads = []
  , _badBlocks      = Set.empty
  , _requiredBlocks = Set.empty
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
data HeaderError b
  = ErrH'KnownHeader
    -- ^ Not strictly an error. We have this header already
  | ErrH'HeightMismatch
    -- ^ Block has invalid height
  | ErrH'UnknownParent
    -- ^ We don't have parent block
  | ErrH'ValidationFailure (BlockException b)
    -- ^ We failed to validate header
  | ErrH'BadParent
    -- ^ Header has acestor which is known bad block.
  deriving stock (Generic)

deriving stock instance (Show (BlockException b)) => Show (HeaderError b)
deriving stock instance (Eq   (BlockException b)) => Eq   (HeaderError b)

instance (Typeable b, Show (BlockException b)) => Exception (HeaderError b)
instance (JSON.ToJSON (BlockException b)) => JSON.ToJSON (HeaderError b)


data BlockError b
  = ErrB'UnknownBlock
  | ErrB'InvalidBlock (BlockException b)
  deriving stock (Generic)

deriving stock instance (Show (BlockException b)) => Show (BlockError b)
deriving stock instance (Eq   (BlockException b)) => Eq   (BlockError b)

instance (Typeable b, Show (BlockException b)) => Exception (BlockError b)
instance (JSON.ToJSON (BlockException b)) => JSON.ToJSON (BlockError b)


----------------------------------------------------------------
--
----------------------------------------------------------------

-- | Add new header. Header is only accepted only if we already have
--   header for previous block and header is otherwise valid.
processHeader
  :: (StateView view m b, MonadIO m)
  => Header b
  -> ExceptT (HeaderError b) (StateT (Consensus view m b) m) ()
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
  now <- getCurrentTime
  validateHeader parent now header >>= \case
    Left err -> throwError $ ErrH'ValidationFailure err
    Right () -> return ()
  -- Create new index entry
  let work = bhWork parent <> blockWork header
      bh   = BH { bhHeight   = blockHeight header
                , bhTime     = blockTime   header
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
  :: (StateView view m b, Monad m)
  => BH b
  -> ExceptT (HeaderError b) (StateT (Consensus view m b) m) (Head b)
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
  :: (StateView view m b, MonadIO m, MonadLogger m)
  => BlockDB m b
  -> Block b
  -> ExceptT (BlockError b) (StateT (Consensus view m b) m) [(BlockID b, BlockException b)]
processBlock db block = do
  use (blockIndex . to (lookupIdx bid)) >>= \case
    Just _  -> return ()
    Nothing -> throwError ErrB'UnknownBlock
  -- Since we got block we should remove it from set of block we want
  -- to download regardless of validation results
  requiredBlocks %= Set.delete bid
  -- Perform context free validation of block. If we validation fails
  -- we will add it to set of bad block and won't write on disk
  validateBlock block >>= \case
    Left err -> do logger DebugS "Context-free block validation failed" (sl "err" err)
                   invalidateBlock bid
                   throwError $ ErrB'InvalidBlock err
    Right () -> do lift $ lift $ storeBlock db block
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
  bestCandidate db
  where
    bid          = blockID block


-- | Invalidate block. We need to truncate all candidate heads that
--   contains it.
invalidateBlock
  :: (StateView view n b, MonadState (Consensus view n b) m)
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
  :: (StateView view m b, MonadLogger m)
  => BlockDB m b
  -> ExceptT (BlockError b) (StateT (Consensus view m b) m) [(BlockID b, BlockException b)]
bestCandidate db = do
  bestWork <- use $ bestHead . _1 . to bhWork
  missing  <- use requiredBlocks
  heads    <- use $ candidateHeads . to (  mapMaybe (findBest missing)
                                       >>> filter (\b -> bhWork b > bestWork)
                                       >>> sortOn (Down . bhWork)
                                        )
  case heads of
    []  -> [] <$ cleanCandidates
    h:_ -> do
      bIdx        <- use blockIndex
      (best,st,_) <- use bestHead
      let rollback _    = lift . revertBlock
          update   bh s = do
            block <- lift (retrieveBlock db $ bhBID bh) >>= \case
              Nothing -> error "CANT retrieveBlock"
              Just b  -> return b
            lift (applyBlock s bIdx bh block) >>= \case
              Left  e -> do logger DebugS "Block apply failed" ( sl "bid" (bhBID bh)
                                                              <> sl "err" e)
                            throwError (bhBID bh, e)
              Right b -> return b
      state' <- lift $ lift
              $ runExceptT
              $ traverseBlockIndexM rollback update best h st
      case state' of
        Left  (bid,e) -> do invalidateBlock bid
                            ((bid,e) :) <$> bestCandidate db
        Right s       -> do bestHead .= (h,s,makeLocator h) >> cleanCandidates
                            pure []
  where
    findBest missing Head{..} =
      case Seq.takeWhileL (\b -> bhBID b `Set.notMember` missing) bchMissing of
        Empty   -> Nothing
        _ :|> b -> Just b

cleanCandidates :: (StateView view n b, MonadState (Consensus view n b) m) => m ()
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

-------------------------------------------------------------------------------
-- Accessing the state view.


-- | Create consensus state out of block index and state view.
createConsensus
  :: (StateView view m b, MonadLogger m)
  => BlockDB m b
  -> view
  -> BlockIndex b
  -> m (Consensus view m b)
-- NOTE: So far we only record full blocks and not headers. Thus we
--       don't have any blocks we want to fetch and all candidate
--       heads have all required blocks.
-- NOTE; We don't track known bad blocks either.
createConsensus db sView bIdx = do
  let c0 = Consensus
        { _blockIndex     = bIdx
        , _bestHead       = (bh, sView, makeLocator bh)
        , _candidateHeads = [ Head b (Seq.singleton b) | b <- blockIndexHeads bIdx ]
        , _badBlocks      = Set.empty
        , _requiredBlocks = Set.empty
        }
      bh = case stateBID sView `lookupIdx` bIdx of
             Just b  -> b
             Nothing -> error "Internal error: state's BID is not in index"
  execStateT (runExceptT (bestCandidate db)) c0
