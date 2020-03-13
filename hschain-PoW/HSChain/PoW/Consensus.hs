{-# LANGUAGE PolyKinds #-}
-- |
module HSChain.PoW.Consensus where

--import Data.List.NonEmpty (NonEmpty)
import Data.Map           (Map)
--import qualified Data.Map.Strict as Map

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
  { blockIDMap :: Map (BlockID b) (BH b)
  }

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


data Locator b = Locator
  { locatorSeq :: [(Height, BlockID b)]
  }

data HeaderRequest b = HeaderRequest (BlockID b) (Locator b)


----------------------------------------------------------------
-- Blockchain state handling
----------------------------------------------------------------

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
  { blockIndex       :: BlockIndex b
    -- ^ Index of all known headers that have enough work in them and
    --   otherwise valid,
  , bestHead         :: (BH b, StateView m b)
    -- ^ Best head of blockchain. It must be verified and we must have
    --   valid state representation
  , detachedLocators :: Maybe (HeaderRequest b)
    -- ^ List of locators that aren't connected to main chain yet and
    --   we need to fetch more headers.
  , missingBlocks    :: [BlockID b]
    -- ^ List of block that we need to fetch. Those are missing blocks that
  }


----------------------------------------------------------------
-- Transitions
----------------------------------------------------------------

processHeader :: Header b -> Consensus m b -> Consensus m b
processHeader = undefined

processLocator :: Locator b -> Consensus m b -> Consensus m b
processLocator = undefined

processBlock :: Block b -> Consensus m b -> m (Consensus m b)
processBlock = undefined
