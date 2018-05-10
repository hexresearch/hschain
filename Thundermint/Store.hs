{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE KindSignatures  #-}
{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies    #-}
-- |
-- Abstract API for storing of blockchain. Storage works as follows:
--
--  * Blockchain is stored in some databases (on in memory for testing
--    purposes)
--
--  * Rounds' state is kept in memory so it will be lost in case of
--    crash but incoming messages are stored in write ahead log so
--    they could be replayed.
module Thundermint.Store (
    -- * Block storage
    Access(..)
  , Writable
  , BlockStorage(..)
  , hoistBlockStorageRW
  , makeReadOnly
    -- * In memory store for proposals
  , BlockMap
  -- ,
  --   -- * Write ahead log
  --   , 
    -- * In-memory storage for information about current height
  -- , HeightStorage(..)
  ) where

-- import qualified Data.Map        as Map
import           Data.Map          (Map)
import           Data.Set          (Set)

import Thundermint.Consensus.Types
-- import Thundermint.Crypto

----------------------------------------------------------------
-- Abstract API for storing data
----------------------------------------------------------------

-- | Access rights for storage
data Access = RO                -- ^ Read-only access
            | RW                -- ^ Read-write access
            deriving (Show)

type family Writable (rw :: Access) a where
  Writable 'RO a = ()
  Writable 'RW a = a

-- | API fort persistent storage of blockchain and related
--   information. We store blocks that are already commited and
--   information that will be commited in next block (commit for last
--   block).
data BlockStorage rw m alg a = BlockStorage
  { blockchainHeight   :: m Height
    -- ^ Current height of blockchain
  , retrieveBlock      :: Height -> m (Maybe (Block alg a))
  , retrieveBlockID    :: Height -> m (Maybe (BlockID alg a))
  , retrieveCommit     :: Height -> m (Maybe (Commit alg a))
    -- ^ Get block at given height
  , retrieveLastCommit :: m (Maybe (Commit alg a))
    -- ^ Commit justifying last lock in blockchain. Note that we need
    --   to store it separately from blockchain sine it will be part
    --   of next block.
  , storeCommit        :: Writable rw (Commit alg a -> Block alg a -> m ())
    -- ^ Write block into storage
    --
    --   FIXME: we really should track validity of blocks and
    --          commits. It's VERY easy to mess up accidentally

  , retrievePropBlocks  :: Height -> m (Map (BlockID alg a) (Block alg a))
  , retrieveStoredProps :: m (Height, Set (BlockID alg a))
  , storePropBlock      :: Writable rw (Height -> Block alg a -> m ())
  }


-- | Strip write rights if storage API had any
makeReadOnly :: BlockStorage rw m alg a -> BlockStorage 'RO m alg a
makeReadOnly BlockStorage{..} =
  BlockStorage{ storeCommit    = ()
              , storePropBlock = ()
              , ..
              }

hoistBlockStorageRW
  :: (forall x. m x -> n x)
  -> BlockStorage 'RW m alg a
  -> BlockStorage 'RW n alg a
hoistBlockStorageRW fun BlockStorage{..} =
  BlockStorage { blockchainHeight    = fun blockchainHeight
               , retrieveBlock       = fun . retrieveBlock
               , retrieveBlockID     = fun . retrieveBlockID
               , retrieveCommit      = fun . retrieveCommit
               , retrieveLastCommit  = fun retrieveLastCommit
               , storeCommit         = \c b -> fun (storeCommit c b)
               , retrievePropBlocks  = fun . retrievePropBlocks
               , retrieveStoredProps = fun retrieveStoredProps
               , storePropBlock      = \h b -> fun (storePropBlock h b)
               }


----------------------------------------------------------------
-- In memory proposed block storage
----------------------------------------------------------------

type BlockMap alg a = Map (BlockID alg a) (Block alg a)

----------------------------------------------------------------
--
----------------------------------------------------------------

-- data WALMessage alg a
--   = 
-- -- | Write ahead log. Here we store all incoming messages for given
-- --   height. It should be used for
-- data WAL alg a = WAL
--   { 
--   }
  
  
--   , retrieveProposals  :: m (Map Round (Proposal alg a))
--     -- ^ All proposals
--   , storeProposal    :: Writable rw ( Proposal alg a
--                                    -> m (Map Round (Proposal alg a))
--                                     )
--     -- ^ Store proposal and return map of all proposals for current
--     --   height. Throws error if height does not match current height

--   , retrievePreVotes   :: m (HeightVoteSet 'PreVote      alg a)
--     -- ^ All prevotes for current height
--   , storePreVote     :: Writable rw ( Signed 'Verified alg (Vote 'PreVote alg a)
--                                    -> m (HeightVoteSet 'PreVote alg a)
--                                     )

--   , retrievePreCommits :: m (HeightVoteSet 'PreCommit alg a)
--     -- ^ All precommits for current height
--   , storePreCommit   :: Writable rw ( Signed 'Verified alg (Vote 'PreCommit alg a)
--                                    -> m (HeightVoteSet 'PreCommit alg a)
--                                     )
--   }


-- -- | Change underlying monad 
-- hoistStorageRW :: (forall a. m a -> n a)
--                -> BlockStorage 'RW m alg a
--                -> BlockStorage 'RW n alg a
-- hoistStorageRW f BlockStorage{..} = BlockStorage
--   { blockchainHeight   = f blockchainHeight
--   , blockAtHeight      = f . blockAtHeight
--   , retrieveLastCommit = f retrieveLastCommit
--   , storeLastCommit    = f . storeLastCommit
--   , retrieveProposals  = f retrieveProposals
--   , storeProposal      = f . storeProposal
--   , retrievePreVotes   = f retrievePreVotes
--   , storePreVote       = f . storePreVote
--   , retrievePreCommits = f retrievePreCommits
--   , storePreCommit     = f . storePreCommit
--   }



-- ----------------------------------------------------------------
-- -- Storage for used for deciding on next block
-- ----------------------------------------------------------------

-- -- | Storage of 
-- data HeightStorage rw m alg a = HeightStorage
--   {
--   } 
