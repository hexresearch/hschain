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
  , ProposalStorage(..)
  , hoistPropStorageRM
  , makeReadOnlyPS
  ) where

import           Data.Map          (Map)

import Thundermint.Consensus.Types


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
    -- ^ Retrieve block at given height
  , retrieveBlockID    :: Height -> m (Maybe (BlockID alg a))
    -- ^ Retrieve ID block at given height
  , retrieveCommit     :: Height -> m (Maybe (Commit alg a))
    -- ^ Retrieve commit for block at the given height.
  , retrieveLastCommit :: m (Maybe (Commit alg a))
    -- ^ Commit justifying last lock in blockchain. Note that it's
    --   stored separately and will be added to next block when it's
    --   commited
  , storeCommit        :: Writable rw (Commit alg a -> Block alg a -> m ())
    -- ^ Write block into storage
    --
    --   FIXME: we really should track validity of blocks and
    --          commits. It's VERY easy to mess up accidentally
  , closeBlockStorage  :: Writable rw (m ())
    -- ^ Close all handles etc. Functions in the dictionary should not
    --   be called after that
  }


-- | Strip write rights if storage API had any
makeReadOnly :: BlockStorage rw m alg a -> BlockStorage 'RO m alg a
makeReadOnly BlockStorage{..} =
  BlockStorage{ storeCommit       = ()
              , closeBlockStorage = ()
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
               , closeBlockStorage   = fun closeBlockStorage
               }


----------------------------------------------------------------
-- Storage for consensus
----------------------------------------------------------------

-- | Storage for intermediate data used for
data ProposalStorage rw m alg a = ProposalStorage
  { currentHeight       :: m Height
    -- ^ Get current height of storage
  , advanceToHeight     :: Writable rw (Height -> m ())
    -- ^ Advance to given height. If height is different from current
    --   all stored data is discarded
  , retrievePropBlocks  :: Height -> m (Map (BlockID alg a) (Block alg a))
    -- ^ Retrieve blocks
  , storePropBlock      :: Writable rw (Block alg a -> m ())
    -- ^ Store block proposed at given height. If height is different
    --   from height we are at block is ignored.
  }

makeReadOnlyPS :: ProposalStorage rw m alg a -> ProposalStorage 'RO m alg a
makeReadOnlyPS ProposalStorage{..} =
  ProposalStorage { advanceToHeight = ()
                  , storePropBlock  = ()
                  , ..
                  }

hoistPropStorageRM
  :: (forall x. m x -> n x)
  -> ProposalStorage 'RW m alg a
  -> ProposalStorage 'RW n alg a
hoistPropStorageRM fun ProposalStorage{..} =
  ProposalStorage { currentHeight      = fun currentHeight
                  , advanceToHeight    = fun . advanceToHeight
                  , retrievePropBlocks = fun . retrievePropBlocks
                  , storePropBlock     = fun . storePropBlock
                  }
