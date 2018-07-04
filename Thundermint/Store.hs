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
  , hoistBlockStorageRO
  , makeReadOnly
    -- * In memory store for proposals
  , ProposalStorage(..)
  , hoistPropStorageRW
  , hoistPropStorageRO
  , makeReadOnlyPS
    -- * Mempool
  , MempoolCursor(..)
  , Mempool(..)
  , nullMempool
  ) where

import           Data.Map          (Map)

import Thundermint.Crypto.Containers
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

-- | API for persistent storage of blockchain and related
--   information. All assertion about behavior obviously hold only if
--   database backing store is not corrupted.
data BlockStorage rw m alg a = BlockStorage
  { blockchainHeight   :: m Height
    -- ^ Current height of blockchain (height of last commited block).

  , retrieveBlock      :: Height -> m (Maybe (Block alg a))
    -- ^ Retrieve block at given height.
    --
    --   Must return block for every height @0 <= h <= blockchainHeight@
  , retrieveBlockID    :: Height -> m (Maybe (BlockID alg a))
    -- ^ Retrieve ID of block at given height. Must return same result
    --   as @fmap blockHash . retrieveBlock@ but implementation could
    --   do that more efficiently.
  , retrieveCommit     :: Height -> m (Maybe (Commit alg a))
    -- ^ Retrieve commit justifying commit of block at height
    --   @h@. Must return same result as @fmap blockLastCommit . retrieveBlock . next@
    --   but do it more efficiently.
    --
    --   Note that this method returns @Nothing@ for last block since
    --   its commit is not persisted in blockchain yet and there's no
    --   commit for genesis block (h=0)
  , retrieveCommitRound :: Height -> m (Maybe Round)
    -- ^ Retrieve round when commit was made.

  , storeCommit :: Writable rw
      (ValidatorSet alg -> Commit alg a -> Block alg a -> m ())
    -- ^ Write block and commit justifying it into persistent storage.

  , retrieveLocalCommit :: Height -> m (Maybe (Commit alg a))
    -- ^ Retrieve local commit justifying commit of block as known by
    --   node at moment of the commit. Implementation only MUST store
    --   commit for the last block but may choose to store earlier
    --   commits as well.
    --
    --   Note that commits returned by this functions may to differ
    --   from ones returned by @retrieveCommit@ by set of votes since
    --   1) @retrieveCommit@ retrieve commit as seen by proposer not
    --   local node 2) each node collect straggler precommits for some
    --   time interval after commit.

  , retrieveValidatorSet :: Height -> m (Maybe (ValidatorSet alg))
    -- ^ Retrieve set of validators for given round.
    --
    --   Must return validator set for every @0 < h <= blockchainHeight + 1@

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
  BlockStorage { blockchainHeight     = fun blockchainHeight
               , retrieveBlock        = fun . retrieveBlock
               , retrieveBlockID      = fun . retrieveBlockID
               , retrieveCommitRound  = fun . retrieveCommitRound
               , retrieveCommit       = fun . retrieveCommit
               , retrieveLocalCommit  = fun . retrieveLocalCommit
               , retrieveValidatorSet = fun . retrieveValidatorSet
               , storeCommit          = \v c b -> fun (storeCommit v c b)
               , closeBlockStorage    = fun closeBlockStorage
               }

hoistBlockStorageRO
  :: (forall x. m x -> n x)
  -> BlockStorage 'RO m alg a
  -> BlockStorage 'RO n alg a
hoistBlockStorageRO fun BlockStorage{..} =
  BlockStorage { blockchainHeight     = fun blockchainHeight
               , retrieveBlock        = fun . retrieveBlock
               , retrieveBlockID      = fun . retrieveBlockID
               , retrieveCommitRound  = fun . retrieveCommitRound
               , retrieveCommit       = fun . retrieveCommit
               , retrieveLocalCommit  = fun . retrieveLocalCommit
               , retrieveValidatorSet = fun . retrieveValidatorSet
               , ..
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
  , waitForBlockID      :: BlockID alg a -> m (Block alg a)
    -- ^ Wait for block with given block ID. Call will block until
    --   block appears in storage.
  , storePropBlock      :: Writable rw (Block alg a -> m ())
    -- ^ Store block proposed at given height. If height is different
    --   from height we are at block is ignored.

  , allowBlockID        :: Writable rw (Round -> BlockID alg a -> m ())
    -- ^ Mark block ID as one that we could accept
  , blockAtRound        :: Height -> Round -> m (Maybe (Block alg a, BlockID alg a))
    -- ^ Get block at given round and height
  }

makeReadOnlyPS :: ProposalStorage rw m alg a -> ProposalStorage 'RO m alg a
makeReadOnlyPS ProposalStorage{..} =
  ProposalStorage { advanceToHeight = ()
                  , storePropBlock  = ()
                  , allowBlockID    = ()
                  , ..
                  }

hoistPropStorageRW
  :: (forall x. m x -> n x)
  -> ProposalStorage 'RW m alg a
  -> ProposalStorage 'RW n alg a
hoistPropStorageRW fun ProposalStorage{..} =
  ProposalStorage { currentHeight      = fun currentHeight
                  , advanceToHeight    = fun . advanceToHeight
                  , retrievePropBlocks = fun . retrievePropBlocks
                  , waitForBlockID     = fun . waitForBlockID
                  , storePropBlock     = fun . storePropBlock
                  , allowBlockID       = \r bid -> fun (allowBlockID r bid)
                  , blockAtRound       = \h r   -> fun (blockAtRound h r)
                  }

hoistPropStorageRO
  :: (forall x. m x -> n x)
  -> ProposalStorage 'RO m alg a
  -> ProposalStorage 'RO n alg a
hoistPropStorageRO fun ProposalStorage{..} =
  ProposalStorage { currentHeight      = fun currentHeight
                  , advanceToHeight    = ()
                  , retrievePropBlocks = fun . retrievePropBlocks
                  , waitForBlockID     = fun . waitForBlockID
                  , storePropBlock     = ()
                  , allowBlockID       = ()
                  , blockAtRound       = \h r   -> fun (blockAtRound h r)
                  }

----------------------------------------------------------------
--
----------------------------------------------------------------

-- | Cursor into mempool which is used for gossiping data
data MempoolCursor m tx = MempoolCursor
  { pushTransaction :: tx -> m ()
    -- ^ Add transaction to the mempool. It's preliminary checked and
    --   if check fails it immediately discarded
  , advanceCursor   :: m (Maybe tx)
    -- ^ Take transaction from front and advance cursor. If cursor points at the end of qu
  }

-- | Mempool which is used for storing transactions before they're
--   added into blockchain. Transactions are stored in FIFO manner
data Mempool m tx = Mempool
  { takeNTransactiona :: Maybe Int -> m [tx]
    -- ^ Take up to N transactions from mempool. If Nothing is passed
    --   that all transactions will be returned
  , getMempoolCursor  :: m (MempoolCursor m tx)
    -- ^ Get cursor pointing to be
  }

nullMempool :: Monad m => Mempool m ()
nullMempool = Mempool
  { takeNTransactiona = const (return [])
  , getMempoolCursor  = return MempoolCursor
      { pushTransaction = const (return ())
      , advanceCursor   = return Nothing
      }
  }
