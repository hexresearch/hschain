{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE DeriveGeneric    #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures   #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE TypeFamilies     #-}
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
  , MempoolInfo(..)
  , hoistMempoolCursor
  , hoistMempool
  , nullMempool
  , nullMempoolAny
  -- * Blockchain invariants checkers
  , BlockchainInconsistency
  , checkStorage
  , checkProposedBlock
  ) where

import qualified Katip

import Codec.Serialise           (Serialise)
import Control.Monad             ((<=<), foldM)
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Control.Monad.Trans.Writer
import Data.Foldable             (forM_)
import Data.Map                  (Map)
import Data.Maybe                (isNothing, maybe)
import GHC.Generics              (Generic)

import qualified Data.Aeson      as JSON
import qualified Data.Aeson.TH   as JSON
import qualified Data.ByteString as BS


import Thundermint.Consensus.Types
import Thundermint.Crypto
import Thundermint.Crypto.Containers
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
  { currentHeight      :: m Height
    -- ^ Get current height of storage
  , advanceToHeight    :: Writable rw (Height -> m ())
    -- ^ Advance to given height. If height is different from current
    --   all stored data is discarded
  , retrievePropBlocks :: Height -> m (Map (BlockID alg a) (Block alg a))
    -- ^ Retrieve blocks
  , waitForBlockID     :: BlockID alg a -> m (Block alg a)
    -- ^ Wait for block with given block ID. Call will block until
    --   block appears in storage.
  , storePropBlock     :: Writable rw (Block alg a -> m ())
    -- ^ Store block proposed at given height. If height is different
    --   from height we are at block is ignored.

  , allowBlockID       :: Writable rw (Round -> BlockID alg a -> m ())
    -- ^ Mark block ID as one that we could accept
  , blockAtRound       :: Height -> Round -> m (Maybe (Block alg a, BlockID alg a))
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

-- | Statistics about mempool
data MempoolInfo = MempoolInfo
  { mempool'size      :: Int
  -- ^ Number of transactions currently in mempool
  , mempool'added     :: Int
  -- ^ Number of transactions added to mempool since program start
  , mempool'discarded :: Int
  -- ^ Number of transaction discarded immediately since program start
  , mempool'filtered  :: Int
  -- ^ Number of transaction removed during filtering
  }
  deriving (Show,Generic)
JSON.deriveJSON JSON.defaultOptions
  { JSON.fieldLabelModifier = drop 8 } ''MempoolInfo

instance Katip.ToObject MempoolInfo
instance Katip.LogItem  MempoolInfo where
  payloadKeys Katip.V0 _ = Katip.SomeKeys []
  payloadKeys _        _ = Katip.AllKeys

-- | Cursor into mempool which is used for gossiping data
data MempoolCursor m alg tx = MempoolCursor
  { pushTransaction :: tx -> m (Maybe (Hash alg))
    -- ^ Add transaction to the mempool. It's preliminary checked and
    --   if check fails it immediately discarded. If transaction is
    --   accepted its hash is computed and returned
  , advanceCursor   :: m (Maybe tx)
    -- ^ Take transaction from front and advance cursor. If cursor
    -- points at the end of queue nothing happens.
  }

-- | Mempool which is used for storing transactions before they're
--   added into blockchain. Transactions are stored in FIFO manner
data Mempool m alg tx = Mempool
  { peekNTransactions :: Maybe Int -> m [tx]
    -- ^ Take up to N transactions from mempool. If Nothing is passed
    --   that all transactions will be returned. This operation does
    --   not alter mempool state
  , filterMempool     :: m ()
    -- ^ Remove transactions that are no longer valid from mempool
  , getMempoolCursor  :: m (MempoolCursor m alg tx)
    -- ^ Get cursor pointing to be
  , mempoolStats      :: m MempoolInfo
    -- ^ Number of elements in mempool
  }

hoistMempoolCursor :: (forall a. m a -> n a) -> MempoolCursor m alg tx -> MempoolCursor n alg tx
hoistMempoolCursor fun MempoolCursor{..} = MempoolCursor
  { pushTransaction = fun . pushTransaction
  , advanceCursor   = fun advanceCursor
  }

hoistMempool :: Functor n => (forall a. m a -> n a) -> Mempool m alg tx -> Mempool n alg tx
hoistMempool fun Mempool{..} = Mempool
  { peekNTransactions = fun . peekNTransactions
  , filterMempool     = fun filterMempool
  , getMempoolCursor  = hoistMempoolCursor fun <$> fun getMempoolCursor
  , mempoolStats      = fun mempoolStats
  }


nullMempoolAny :: (Monad m) => Mempool m alg tx
nullMempoolAny = Mempool
  { peekNTransactions = const (return [])
  , filterMempool     = return ()
  , mempoolStats      = return $ MempoolInfo 0 0 0 0
  , getMempoolCursor  = return MempoolCursor
      { pushTransaction = const $ return Nothing
      , advanceCursor   = return Nothing
      }
  }

nullMempool :: (Monad m) => Mempool m alg ()
nullMempool = nullMempoolAny

----------------------------------------------------------------
-- validate blockchain invariants
----------------------------------------------------------------

-- | Blockchain inconsistency types
data BlockchainInconsistency
  = MissingBlock        Height
  -- ^ Missing block at given height.
  | MissingLocalCommit  Height
  -- ^ Commit justifying commit of block at height H is missing
  | MissingValidatorSet Height
  -- ^ Validator set for block at height H is missing

  | BlockWrongChainID  Height
  -- ^ Block at height H has blockchain ID different from genesis block
  | BlockHeightMismatch Height
  -- ^ Height in header of block at height H is not equal to H
  | BlockValidatorHashMismatch Height
  -- ^ Hash of set of validators in block header does not match hash
  --   of actual set
  | BlockInvalidPrevBID Height
  -- ^ Previous block is does not match ID of block commited to
  --   blockchain.
  | GenesisHasLastCommit
  -- ^ Genesis block has last commit field set.
  | GenesisHasHeaderLastBlockID
  -- ^ Block ID of previous block is set in genesis block
  | FirstBlockHasLastCommit
  -- ^ Block at H=1 has last commit field set.
  | BlockMissingLastCommit Height
  -- ^ Block with H>1 last commit field is @Nothing@
  | InvalidCommit Height String
  -- ^ Commit embedded into block at height H (it justifies commit of
  --   block H-1) is invalid for some reason
  | InvalidLocalCommit Height String
  -- ^ Commit which justified commit of block at height H is invalid
  --   for some reason.
  deriving (Eq, Show)


-------------------------------------------------------------------------------
-- Invariant checking for storage and proposed blocks
-------------------------------------------------------------------------------

-- | check storage against all consistency invariants
checkStorage
  :: (Monad m, Crypto alg, Serialise a, Serialise (PublicKey alg))
  => BlockStorage rw m alg a
  -> m [BlockchainInconsistency]
checkStorage storage = execWriterT $ do
  maxH         <- lift $ blockchainHeight storage
  Just genesis <- lift $ retrieveBlock storage (Height 0)
  let genesisChainId = headerChainID $ blockHeader genesis
  --
  forM_ [Height 0 .. maxH] $ \case
    Height 0 -> genesisBlockInvariant genesis
    h        -> checkRequire $ do
      -- Block, ID and validator set must be present
      block  <- require [MissingBlock h]        $ retrieveBlock        storage h
      bid    <- require [MissingBlock h]        $ retrieveBlockID      storage h
      vset   <- require [MissingValidatorSet h] $ retrieveValidatorSet storage h
      commit <- require [MissingLocalCommit h]  $ retrieveLocalCommit  storage h
      -- Data for previous step.
      --
      -- NOTE: We don't show any errors for missing BID for previous
      --       block since we detected when checking for previous height
      mprevVset <- lift $ lift $ retrieveValidatorSet storage (pred h)
      prevBID   <- require [] $ retrieveBlockID storage (pred h)
      --
      lift $ blockInvariant genesisChainId h prevBID (mprevVset,vset) block
      lift $ commitInvariant (InvalidLocalCommit h)
        h bid vset commit


-- | Check that block proposed at given height is correct in sense all
--   blockchain invariants hold
checkProposedBlock
  :: (Monad m, Crypto alg, Serialise a, Serialise (PublicKey alg))
  => BlockStorage rw m alg a
  -> Height
  -> Block alg a
  -> m [BlockchainInconsistency]
checkProposedBlock storage h block = do
  Just genesis <- retrieveBlock        storage (Height 0)
  Just prevBID <- retrieveBlockID      storage (pred h)
  Just vset    <- retrieveValidatorSet storage  h
  mprevVset    <- retrieveValidatorSet storage (pred h)
  execWriterT $ blockInvariant
    (headerChainID $ blockHeader genesis) h prevBID (mprevVset,vset) block





-- | Check invariants for genesis block
genesisBlockInvariant
  :: Monad m
  => Block alg a
  -> WriterT [BlockchainInconsistency] m ()
genesisBlockInvariant Block{blockHeader = Header{..}, ..} = do
  -- It must have height 0
  (headerHeight == Height 0)
    `orElse` BlockHeightMismatch (Height 0)
  -- Last commit it must be Nothing
  isNothing blockLastCommit
    `orElse` GenesisHasLastCommit
  -- Last block must be Nothing
  isNothing headerLastBlockID
    `orElse` GenesisHasHeaderLastBlockID


-- | Check invariant for block at height > 0
blockInvariant
  :: (Monad m, Crypto alg, Serialise a, Serialise (PublicKey alg))
  => BS.ByteString
  -- ^ Blockchain ID
  -> Height
  -- ^ Height of block
  -> BlockID alg a
  -- ^ Block ID of previous block
  -> (Maybe (ValidatorSet alg), ValidatorSet alg)
  -- ^ Validator set for previous and current height
  -> Block alg a
  -- ^ Block to check
  -> WriterT [BlockchainInconsistency] m ()
blockInvariant _ h _ _ _
  | h <= Height 0 = error "blockInvariant called with invalid parameters"
blockInvariant chainID h prevBID (mprevValSet, valSet) Block{blockHeader=Header{..}, ..} = do
  -- All blocks must have same chain ID, i.e. chain ID of
  -- genesis block
  (chainID == headerChainID)
    `orElse` BlockWrongChainID h
  -- Block at height H has H in its header
  (headerHeight == h)
    `orElse` BlockHeightMismatch h
  -- Previous block ID in header must match actual BID of previous
  -- block
  (headerLastBlockID == Just prevBID)
    `orElse` BlockInvalidPrevBID h
  -- Validators' hash does not match correct one
  (headerValidatorsHash == hash valSet)
    `orElse` BlockValidatorHashMismatch h

  -- Validate commit of previous block
  case (headerHeight, blockLastCommit) of
    -- Last commit at H=1 must be Nothing
    (Height 1, Nothing) -> return ()
    (Height 1, Just _ ) -> tell [FirstBlockHasLastCommit]
    -- Otherwise commit must be present and valid
    (_, Nothing       ) -> tell [BlockMissingLastCommit h]
    (_, Just commit   )
      | Just prevValSet <- mprevValSet
        -> commitInvariant (InvalidCommit h) (pred h) prevBID prevValSet commit
      | otherwise
        -> tell [InvalidCommit h "Cannot validate commit"]

commitInvariant
  :: (Monad m)
  => (String -> BlockchainInconsistency)
  -> Height
  -> BlockID alg a
  -> ValidatorSet alg
  -> Commit alg a
  -> WriterT [BlockchainInconsistency] m ()
commitInvariant mkErr h bid valSet Commit{..} = do
  -- It must justify commit of correct block!
  (commitBlockID == bid)
    `orElse` mkErr "Commit is for wrong block"
  -- All votes should be for previous height
  do let invalid = [ svote
                   | svote <- commitPrecommits
                   , h /= voteHeight (signedValue svote)
                   ]
     null invalid
       `orElse` mkErr "Commit contains votes for invalid height"
  -- All votes must be in same round
  do let rounds = voteRound . signedValue <$> commitPrecommits
     case rounds of
       r:rs | all (==r) rs -> return ()
       _                   -> tell [mkErr "Commit contains votes for different rounds"]
  -- Commit has enough (+2/3) voting power, doesn't have votes
  -- from unknown validators, doesn't have duplicate votes, and
  -- vote goes for correct block
  let mvoteSet = foldM (flip insertSigned) (emptySignedSet valSet) commitPrecommits
  case mvoteSet of
    InsertConflict _ -> tell [mkErr "Conflicting votes"]
    InsertDup        -> tell [mkErr "Duplicate votes"]
    InsertUnknown  _ -> tell [mkErr "Votes from unknown source"]
    InsertOK vset    -> case majority23 vset of
      Nothing        -> tell [mkErr "Commit doesn't have 2/3+ majority"]
      Just v
        | voteBlockID v == Just bid -> return ()
        | otherwise
          -> tell [mkErr "2/3+ majority for wrong block"]


checkRequire
  :: Monad m
  => ExceptT [BlockchainInconsistency] (WriterT [BlockchainInconsistency] m) ()
  -> WriterT [BlockchainInconsistency] m ()
checkRequire m = runExceptT m >>= \case
  Left  e  -> tell e
  Right () -> return ()

require :: (Monad m, Monoid w) => e -> m (Maybe a) -> ExceptT e (WriterT w m) a
require e = maybe (throwE e) return <=< lift . lift

orElse :: Monad m => Bool -> e -> WriterT [e] m ()
orElse True  _ = return ()
orElse False e = tell [e]
