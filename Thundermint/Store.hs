{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE KindSignatures  #-}
{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
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
  , MempoolInfo(..)
  , hoistMempoolCursor
  , hoistMempool
  , nullMempool
  , nullMempoolAny
  -- * Blockchain invariants checkers
  , BlockchainInconsistency
  , checkStorage
  , checkBlocks
  , checkBlock
  , checkCommits
  , checkCommit
  , checkValidators
  , checkCommitsBlocks
  , checkBlockByCommit
  ) where

import Control.Monad.Trans.Writer

import qualified Katip

import Codec.Serialise           (Serialise)
import Control.Monad             (when)
import Control.Monad.Trans.Class
import Data.Foldable             (forM_)
import Data.Map                  (Map)
import Data.Maybe                (isJust, isNothing, maybe)
import Data.Monoid               ((<>))
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
data BlockchainInconsistency = MissingGenesisBlock
                             -- ^ Missing genesis block
                             | MissingBlock Height
                             -- ^ Missing block at Height
                             | MissingCommit Height
                             -- ^ Missing commit at Height
                             | MissingLocalCommit Height
                             -- ^ Missing local commit at Height
                             | MissingValidatorSet Height
                             -- ^ Missing validator' set at Height
                             | MissingHeaderLastBlockID Height
                             -- ^ Missing validator' set at Height

                             -- * genesis invariants
                             | GenesisHasLastCommit
                             -- ^ genesis block has last commit
                             | GenesisHasHeaderLastBlockID
                             -- ^ genesis block has headerLastBlockID
                             | FirstBlockHasLastCommit
                             -- ^ block at height 1 has blockLastCommit

                             -- * Block invariants
                             | BlockLastCommitHasMisMatchVoteHeight Height
                             -- ^ at height H+1 (block blockLastCommit)' some vote(s) does not refer to block at H
                             | BlockLastCommitHasMisMatchVoteBlockID Height
                             -- ^ lastCommit's preCommits some voteBlockID does not equal to headerLastBlockID or to commitBlockID
                             | BlockLastCommitHasUnknownValidator Height
                             -- ^ some signature(s) in block's commitPrecommits does not belongs to known validators' signatures et
                             | BlockHasMisMatchChainId Height
                             -- ^ block has different chainId from genesis block
                             | BlockHasMisMatchHeightInHeader Height
                             -- ^ block at height H has different H in its header
                             | BlockHasMisMatchHash Height
                             -- ^ hash of block and hash from db miss match

                             -- * Commit invariants
                             | CommitHasUnknownValidator Height
                             -- ^ there is not enought voting power
                             | CommitHasDeficitVotingPower Height
                             -- ^ some signature(s) in commit' commitPrecommits does not belongs to known validators' signatures et

                             -- * Block, Commit couple invariants
                             | CommitBlockHeaderBlockIDMisMatch Height
                             -- ^ headerLastBlockID at height H is not equal to BlockID at previous height
                               deriving (Eq, Show)


-------------------------------------------------------------------------------
-- to validate the block on the fly before accepting/adding
-------------------------------------------------------------------------------

-- | Check that commit validates previous block
checkBlockByCommit :: Monad m =>
                            Height
                            -> Maybe (Block alg a)
                            -> Maybe (Commit alg a)
                            -> m [BlockchainInconsistency]
checkBlockByCommit height block commit = execWriterT $ commitBlockInvariant height block commit



-- | check a block invarinats
checkBlock
  :: (Monad m, Crypto alg, Serialise a) =>
     BS.ByteString
     -> ValidatorSet alg
     -> BlockHash alg (Block alg a)
     -> Height
     -> Block alg a
     -> m [BlockchainInconsistency]
checkBlock chainID valSet blockID h block =
    execWriterT $ blockInvariant chainID (Just valSet) (Just blockID) h (Just block)


checkCommit
  :: Monad m =>
     Height
     -> Maybe (ValidatorSet alg)
     -> Maybe (Commit alg a1)
     -> Maybe (Commit alg a2)
     -> m [BlockchainInconsistency]
checkCommit h vs cmt localCmt = execWriterT $ do
                                   commitInvariant vs h cmt
                                   commitInvariant vs h localCmt

-------------------------------------------------------------------------------
-- Storage checkers
-------------------------------------------------------------------------------

-- | check storage against all consistency invariants
checkStorage
  :: (Monad m, Crypto alg, Serialise a1) =>
     BlockStorage rw m alg a1 -> m [BlockchainInconsistency]
checkStorage storage = do bs <- checkBlocks storage
                          cs <- checkCommits storage
                          vs <- checkValidators storage
                          cbs <- checkCommitsBlocks storage
                          return $ bs <> cs <> vs <> cbs


-- | check all blocks' invarinats
checkBlocks
  :: (Monad m, Crypto alg, Serialise a) =>
     BlockStorage rw m alg a
     -> m [BlockchainInconsistency]
checkBlocks storage = execWriterT $ do
    maxH <- lift $ blockchainHeight storage
    Just genesis <- lift $ retrieveBlock storage (Height 0)
    let genesisChainId = headerChainID $ blockHeader genesis
    forM_ [Height 0 .. maxH]  $ \h -> do
                   b   <- lift $ retrieveBlock storage h
                   bID <- lift $ retrieveBlockID storage h
                   vs  <- lift $ retrieveValidatorSet storage (pred h)
                   blockInvariant genesisChainId vs bID h b



-- | check all commits' invarinats
checkCommits :: Monad m =>
               BlockStorage rw m alg a1
            -> m [BlockchainInconsistency]
checkCommits storage = execWriterT $ do
    maxH <- lift $ blockchainHeight storage
    forM_ [Height 1 .. pred maxH]  $ \h -> do
            cmt <- lift $ retrieveCommit storage h
            vs  <- lift $ retrieveValidatorSet storage h
            commitInvariant vs h cmt

            local_cmt <- lift $ retrieveLocalCommit storage h
            commitInvariant vs h local_cmt

-- | check all validators' invarinats
checkValidators :: Monad m =>
               BlockStorage rw m alg a1
            -> m [BlockchainInconsistency]
checkValidators storage = execWriterT $ do
    maxH <- lift $ blockchainHeight storage
    forM_ [Height 1 .. pred maxH]  $ \h -> do
            vs  <- lift $ retrieveValidatorSet storage h
            validatorInvariant h vs


-- | check all (commit, block) couple invarinats
checkCommitsBlocks :: Monad m =>
               BlockStorage rw m alg a1
            -> m [BlockchainInconsistency]
checkCommitsBlocks storage = do
    maxH <- blockchainHeight storage
    let heights = enumFromTo (Height 1) (pred maxH)

    xs <- mapM  (\h -> execWriterT $ do
                          cmt <- lift $ retrieveCommit storage (pred h)
                          blk  <- lift $ retrieveBlock storage h
                          commitBlockInvariant h blk cmt
                       ) heights

    return $ concat xs




-------------------------------------------------------------------------------
-- validator invariants
-------------------------------------------------------------------------------

validatorInvariant
    ::  (Monad m) =>
       Height
    -> Maybe (ValidatorSet alg)
    -> WriterT [BlockchainInconsistency] m ()
validatorInvariant h vs = do
  when (isNothing vs) $
    tell [MissingValidatorSet h]

-------------------------------------------------------------------------------
-- commit invariants
-------------------------------------------------------------------------------
commitInvariant
    ::  (Monad m) =>
       Maybe (ValidatorSet alg)
    -> Height
    -> Maybe (Commit alg a)
    -> WriterT [BlockchainInconsistency] m ()

commitInvariant _ h Nothing = do tell [MissingCommit h]
commitInvariant  Nothing h@(Height n) _ = do
  when (n > 0) $
    tell  [MissingValidatorSet h]
commitInvariant (Just vs) h (Just Commit{..}) = do
  -- check voting power is enought, i. e. contains at least 2/3 voting power
  when (tot < quorum) $
    tell [CommitHasDeficitVotingPower h]
  -- all signatures in Commit-> commitPrecommits should be known validators' signatures
  when (any null . mapM (validatorByAddr vs) $  map signedAddr commitPrecommits)  $
    tell [CommitHasDeficitVotingPower h]

 where
   power = maybe 0 validatorVotingPower
           . validatorByAddr vs
   tot    = sum [ power a | a <- map signedAddr commitPrecommits]
   quorum = 2 * totalVotingPower vs `div` 3 + 1



-------------------------------------------------------------------------------
-- commit, block couple invariants
-------------------------------------------------------------------------------
commitBlockInvariant
    ::  (Monad m) =>
        Height
    -> Maybe (Block alg a)
    -> Maybe (Commit alg a)
    -> WriterT [BlockchainInconsistency] m ()

commitBlockInvariant _ _ Nothing = tell []
commitBlockInvariant _ Nothing _ = tell []
commitBlockInvariant h@(Height n) (Just Block{..}) (Just Commit{..}) = do
  -- headerLastBlockID at height H must be equal to BlockID at previous height
  when (n > 1 && maybe False (/= commitBlockID) (headerLastBlockID blockHeader)) $
    tell [CommitBlockHeaderBlockIDMisMatch h]



-------------------------------------------------------------------------------
-- block invarinats
-------------------------------------------------------------------------------
blockInvariant
    :: (Monad m, Crypto alg, Serialise a) =>
       BS.ByteString
    -> Maybe (ValidatorSet alg)
    -> Maybe (BlockHash alg (Block alg a))
    -> Height
    -> Maybe (Block alg a)
    -> WriterT [BlockchainInconsistency] m ()
blockInvariant _ _ _ h@(Height n) Nothing = do
  -- genesis block is missing
  when (n == 0) $
    tell [MissingGenesisBlock]
  when (n > 0) $
    tell [MissingBlock h]

blockInvariant chainID validatorS blockID h@(Height n) b@(Just Block{..}) = do

  case validatorS of
    Nothing -> when (n > 1) $
                 tell  [MissingValidatorSet (pred h)]
    Just vs -> -- all signatures in Block -> blockLastCommit -> commitPrecommits should be known validators' signatures
               when ( maybe False (any isNothing . map (validatorByAddr vs . signedAddr))
                                (commitPrecommits <$> blockLastCommit) )  $
                  tell [BlockLastCommitHasUnknownValidator h]

  -- All blocks must have same headerChainID, i.e. headerChainID of  genesis block
  when (chainID /=  (headerChainID blockHeader)) $
    tell [BlockHasMisMatchChainId h]
  -- block at height H has H in its header
  when (headerHeight  blockHeader /= h)   $
    tell [BlockHasMisMatchHeightInHeader h]
  -- hash of block and hash from db should match
  when (blockID /= (blockHash <$> b))   $
    tell [BlockHasMisMatchHash h]
  -- blockLastCommit of genesis block should be Nothing
  when (n == 0 && isJust blockLastCommit) $
    tell [GenesisHasLastCommit]
  -- blockLastCommit of block at height 1 should be Nothing
  when (n == 1 && isJust blockLastCommit) $
    tell [FirstBlockHasLastCommit]
  -- headerLastBlockID of genesis block should be Nothing
  when (n == 0 && isJust (headerLastBlockID blockHeader)) $
    tell [GenesisHasHeaderLastBlockID]
  -- headerLastBlockID of non genesis blocks should be Just
  when (n > 0 && (isNothing $ headerLastBlockID blockHeader) ) $
    tell [MissingHeaderLastBlockID h]
  -- block at height H+1 blockLastCommit all votes refer to block at H
  when ( maybe False (any ((/= pred h) . voteHeight . signedValue)) (commitPrecommits <$> blockLastCommit))  $
    tell [BlockLastCommitHasMisMatchVoteHeight h]
  -- --  block's all voteBlockID equals to headerLastBlockID and equals to commitBlockID
  -- when ( maybe False (\Commit{..} -> any ((\b' -> b' /= Just commitBlockID
  --                                          || b' /= headerLastBlockID blockHeader) <$> voteBlockID <$> signedValue) commitPrecommits) blockLastCommit)  $
  --   tell [BlockLastCommitHasMisMatchVoteBlockID h]
