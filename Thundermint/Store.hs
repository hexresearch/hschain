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
  -- * Blockchain invariants checkers
  , checkBlocks
  , checkCommits
  , checkValidators
  ) where


import qualified Katip

import Codec.Serialise  (Serialise)
import Data.Map         (Map)
import Data.Traversable (forM)
import GHC.Generics     (Generic)

import qualified Data.Aeson    as JSON
import qualified Data.Aeson.TH as JSON


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
  { peekNTransactions :: Maybe Int -> m [tx]
    -- ^ Take up to N transactions from mempool. If Nothing is passed
    --   that all transactions will be returned. This operation does
    --   not alter mempool state
  , filterMempool     :: m ()
    -- ^ Remove transactions that are no longer valid from mempool
  , getMempoolCursor  :: m (MempoolCursor m tx)
    -- ^ Get cursor pointing to be
  , mempoolStats      :: m MempoolInfo
    -- ^ Number of elements in mempool
  }

hoistMempoolCursor :: (forall a. m a -> n a) -> MempoolCursor m tx -> MempoolCursor n tx
hoistMempoolCursor fun MempoolCursor{..} = MempoolCursor
  { pushTransaction = fun . pushTransaction
  , advanceCursor   = fun advanceCursor
  }

hoistMempool :: Functor n => (forall a. m a -> n a) -> Mempool m tx -> Mempool n tx
hoistMempool fun Mempool{..} = Mempool
  { peekNTransactions = fun . peekNTransactions
  , filterMempool     = fun filterMempool
  , getMempoolCursor  = hoistMempoolCursor fun <$> fun getMempoolCursor
  , mempoolStats      = fun mempoolStats
  }

nullMempool :: Monad m => Mempool m ()
nullMempool = Mempool
  { peekNTransactions = const (return [])
  , filterMempool     = return ()
  , mempoolStats      = return $ MempoolInfo 0 0 0 0
  , getMempoolCursor  = return MempoolCursor
      { pushTransaction = const (return ())
      , advanceCursor   = return Nothing
      }
  }


----------------------------------------------------------------
-- validate blockchain invariants
----------------------------------------------------------------

-- | Blockchain inconsistency types
data BlockchainInconsistency = MissingGenesisBlock
                             | MissingBlock Height
                             | MissingCommit Height
                             | MissingLocalCommit Height -- ^ Missing commit for block at maximum Height
                             | MissingValidatorSet Height
                             -- * genesis invariants
                             | GenesisHasLastCommit
                             | GenesisHasHeaderLastBlockID
                             | FirstBlockHasLastCommit
                             -- * Block invariants
                             | BlockLastCommitHasMisMatchVoteHeight Height
                             | BlockLastCommitHasMisMatchVoteBlockID Height
                             | BlockLastCommitHasUnknownValidator Height
                             | BlockHasMisMatchChainId Height -- ^ chainId  differ from genesis block chainId
                             | BlockHasMisMatchHeightInHeader Height
                             | BlockHasMisMatchHash Height
                             -- * Commit invariants
                             | CommitHasUnknownValidator Height
                             | CommitHasDeficitVotingPower Height
                               deriving (Eq, Show)


-- | genesis block is missing
genesisInvariant01 :: Height -> Maybe (Block alg a) -> [BlockchainInconsistency]
genesisInvariant01 (Height 0) Nothing = [MissingGenesisBlock]
genesisInvariant01 _ _                = []

-- | genesis block lastCommit, headerLastBlockID should be Nothing
genesisInvariant02 :: Height -> Maybe (Block alg a) -> [BlockchainInconsistency]
genesisInvariant02 (Height 0) (Just Block{..}) = case blockLastCommit of
                                                   Just _  -> [GenesisHasLastCommit]
                                                   Nothing -> []
genesisInvariant02 _ _ = []

-- | genesis block headerLastBlockID should be Nothing
genesisInvariant03 :: Height -> Maybe (Block alg a) -> [BlockchainInconsistency]
genesisInvariant03 (Height 0) (Just Block{..}) = case headerLastBlockID blockHeader of
                                                   Just _  -> [GenesisHasHeaderLastBlockID]
                                                   Nothing -> []
genesisInvariant03 _ _ = []


-- | block, commit, validator at height H is present
blockInvariant01 :: Height -> Maybe a -> [BlockchainInconsistency]
blockInvariant01 h Nothing = [MissingBlock h]
blockInvariant01 _ _       = []

-- |  Block at height H has H in its header
blockInvariant02 :: Height -> Maybe (Block alg a) -> [BlockchainInconsistency]
blockInvariant02 _ Nothing = []
blockInvariant02 h (Just Block{..}) = if headerHeight  blockHeader /= h
                                      then [BlockHasMisMatchHeightInHeader h]
                                      else []
-- | All blocks must have same headerChainID, i.e. headerChainID of  genesis block
blockInvariant03 _ _ Nothing = []
blockInvariant03 chainId h (Just Block{..}) = if headerChainID blockHeader /= chainId
                                              then [BlockHasMisMatchChainId h]
                                              else []

-- | first block (at height 1) blockLastCommit should be Nothing
blockInvariant04 :: Height -> Maybe (Block alg a) -> [BlockchainInconsistency]
blockInvariant04 (Height 1) (Just Block{..}) = case blockLastCommit of
                                                 Just _  -> [FirstBlockHasLastCommit]
                                                 Nothing -> []
blockInvariant04 _ _ = []

-- | block at height H+1 blockLastCommit all votes refer to block at henght H
blockInvariant05 :: Height -> Maybe (Block alg a) -> [BlockchainInconsistency]
blockInvariant05 _ Nothing = []
blockInvariant05 h@(Height n) (Just Block{..}) =
    case blockLastCommit of
      Just Commit{..} ->
          let voteHeights = map voteH commitPrecommits
              xs = filter (/= Height (n-1)) voteHeights
          in case xs of
               [] -> []
               _  -> [BlockLastCommitHasMisMatchVoteHeight h]

      Nothing -> []
    where
      voteH cmt = let (Vote v _ _ _) = signedValue cmt in v

-- | block's all voteBlockID equals to headerLastBlockID and equals to commitBlockID
blockInvariant06 :: Height -> Maybe (Block alg a) -> [BlockchainInconsistency]
blockInvariant06 _ Nothing = []
blockInvariant06 h (Just Block{..}) =
    case blockLastCommit of
      Just Commit{..} ->
          let voteHeights = map voteBID commitPrecommits
              xs = filter (maybe True (\b -> (b /= commitBlockID) ||
                                      (Just b /= headerLastBlockID blockHeader)) ) voteHeights
          in case xs of
               [] -> []
               _  -> [BlockLastCommitHasMisMatchVoteBlockID h]

      Nothing -> []
    where
      voteBID cmt = let (Vote _ _ _ bId) = signedValue cmt in bId





-- | all signes in Block -> blockLastCommit -> commitPrecommits should be known validators' signes
blockInvariant07 :: Maybe (ValidatorSet alg)
                  -> Height -> Maybe (Block alg a) -> [BlockchainInconsistency]
blockInvariant07 _ _ Nothing = []
blockInvariant07 Nothing  h@(Height n) _  = if n > 1 then [MissingValidatorSet (pred h)] else []
blockInvariant07 (Just vs) h (Just Block{..}) =
     case blockLastCommit of
       Nothing         -> []
       Just Commit{..} -> let signes = map signedAddr commitPrecommits
                              xs = sequence . map (validatorByAddr vs) $ signes
                          in case xs of
                               Nothing -> [BlockLastCommitHasUnknownValidator h]
                               Just _  -> []


-- | hash of block and hash from db should match
blockInvariant08 :: (Crypto alg, Serialise a) =>  Maybe (BlockID alg a) -> Height -> Maybe (Block alg a) -> [BlockchainInconsistency]
blockInvariant08 blockID h block = if (blockHash <$> block) == blockID
                            then []
                            else [BlockHasMisMatchHash h]


checkBlocks :: (Monad m, Crypto alg, Serialise a) =>
               BlockStorage rw m alg a -> m [BlockchainInconsistency]
checkBlocks storage = do
    maxH <- blockchainHeight storage
    Just genesis   <- retrieveBlock storage (Height 0)
    let heights = enumFromTo (toEnum 0) maxH
        genesisChainId = headerChainID $ blockHeader genesis
    xs <- forM heights (\h -> do
                            b   <- retrieveBlock storage h
                            bID <- retrieveBlockID storage h
                            vs  <- retrieveValidatorSet storage (pred h) -- we check blockLastCommit validators which refer to H-1 block, hence i get (H-1) validators set
                            return $ concatMap (\inv -> inv h b) [ genesisInvariant01
                                                                 , genesisInvariant02
                                                                 , genesisInvariant03
                                                                 , blockInvariant01
                                                                 , blockInvariant02
                                                                 , blockInvariant03 genesisChainId
                                                                 , blockInvariant04
                                                                 , blockInvariant05
                                                                 , blockInvariant06
                                                                 , blockInvariant07 vs
                                                                 , blockInvariant08 bID
                                                                 ]
                       )
    return $ concat xs



-- | local commit is present
localCommitInvariant01 :: Height -> Maybe (Commit alg a) -> [BlockchainInconsistency]
localCommitInvariant01 h Nothing = [MissingLocalCommit h]
localCommitInvariant01 _ _       = []

-- | commit is present
commitInvariant01 :: Height -> Maybe (Commit alg a) -> [BlockchainInconsistency]
commitInvariant01 h Nothing = [MissingCommit h]
commitInvariant01 _ _       = []


-- | check voting power is enought, i. e. contains at least 2/3 voting power
commitInvariant02 :: Maybe (ValidatorSet alg)
                  -> Height -> Maybe (Commit alg a) -> [BlockchainInconsistency]
commitInvariant02 _ _ Nothing  = []
commitInvariant02 Nothing  _ _ = []
commitInvariant02 (Just vs) h (Just Commit{..})
    | tot >= quorum = []
    | otherwise = [CommitHasDeficitVotingPower h]
  where
    power  = maybe 0 validatorVotingPower
           . validatorByAddr vs
    tot    = sum [ power a | a <- map signedAddr commitPrecommits]
    quorum = 2 * totalVotingPower vs `div` 3 + 1

-- | all signes in Commit-> commitPrecommits should be known validators' signes
commitInvariant03 :: Maybe (ValidatorSet alg)
                  -> Height -> Maybe (Commit alg a) -> [BlockchainInconsistency]
commitInvariant03 _ _ Nothing = []
commitInvariant03 Nothing  h@(Height n) _  = if n > 0 then [MissingValidatorSet h] else []
commitInvariant03 (Just vs) h (Just Commit{..}) =
    let signes = map signedAddr commitPrecommits
        xs = sequence . map (validatorByAddr vs) $ signes
    in case xs of
         Nothing -> [CommitHasUnknownValidator h]
         Just _  -> []


checkCommits :: Monad m =>
               BlockStorage rw m alg a1
            -> m [BlockchainInconsistency]
checkCommits storage = do
    maxH <- blockchainHeight storage
    let heights = enumFromTo (Height 1) (pred maxH)
    localCmt   <- retrieveLocalCommit storage maxH
    localCmtVs <- retrieveValidatorSet storage maxH
    xs <- forM heights (\h -> do
                          cmt <- retrieveCommit storage h
                          vs <- retrieveValidatorSet storage h
                          return $ concatMap (\inv -> inv h cmt) [ commitInvariant01
                                                                 , commitInvariant02 vs
                                                                 , commitInvariant03 vs]
                       )
    let checkLocalCmt = concatMap (\inv -> inv maxH localCmt) [ localCommitInvariant01
                                                              , commitInvariant02 localCmtVs]
    return $ concat (checkLocalCmt : xs)



-- | commit is present
validatorInvariant01 :: Height -> Maybe (ValidatorSet alg) -> [BlockchainInconsistency]
validatorInvariant01 h Nothing = [MissingValidatorSet h]
validatorInvariant01 _ _       = []

checkValidators :: Monad m =>
               BlockStorage rw m alg a1
            -> m [BlockchainInconsistency]
checkValidators storage = do
    maxH <- blockchainHeight storage
    let heights = enumFromTo (Height 1)  maxH
    xs <- forM heights (\h -> do
                          cmt <- retrieveValidatorSet storage h
                          return $ concatMap (\inv -> inv h cmt) [ validatorInvariant01 ]
                       )
    return $ concat xs
