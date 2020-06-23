{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE MultiWayIf                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE ViewPatterns               #-}
-- |
module HSChain.Mempool (
   -- * Mempool
    MempoolHandle(..)
  , MempoolCursor(..)
  , MempoolInfo(..)
    -- * Implementation
  , MempoolCmd(..)
  , MempoolDict(..)
  , makeMempoolHandle
  , makeMempoolThread
  , newMempoolDict
    -- * Mempool data structures
  , MempoolState(..)
  , emptyMempoolState
  , mempoolAddTX
  , mempoolFilterTX
  , mempoolRemoveTX
  , mempoolSelfTest
  ) where

import Control.Applicative
import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Reader
import Control.Monad.Trans.Maybe

import Data.Foldable
import Data.Maybe                (fromMaybe,mapMaybe)
import Data.List                 (nub,sort)
import Data.Map.Strict           (Map)
import Data.IntMap.Strict        (IntMap)
import qualified Data.Aeson         as JSON
import qualified Data.Aeson.TH      as JSON
import qualified Data.Map.Strict    as Map
import qualified Data.IntMap.Strict as IMap
import qualified Data.Set           as Set
import qualified Katip
import Text.Printf
import GHC.Generics              (Generic)

import HSChain.Crypto
import HSChain.Types.Merkle.Types
import HSChain.Control.Util


----------------------------------------------------------------
-- Mempool
----------------------------------------------------------------

-- | Handle for working with mempool
data MempoolHandle alg tx = MempoolHandle
  { getMempoolCursor :: forall m. MonadIO m => m (MempoolCursor alg tx)
  , mempoolSize      :: forall m. MonadIO m => m Int
  }

-- | Cursor into mempool which is used for gossiping data
data MempoolCursor alg tx = MempoolCursor
  { pushTxSync      :: !(forall m. MonadIO m => tx -> m (Maybe (Hashed alg tx)))
    -- ^ Add transaction to the mempool. It's checked against current
    --   state of blockchain and if check fails it's immediately
    --   discarded. If transaction is accepted and not in mempool
    --   already its hash is computed and returned.
  , pushTxAsync     :: !(forall m. MonadIO m => tx -> m ())
    -- ^ Add transaction to the mempool wihtout waiting for validation result.
  , advanceCursor   :: !(forall m. MonadIO m => m (Maybe tx))
    -- ^ Take transaction from front and advance cursor. If cursor
    -- points at the end of queue nothing happens.
  }

data MempoolDict m alg tx = MempoolDict
  { varMempool  :: TVar (MempoolState alg tx)
  , varValidate :: TVar (tx -> m Bool)
  , varPending  :: TVar [Hashed alg tx]
  , chPushTx    :: TChan (MempoolCmd m alg tx)
  }

-- | Command to push new TX to mempool
data MempoolCmd m alg tx
  = CmdAddTx !(Maybe (MVar (Maybe (Hashed alg tx))))
             !tx
    -- ^ Add transaction to mempool.
  | CmdRemoveTx [Hashed alg tx]
    -- ^ Remove given transactions from mempool.
  | CmdStartFiltering (tx -> m Bool)
    -- ^ Start Remove now invalid TXs from mempool using given
    --   predicate.
  | CmdAskForState !(MVar (MempoolState alg tx))
    -- ^ Request state from mempool. While it could be obtained from
    --   'varMempool' variable using this method ensures that previous
    --   command completed.

newMempoolDict :: MonadIO m => m (MempoolDict m alg tx)
newMempoolDict = liftIO $ do
  varMempool   <- newTVarIO emptyMempoolState
  varValidate  <- newTVarIO $ const $ return False
  varPending   <- newTVarIO []
  chPushTx     <- newTChanIO
  return MempoolDict{..}

makeMempoolHandle :: MempoolDict m alg tx -> MempoolHandle alg tx
makeMempoolHandle MempoolDict{..} = MempoolHandle
  { getMempoolCursor = do
      varN <- liftIO $ newTVarIO 0
      return MempoolCursor
        { pushTxSync    = \tx -> liftIO $ do
            reply <- newEmptyMVar
            atomicallyIO $ writeTChan chPushTx $ CmdAddTx (Just reply) tx
            takeMVar reply
        --
        , pushTxAsync =
            atomicallyIO . writeTChan chPushTx . CmdAddTx Nothing
        --
        , advanceCursor = atomicallyIO $ do
            mem <- readTVar varMempool
            n   <- readTVar varN
            case n `IMap.lookupGT` mempFIFO mem of
              Nothing       -> return Nothing
              Just (n', tx) -> do writeTVar varN $! n'
                                  return $ Just $ merkleValue tx
        }
  , mempoolSize = liftIO $ IMap.size . mempFIFO <$> readTVarIO varMempool
  }

makeMempoolThread
  :: (CryptoHash alg, CryptoHashable tx, MonadIO m)
  => MempoolDict m alg tx
  -> m ()
makeMempoolThread dict@MempoolDict{..} = do
  let awaitPending = do
        txs <- readTVar varPending
        case splitAt 4 txs of
          ([],_)         -> retry
          (toCheck,rest) -> toCheck <$ writeTVar varPending rest
  --
  forever $ join $ atomicallyIO
    $  handleCommand      dict <$> readTChan chPushTx
   <|> handlePendingCheck dict <$> awaitPending


handleCommand
  :: (CryptoHash alg, CryptoHashable tx, MonadIO m)
  => MempoolDict m alg tx -> MempoolCmd m alg tx -> m ()
handleCommand MempoolDict{..} = \case
  CmdAddTx retVar tx -> do
    let txNode = merkled tx
    validation <- liftIO $ readTVarIO varValidate
    mmem <- mempoolAddTX validation txNode
        =<< liftIO (readTVarIO varMempool)
    case mmem of
      Nothing   -> return ()
      Just mem' -> atomicallyIO $ writeTVar   varMempool   mem'
    liftIO $ forM_ retVar $ \v -> tryPutMVar v (merkleHashed txNode <$ mmem)
  --
  CmdRemoveTx txs -> do
    atomicallyIO $ modifyTVar' varMempool $ mempoolRemoveTX txs
  --
  CmdStartFiltering validation -> atomicallyIO $ do
    writeTVar varValidate validation
    st <- readTVar varMempool
    writeTVar varPending $ fmap merkleHashed $ toList $ mempFIFO st
  --
  CmdAskForState reply ->
    liftIO $ void $ tryPutMVar reply =<< readTVarIO varMempool

handlePendingCheck
  :: (MonadIO m)
  => MempoolDict m alg tx -> [Hashed alg tx] -> m ()
handlePendingCheck MempoolDict{..} txHashes = do
  m   <- liftIO $ readTVarIO varMempool
  val <- liftIO $ readTVarIO varValidate
  badTxs <- filterM (fmap not . val . merkleValue)
          $ mapMaybe (\h -> snd <$> Map.lookup h (mempRevMap m)) txHashes
  atomicallyIO $ writeTVar varMempool $ mempoolRemoveTX (map merkleHashed badTxs) m


----------------------------------------------------------------
-- Mempool data structures
----------------------------------------------------------------

-- | State of mempool
data MempoolState alg tx = MempoolState
  { mempFIFO   :: !(IntMap (MerkleNode Identity alg tx))
    -- ^ Transactions arranged in FIFO order
  , mempRevMap :: !(Map (Hashed alg tx) (Int, MerkleNode Identity alg tx))
    -- ^ Reverse map of transactions
  , mempMaxN   :: !Int
    -- ^ Maximum key for FIFO
  }

instance Foldable (MempoolState alg) where
  foldMap f m = foldMap (f . merkleValue) (mempFIFO m)

emptyMempoolState :: MempoolState alg tx
emptyMempoolState = MempoolState
  { mempFIFO   = IMap.empty
  , mempRevMap = Map.empty
  , mempMaxN   = 0
  }

-- | Add transaction to mempool
mempoolAddTX
  :: (Monad m)
  => (tx -> m Bool)
  -> MerkleNode Identity alg tx
  -> MempoolState alg tx
  -> m (Maybe (MempoolState alg tx))
mempoolAddTX validation txNode@(MerkleNode txHash tx) MempoolState{..} = runMaybeT $ do
  -- Ignore TX that we already have
  guard $ txHash `Map.notMember` mempRevMap
  -- Validate TX
  lift (validation tx) >>= \case
    False -> empty
    True  -> do
      let n = mempMaxN + 1
      return $! MempoolState
        { mempFIFO   = IMap.insert n      txNode     mempFIFO
        , mempRevMap = Map.insert  txHash (n,txNode) mempRevMap
        , mempMaxN   = n
        }


-- | Remove all transaction from mepool that doesn't satisfy predicate
mempoolFilterTX
  :: (Monad m)
  => (tx -> m Bool)
  -> MempoolState alg tx
  -> m (MempoolState alg tx)
mempoolFilterTX validation MempoolState{..} = do
  invalidTx <- filterM (\(_,tx) -> not <$> validation (merkleValue tx))
             $ IMap.toList mempFIFO
  return MempoolState
    { mempFIFO   = foldl' (\m (i,_)  -> IMap.delete i m) mempFIFO invalidTx
    , mempRevMap = foldl' (\m (_,tx) -> Map.delete (merkleHashed tx) m) mempRevMap invalidTx
    , ..
    }

mempoolRemoveTX
  :: (Foldable f)
  => f (Hashed alg tx)
  -> MempoolState alg tx
  -> MempoolState alg tx
mempoolRemoveTX hashes MempoolState{..} = MempoolState
  { mempFIFO   = IMap.filter (\tx -> merkleHashed tx `Set.notMember` hashSet) mempFIFO
  , mempRevMap = Map.withoutKeys mempRevMap hashSet
  , ..
  }
  where
    hashSet = Set.fromList $ toList hashes

-- | Check mempool for internal consistency. Each returned string
--   is internal inconsistency
mempoolSelfTest
  :: (Show a, Ord a)
  => MempoolState alg a -> [String]
mempoolSelfTest MempoolState{..} = concat
  [ [ printf "Mismatch in rev.map. nFIFO=%i nRev=%i" nFIFO nRev
    | nFIFO /= nRev
    ]
  , [ "Duplicate transactions present"
    | let txs = toList mempFIFO
    , nub txs /= txs
    ]
  , [ printf "RevMap is not reverse of FIFO.\nFIFO: %s\nRevMap: %s" (show lFifo) (show lRev)
    | lFifo /= lRev
    ]
  , [ printf "Max N less than max FIFO key. maxN=%i maxKey=%s" mempMaxN (show maxKey)
    | mempMaxN < fromMaybe 0 maxKey
    ]
  ]
  where
    nFIFO  = IMap.size mempFIFO
    nRev   = Map.size mempRevMap
    lFifo  = IMap.toAscList mempFIFO
    lRev   = sort $ toList mempRevMap
    maxKey = fst <$> IMap.lookupMax mempFIFO


----------------------------------------------------------------
-- Logging helpers
----------------------------------------------------------------


-- | Statistics about mempool
data MempoolInfo = MempoolInfo
  { mempool'size      :: !Int
  -- ^ Number of transactions currently in mempool
  , mempool'added     :: !Int
  -- ^ Number of transactions added to mempool since program start
  , mempool'discarded :: !Int
  -- ^ Number of transaction discarded immediately since program start
  , mempool'filtered  :: !Int
  -- ^ Number of transaction removed during filtering
  }
  deriving (Show,Generic)
JSON.deriveJSON JSON.defaultOptions
  { JSON.fieldLabelModifier = drop 8 } ''MempoolInfo

instance Katip.ToObject MempoolInfo
instance Katip.LogItem  MempoolInfo where
  payloadKeys Katip.V0 _ = Katip.SomeKeys []
  payloadKeys _        _ = Katip.AllKeys
