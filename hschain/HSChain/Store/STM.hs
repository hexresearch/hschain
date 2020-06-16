{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- |
module HSChain.Store.STM (
    -- * Blockchain state storge
    newSTMBchStorage
  , snapshotState
  ) where

import Control.Concurrent.STM
import Control.Monad
import Control.Monad.Catch       (MonadThrow)
import Control.Monad.IO.Class

import HSChain.Store
import HSChain.Types.Blockchain
import HSChain.Types.Merkle.Types


----------------------------------------------------------------
-- Blockchain storage
----------------------------------------------------------------

-- | Create new storage for blockchain 
newSTMBchStorage
  :: (MonadIO m)
  => MerkleNode Identity (Alg a) (BlockchainState a) -> m (BChStore m a)
newSTMBchStorage st0 = do
  varSt <- liftIO $ newTVarIO (Nothing, st0)
  return BChStore
    { bchCurrentState = liftIO (readTVarIO varSt)
    --
    , bchStoreRetrieve = \h -> 
        liftIO (readTVarIO varSt) >>= \case
          (Just h', s) | h == h' -> return (Just s)
          _                      -> return Nothing
    --
    , bchStoreStore   = \h st ->
        liftIO $ atomically $ writeTVar varSt (Just h, st)
    }

-- | Store complete snapshot of state every N
snapshotState
  :: (MonadIO m, MonadDB a m, MonadThrow m, BlockData a)
  => Int           -- ^ Store snapshot every n height
  -> BChStore m a  -- ^ Store to modify
  -> m (BChStore m a)
snapshotState everyN BChStore{..} = do
  queryRO retrieveSavedState >>= mapM_ (\(h,s) -> bchStoreStore h s)
  return BChStore
    { bchStoreStore = \h@(Height n) st -> do
        when (fromIntegral n `mod` everyN == 0) $
          mustQueryRW (storeStateSnapshot h st)
        bchStoreStore h st
    , ..
    }
