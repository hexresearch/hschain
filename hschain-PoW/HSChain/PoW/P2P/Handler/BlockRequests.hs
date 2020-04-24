{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE RecordWildCards  #-}
-- |
module HSChain.PoW.P2P.Handler.BlockRequests
  ( BlockRegistry
  , newBlockRegistry
  , reserveBID
  , releaseBID
  ) where

import Control.Concurrent.STM
import Control.Monad
import Control.Monad.Catch
import Control.Monad.Trans.Cont
import Control.Monad.IO.Class
import Data.Set (Set, (\\))
import qualified Data.Set           as Set

import HSChain.Control.Util
import HSChain.Control.Class
import HSChain.Control.Channels
import HSChain.PoW.Types


-- | Registry of blocks that we want to fetch
data BlockRegistry b = BlockRegistry
  { required  :: TVar (Set (BlockID b))
  -- ^ Blocks that we need to fetch
  , available :: TVar (Set (BlockID b))
  -- ^ Blocks that could be requested by peers
  }

newBlockRegistry
  :: (MonadMask m, MonadFork m, Ord (BlockID b))
  => Src (Set (BlockID b))
  -> ContT r m (BlockRegistry b)
newBlockRegistry srcBids = do
  required  <- liftIO $ newTVarIO Set.empty
  available <- liftIO $ newTVarIO Set.empty
  cforkLinkedIO $ forever $ do
    bids <- awaitIO srcBids
    atomicallyIO $ do
      oldBids <- readTVar required
      writeTVar   required  bids
      modifyTVar' available $ Set.union $ bids \\ oldBids
  return BlockRegistry{..}

reserveBID :: BlockRegistry b -> STM (BlockID b)
reserveBID BlockRegistry{..} = do
  bids <- readTVar available
  case Set.minView bids of
    Nothing        -> retry
    Just (b,bids') -> do writeTVar available bids'
                         return b

releaseBID :: (Ord (BlockID b)) => BlockRegistry b -> BlockID b -> STM ()
releaseBID BlockRegistry{..} bid = do
  reqs <- readTVar required
  when (bid `Set.member` reqs) $
    modifyTVar' available $ Set.insert bid
