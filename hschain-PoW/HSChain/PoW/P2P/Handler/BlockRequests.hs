-- |
module HSChain.PoW.P2P.Handler.BlockRequests
  ( BlockRegistry
  , ReservedBlock(..)
  , BlockRegCmd(..)
  , newBlockRegistry
  , reserveBID
  -- , releaseBID
  ) where

import Control.Applicative
import Control.Concurrent.STM
import Control.Monad
import Control.Monad.Catch
import Control.Monad.Trans.Cont
import Control.Monad.IO.Class
import Control.Lens
import Data.Maybe
import Data.Set           (Set, (\\))
import Data.IntMap.Strict (IntMap)
import qualified Data.Set           as Set
import qualified Data.IntMap.Strict as IMap

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
  , explicitRequests :: TVar (IntMap (BlockID b, Block b -> STM (), Bool))
  -- ^ Explicit requests for blocks (used by light node for example)
  , requestCounter   :: TVar Int  
  }

-- | Information about block we've requested
data ReservedBlock b = ReservedBlock
  { reservedBID   :: !(BlockID b)              -- ^ Block ID we got to fetch
  , releaseOnFail :: !(STM ())                 -- ^ Release block on failure
  , returnBlock   :: Maybe (Block b -> STM ()) -- ^ Way to return block if needed
  }

-- | Commands for block registry
data BlockRegCmd b
  = SetRequired  !(Set (BlockID b))
    -- ^ Use new set of block required by consensus
  | RequestBlock !(BlockID b) (Block b -> STM ())
    -- ^ Request block with given BID and provide callback for
    --   returning it.

newBlockRegistry
  :: (MonadMask m, MonadFork m, Ord (BlockID b))
  => Src (BlockRegCmd b)
  -> ContT r m (BlockRegistry b)
newBlockRegistry srcBids = do
  required         <- liftIO $ newTVarIO Set.empty
  available        <- liftIO $ newTVarIO Set.empty
  explicitRequests <- liftIO $ newTVarIO mempty
  requestCounter   <- liftIO $ newTVarIO 0
  cforkLinkedIO $ forever $ do
    awaitIO srcBids >>= \case
      -- We get new set of BIDs that we need to fetch
      SetRequired bids -> atomicallyIO $ do
        oldBids <- readTVar required
        writeTVar   required  bids
        modifyTVar' available $ Set.union $ bids \\ oldBids
      -- We asked to fetch concrete block
      RequestBlock bid callback -> atomicallyIO $ do
        n <- readTVar requestCounter
        writeTVar  requestCounter $! n + 1
        modifyTVar' explicitRequests $ IMap.insert n (bid, callback, True)
  return BlockRegistry{..}

reserveBID :: (Ord (BlockID b)) => BlockRegistry b -> STM (ReservedBlock b)
reserveBID reg@BlockRegistry{..}
  = explicitReqs <|> consensusReqs
  where
    explicitReqs  = do
      -- Get requests we allowed to fetch
      reqs               <- readTVar explicitRequests
      (i,(bid,callback)) <- expectJust $ listToMaybe [ (i,(b,c)) | (i,(b,c,True)) <- IMap.toList reqs ]
      -- Mark BID as reserved
      modifyTVar' explicitRequests $ at i . _Just . _3 .~ False
      --
      return ReservedBlock
        { reservedBID   = bid
        , releaseOnFail = do modifyTVar' explicitRequests $ at i . _Just . _3 .~ True
        , returnBlock   = Just $ \b -> do modifyTVar' explicitRequests $ IMap.delete i
                                          callback b
        }

    --
    consensusReqs = do
      bids      <- readTVar available
      (b,bids') <- expectJust $ Set.minView bids
      writeTVar available $! bids'
      return ReservedBlock
        { reservedBID   = b
        , releaseOnFail = releaseBID reg b
        , returnBlock   = Nothing
        }

releaseBID :: (Ord (BlockID b)) => BlockRegistry b -> BlockID b -> STM ()
releaseBID BlockRegistry{..} bid = do
  reqs <- readTVar required
  when (bid `Set.member` reqs) $
    modifyTVar' available $ Set.insert bid

expectJust :: Maybe a -> STM a
expectJust = maybe retry pure 
