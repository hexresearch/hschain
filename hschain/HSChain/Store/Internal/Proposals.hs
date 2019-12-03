{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
-- |
-- In-memory proposal storage
module HSChain.Store.Internal.Proposals where

import Control.Concurrent.STM
import Control.Monad.IO.Class
import Control.Monad
import Data.Maybe             (fromMaybe)
import qualified Data.Map.Strict as Map
import           Data.Map.Strict   (Map)

import HSChain.Crypto
import HSChain.Types.Blockchain
import HSChain.Store.Internal.Query


type family Writable (rw :: Access) a where
  Writable 'RO a = ()
  Writable 'RW a = a

-- | Status of block validation.
data BlockValState alg a
  = UntestedBlock !(Block alg a)
  -- ^ We haven't validated block yet
  | UnknownBlock
  -- ^ We haven't even seen block yet
  | GoodBlock     !(ValidatedBlock alg a)
  -- ^ Block is good. We also cache state and new validator set
  | InvalidBlock
  -- ^ Block is invalid so there's no point in storing (and gossiping)
  --   it.

blockFromBlockValidation :: BlockValState alg a -> Maybe (Block alg a)
blockFromBlockValidation = \case
  UntestedBlock b -> Just b
  GoodBlock     b -> Just $ bchValue b
  UnknownBlock    -> Nothing
  InvalidBlock    -> Nothing


data Props alg a = Props
  { propsBIDmap   :: !(Map (BlockID alg a) (BlockValState alg a))
  , propsRoundMap :: !(Map Round (BlockID alg a))
  }

emptyProps :: Props alg a
emptyProps = Props mempty mempty

proposalByBID :: Props alg a -> BlockID alg a -> Maybe (BlockValState alg a)
proposalByBID Props{..} bid = bid `Map.lookup` propsBIDmap


proposalByR :: Props alg a -> Round -> Maybe (BlockID alg a, BlockValState alg a)
proposalByR ps@Props{..} r = do
  bid <- r `Map.lookup` propsRoundMap
  st  <- proposalByBID ps bid
  return (bid,st)

setProposalValidation :: Props alg a -> BlockID alg a -> Maybe (EvaluationResult alg a) -> Props alg a
setProposalValidation Props{..} bid mst = Props
  { propsBIDmap = Map.adjust update bid propsBIDmap
  , ..
  }
  where
    update = case mst of
      Nothing -> \case
        UntestedBlock _ -> InvalidBlock
        InvalidBlock    -> InvalidBlock
        _               -> error "CANT HAPPEN"
      Just bst -> \case
        UntestedBlock b -> GoodBlock (b <$ bst)
        b@GoodBlock{}   -> b
        _               -> error "CANT HAPPEN"


acceptBlockID :: Props alg a -> Round -> BlockID alg a -> Props alg a
acceptBlockID Props{..} r bid = Props
  { propsRoundMap = Map.insert r bid propsRoundMap
  -- NOTE: We can already have block with given BID in map. It could
  --       be reproposed in another round.
  , propsBIDmap   = Map.alter (\case
                                  Nothing -> Just UnknownBlock
                                  Just b  -> Just b
                              ) bid propsBIDmap
  }

addBlockToProps :: (Crypto alg) => Props alg a -> Block alg a -> Props alg a
addBlockToProps Props{..} blk = Props
  { propsBIDmap = Map.adjust (\case
                                 UnknownBlock -> UntestedBlock blk
                                 b            -> b
                             ) (blockHash blk) propsBIDmap
  , ..
  }


----------------------------------------------------------------
-- OLD
----------------------------------------------------------------

-- | Storage for proposed blocks that are not commited yet.
data ProposalStorage rw m alg a = ProposalStorage
  { retrievePropByID   :: !(Height -> BlockID alg a -> m (BlockValState alg a))
    -- ^ Retrieve proposed block by its ID
  , retrievePropByR    :: !(Height -> Round -> m (Maybe (BlockID alg a, BlockValState alg a)))
    -- ^ Retrieve proposed block by round number.

  , setPropValidation  :: !(Writable rw ( BlockID alg a
                                       -> Maybe (EvaluationResult alg a)
                                       -> m ()))
    -- ^ Set whether block is valid or not.
  , resetPropStorage   :: !(Writable rw (Height -> m ()))
    -- ^ Reset proposal storage and set it to expect blocks ith given
    --   height.
  , allowBlockID       :: !(Writable rw (Round -> BlockID alg a -> m ()))
    -- ^ Mark block ID as one that we could accept
  , storePropBlock     :: !(Writable rw (Block alg a -> m ()))
    -- ^ Store block proposed at given height. If height is different
    --   from height we are at block is ignored.
  }

makeReadOnlyPS :: ProposalStorage rw m alg a -> ProposalStorage 'RO m alg a
makeReadOnlyPS ProposalStorage{..} =
  ProposalStorage { resetPropStorage  = ()
                  , storePropBlock    = ()
                  , allowBlockID      = ()
                  , setPropValidation = ()
                  , ..
                  }

instance HoistDict (ProposalStorage 'RW) where
  hoistDict fun ProposalStorage{..} = ProposalStorage
    { retrievePropByID   = \h x -> fun (retrievePropByID h x)
    , retrievePropByR    = \h x -> fun (retrievePropByR  h x)
    , setPropValidation  = \b x -> fun (setPropValidation b x)
    , resetPropStorage   = fun . resetPropStorage
    , storePropBlock     = fun . storePropBlock
    , allowBlockID       = \r bid -> fun (allowBlockID r bid)
    }

instance HoistDict (ProposalStorage 'RO) where
  hoistDict fun ProposalStorage{..} = ProposalStorage
    { retrievePropByID   = \h x -> fun (retrievePropByID h x)
    , retrievePropByR    = \h x -> fun (retrievePropByR  h x)
    , ..
    }


newSTMPropStorage
  :: (Crypto alg, MonadIO m)
  => m (ProposalStorage 'RW m alg a)
newSTMPropStorage = fmap (hoistDict liftIO) $ liftIO $ do
  varH    <- newTVarIO (Height 0) -- Current height
  varPBlk <- newTVarIO Map.empty  -- Proposed blocks
  varRMap <- newTVarIO Map.empty  -- Map of rounds to block IDs
  return ProposalStorage
    { resetPropStorage = \h -> atomically $ do
        writeTVar varH    h
        writeTVar varPBlk Map.empty
        writeTVar varRMap Map.empty
    --
    , setPropValidation = \bid mSt -> do
        let action = atomically . modifyTVar' varPBlk . flip Map.adjust bid
        case mSt of
          Nothing -> action $ \case
            UntestedBlock _ -> InvalidBlock
            InvalidBlock    -> InvalidBlock
            _               -> error "CANT HAPPEN"
          Just st -> action $ \case
            UntestedBlock b -> GoodBlock st { bchValue = b }
            b@GoodBlock{}   -> b
            _               -> error "CANT HAPPEN"
    --
    , storePropBlock = \blk -> atomically $ do
        h <- readTVar varH
        when (blockHeight blk == h) $
          modifyTVar' varPBlk $ flip Map.adjust (blockHash blk) $ \case
            UnknownBlock -> UntestedBlock blk
            b            -> b
    --
    , allowBlockID = \r bid -> atomically $ do
        modifyTVar' varRMap $ Map.insert r   bid
        modifyTVar' varPBlk $ flip Map.alter bid $ \case
          Nothing -> Just UnknownBlock
          Just b  -> Just b
    --
    , retrievePropByID = \h0 bid -> atomically $ do
        h <- readTVar varH
        if h == h0 then fromMaybe UnknownBlock . Map.lookup bid <$> readTVar varPBlk
                   else return UnknownBlock
    --
    , retrievePropByR = \h r -> atomically $ do
        h0 <- readTVar varH
        if h /= h0
          then return Nothing
          else do rMap <- readTVar varRMap
                  pBlk <- readTVar varPBlk
                  return $ do bid <- r   `Map.lookup` rMap
                              blk <- bid `Map.lookup` pBlk
                              return (bid, blk)
    }
