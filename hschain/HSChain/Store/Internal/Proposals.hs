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

import Data.Maybe             (fromMaybe)
import qualified Data.Map.Strict as Map
import           Data.Map.Strict   (Map)

import HSChain.Crypto
import HSChain.Types.Blockchain


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

proposalByBID :: Props alg a -> BlockID alg a -> BlockValState alg a
proposalByBID Props{..} bid
  = fromMaybe UnknownBlock
  $ bid `Map.lookup` propsBIDmap

proposalByR :: Props alg a -> Round -> Maybe (BlockID alg a, BlockValState alg a)
proposalByR ps@Props{..} r = do
  bid <- r   `Map.lookup` propsRoundMap
  return (bid, proposalByBID ps bid)

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
