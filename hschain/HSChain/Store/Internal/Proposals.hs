{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
-- |
-- In-memory proposal storage
module HSChain.Store.Internal.Proposals
  ( Props
  , BlockValState(..)
    -- * Construction and update
  , emptyProps
  , acceptBlockID
  , addBlockToProps
  , setProposalValidation
    -- * Lookup
  , proposalByR
  , proposalByBID
  , blockFromBlockValidation
  ) where

import Data.Aeson             (Value(..))
import Data.Maybe             (fromMaybe)
import qualified Data.Vector     as V
import qualified Data.Map.Strict as Map
import           Data.Map.Strict   (Map)

import HSChain.Crypto
import HSChain.Types.Blockchain
import HSChain.Internal.Types.Consensus


-- | Status of block validation.
data BlockValState m a
  = UntestedBlock !(Block a)
  -- ^ We haven't validated block yet
  | UnknownBlock
  -- ^ We haven't even seen block yet
  | GoodBlock     !(Block a) !(StateView m a)
  -- ^ Block is good. We also cache state and new validator set
  | InvalidBlock  !Value
  -- ^ Block is invalid so there's no point in storing (and gossiping)
  --   it.

blockFromBlockValidation :: BlockValState m a -> Maybe (Block a)
blockFromBlockValidation = \case
  UntestedBlock b   -> Just b
  GoodBlock     b _ -> Just b
  UnknownBlock      -> Nothing
  InvalidBlock  _   -> Nothing


data Props m a = Props
  { propsBIDmap   :: !(Map (BlockID a) (BlockValState m a))
  , propsRoundMap :: !(Map Round (BlockID a))
  }

emptyProps :: Props m a
emptyProps = Props mempty mempty

proposalByBID :: Props m a -> BlockID a -> BlockValState m a
proposalByBID Props{..} bid
  = fromMaybe UnknownBlock
  $ bid `Map.lookup` propsBIDmap

proposalByR :: Props m a -> Round -> Maybe (BlockID a, BlockValState m a)
proposalByR ps@Props{..} r = do
  bid <- r   `Map.lookup` propsRoundMap
  return (bid, proposalByBID ps bid)

setProposalValidation :: BlockID a -> Either Value (StateView m a) -> Props m a -> Props m a
setProposalValidation bid mst Props{..} = Props
  { propsBIDmap = Map.adjust update bid propsBIDmap
  , ..
  }
  where
    update = case mst of
      Left err -> \case
        UntestedBlock _ -> InvalidBlock err
        InvalidBlock  e -> InvalidBlock $ Array $ V.fromList [e,err]
        GoodBlock{}     -> error $ "CANT HAPPEN: Marking good as bad" ++ show err
        UnknownBlock{}  -> error "CANT HAPPEN: Marking unseen as bad"
      Right uc -> \case
        UntestedBlock b -> GoodBlock b uc
        b@GoodBlock{}   -> b
        UnknownBlock    -> error "CANT HAPPEN: Marking unseen as good"
        InvalidBlock e  -> error $ "CANT HAPPEN: Marking bad as good" ++ show e


acceptBlockID :: Round -> BlockID a -> Props m a -> Props m a
acceptBlockID r bid Props{..} = Props
  { propsRoundMap = Map.insert r bid propsRoundMap
  -- NOTE: We can already have block with given BID in map. It could
  --       be reproposed in another round.
  , propsBIDmap   = Map.alter (\case
                                  Nothing -> Just UnknownBlock
                                  Just b  -> Just b
                              ) bid propsBIDmap
  }

addBlockToProps :: (Crypto (Alg a)) => Block a -> Props m a -> Props m a
addBlockToProps blk Props{..} = Props
  { propsBIDmap = Map.adjust (\case
                                 UnknownBlock -> UntestedBlock blk
                                 b            -> b
                             ) (blockHash blk) propsBIDmap
  , ..
  }
