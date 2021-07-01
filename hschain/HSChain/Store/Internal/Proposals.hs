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
data BlockValState view
  = UntestedBlock !(BlockOf view)
  -- ^ We haven't validated block yet
  | UnknownBlock
  -- ^ We haven't even seen block yet
  | GoodBlock     !(BlockOf view) !view
  -- ^ Block is good. We also cache state and new validator set
  | InvalidBlock  !Value
  -- ^ Block is invalid so there's no point in storing (and gossiping)
  --   it.

blockFromBlockValidation :: BlockValState view -> Maybe (BlockOf view)
blockFromBlockValidation = \case
  UntestedBlock b   -> Just b
  GoodBlock     b _ -> Just b
  UnknownBlock      -> Nothing
  InvalidBlock  _   -> Nothing


data Props view = Props
  { propsBIDmap   :: !(Map (BlockIdOf view) (BlockValState view))
  , propsRoundMap :: !(Map Round (BlockIdOf view))
  }

emptyProps :: Props view
emptyProps = Props mempty mempty

proposalByBID :: Props view -> BlockIdOf view -> BlockValState view
proposalByBID Props{..} bid
  = fromMaybe UnknownBlock
  $ bid `Map.lookup` propsBIDmap

proposalByR :: Props view -> Round -> Maybe (BlockIdOf view, BlockValState view)
proposalByR ps@Props{..} r = do
  bid <- r   `Map.lookup` propsRoundMap
  return (bid, proposalByBID ps bid)

setProposalValidation :: BlockIdOf view -> Either Value view -> Props view -> Props view
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


acceptBlockID :: Round -> BlockIdOf view -> Props view -> Props view
acceptBlockID r bid Props{..} = Props
  { propsRoundMap = Map.insert r bid propsRoundMap
  -- NOTE: We can already have block with given BID in map. It could
  --       be reproposed in another round.
  , propsBIDmap   = Map.alter (\case
                                  Nothing -> Just UnknownBlock
                                  Just b  -> Just b
                              ) bid propsBIDmap
  }

addBlockToProps :: (Crypto (AlgOf view)) => BlockOf view -> Props view -> Props view
addBlockToProps blk Props{..} = Props
  { propsBIDmap = Map.adjust (\case
                                 UnknownBlock -> UntestedBlock blk
                                 b            -> b
                             ) (blockHash blk) propsBIDmap
  , ..
  }
