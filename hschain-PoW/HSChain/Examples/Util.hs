{-# LANGUAGE FlexibleContexts #-}
-- |
module HSChain.Examples.Util where

import Control.Monad.IO.Class
import Data.IORef
import qualified Data.Map.Strict as Map

import HSChain.PoW.Types
import HSChain.PoW.Consensus
import HSChain.Types.Merkle.Types
import HSChain.Examples.Simple
import HSChain.PoW.Node (inMemoryView, inMemoryDB)

kvStep :: KVConfig cfg => Block (KV cfg) -> Map.Map Int String -> Maybe (Map.Map Int String)
kvStep b m
  | or [ k `Map.member` m | (k, _) <- txs ] = Nothing
  | otherwise                               = Just $ Map.fromList txs <> m
  where
    txs = merkleValue $ kvData $ blockData b

