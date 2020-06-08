{-# LANGUAGE FlexibleContexts #-}
-- |
module HSChain.Examples.Util where

import qualified Data.Map.Strict as Map

import HSChain.PoW.Types
import HSChain.Types.Merkle.Types
import HSChain.Examples.Simple

kvViewStep :: KVConfig cfg => Block (KV cfg) -> Map.Map Int String -> Maybe (Map.Map Int String)
kvViewStep b m
  | or [ k `Map.member` m | (k, _) <- txs ] = Nothing
  | otherwise                               = Just $ Map.fromList txs <> m
  where
    txs = merkleValue $ kvData $ blockData b

