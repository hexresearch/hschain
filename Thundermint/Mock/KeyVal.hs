{-# LANGUAGE OverloadedStrings #-}
-- |
module Thundermint.Mock.KeyVal (
    genesisBlock
  , transitions
  ) where

import Control.Monad
import Data.Map             (Map)
import qualified Data.Map               as Map

import Thundermint.Consensus.Types
import Thundermint.Blockchain.Interpretation
import Thundermint.Crypto.Ed25519

----------------------------------------------------------------
--
----------------------------------------------------------------

genesisBlock :: Block Ed25519_SHA512 [(String,Int)]
genesisBlock = Block
  { blockHeader = Header
      { headerChainID     = "KV"
      , headerHeight      = Height 0
      , headerTime        = Time 0
      , headerLastBlockID = Nothing
      }
  , blockData       = []
  , blockLastCommit = Nothing
  }

transitions :: BlockFold (Map String Int) (String,Int) [(String,Int)]
transitions = BlockFold
  { processTx           = const process
  , processBlock        = \_ txs s0 -> foldM (flip process) s0 txs
  , transactionsToBlock = \_ ->
      let selectTx _ []     = []
          selectTx c (t:tx) = case process t c of
                                Nothing -> selectTx c  tx
                                Just c' -> t : selectTx c' tx
      in selectTx
  , initialState        = Map.empty
  }
  where
    process (k,v) m
      | k `Map.member` m = Nothing
      | otherwise        = Just $ Map.insert k v m
