{-# LANGUAGE BangPatterns    #-}
{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies    #-}
-- |
-- Encoding of application-specific logic for blockchain
module Thundermint.Blockchain.Interpretation (
    -- * Pure state
    BlockFold(..)
  ) where

import Control.Concurrent.MVar
import Control.Monad (when,(<=<))
import Control.Monad.Catch
import Control.Monad.IO.Class

import Thundermint.Crypto
import Thundermint.Control
import Thundermint.Exceptions
import Thundermint.Store
import Thundermint.Types.Blockchain



-- | Data structure which encapsulate interpretation of
--   blockchain. Type parameters have following meaning:
--
--   * @s@  is type of state of blockchain
--   * @tx@ is type of single transaction
--   * @a@  is type of block
--
--   Each block in blockchain change current state of blockchain (in
--   case of coins it's amount of coins available for each
--   wallet). Transition functions double as validation functions
--   e.g. if transaction or block is not valid it will return
--   @Nothing@.
data BlockFold alg a = BlockFold
  { processTx    :: !(CheckSignature -> Height -> TX a -> BlockchainState a -> Maybe (BlockchainState a))
    -- ^ Try to process single transaction. Nothing indicates that
    --   transaction is invalid. This function will called very
    --   frequently so it need not to perform every check but should
    --   rule out invalid blocks.
    --
    --   FIXME: figure out exact semantics for Height parameter
  , processBlock :: !(CheckSignature -> Block alg a -> BlockchainState a -> Maybe (BlockchainState a))
    -- ^ Try to process whole block. Here application should perform
    --   complete validation of block
  , transactionsToBlock :: !(Height -> BlockchainState a -> [TX a] -> (BlockchainState a, a))
    -- ^ Create block at given height from list of transactions. Not
    --   input could contain invalid transaction and they must be
    --   filtered out so that block is valid.
  , initialState :: !(BlockchainState a)
    -- ^ State of blockchain BEFORE genesis block.
  }
