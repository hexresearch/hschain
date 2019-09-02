{-# LANGUAGE BangPatterns    #-}
{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies    #-}
-- |
-- Encoding of application-specific logic for blockchain
module Thundermint.Blockchain.Interpretation (
    -- * Pure state
    BChLogic(..)
  , Interpreter(..)
  ) where

import Thundermint.Types.Blockchain

data BChLogic q alg a = BChLogic
  { processTx     :: !(TX a -> q ())
  , processBlock  :: !(Block alg a -> q ())
  , generateBlock :: !(Block alg a -> [TX a] -> q a)
  , initialState  :: !(InterpreterState a)
  }

newtype Interpreter q m alg a = Interpreter
  { interpretBCh :: forall x.
                    BlockchainState alg a
                 -> q x
                 -> m (Maybe (x, BlockchainState alg a))
  }


{-
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
-}
