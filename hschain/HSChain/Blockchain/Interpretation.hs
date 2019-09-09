{-# LANGUAGE BangPatterns    #-}
{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies    #-}
-- |
-- Encoding of application-specific logic for blockchain
module HSChain.Blockchain.Interpretation (
    -- * Pure state
    BChLogic(..)
  , Interpreter(..)
  ) where

import HSChain.Types.Blockchain
import HSChain.Blockchain.Internal.Engine.Types


-- | Description of interpretation of blockchain state. Evaluation of
--   blocks and transactions are done in the monad @q@ which is
--   assumed to provide access to current state of blockchain
data BChLogic q alg a = BChLogic
  { processTx     :: !(TX a -> q ())
    -- ^ Process single transactions. Used when 
  , processBlock  :: !(Block alg a -> q ())
    -- ^ Process and validate complete block.
  , generateBlock :: !(NewBlock alg a -> [TX a] -> q a)
    -- ^ Generate block from list of transactions.
  , initialState  :: !(InterpreterState a)
    -- ^ Initial state of blockchain
  }


-- | Interpreter for mond in which evaluation of blockchain is
--   performed
newtype Interpreter q m alg a = Interpreter
  { interpretBCh :: forall x.
                    BlockchainState alg a
                 -> q x
                 -> m (Maybe (x, BlockchainState alg a))
  }
