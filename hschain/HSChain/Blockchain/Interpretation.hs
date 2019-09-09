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


data BChLogic q alg a = BChLogic
  { processTx     :: !(TX a -> q ())
  , processBlock  :: !(Block alg a -> q ())
  , generateBlock :: !(NewBlock alg a -> [TX a] -> q a)
  , initialState  :: !(InterpreterState a)
  }

newtype Interpreter q m alg a = Interpreter
  { interpretBCh :: forall x.
                    BlockchainState alg a
                 -> q x
                 -> m (Maybe (x, BlockchainState alg a))
  }
