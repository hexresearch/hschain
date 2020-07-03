{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DeriveFunctor        #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE DerivingStrategies   #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE UndecidableInstances #-}
-- |
module HSChain.Internal.Types.Consensus (
    -- * Blockchain logic
    StateView(..)
  , MempoolHandle(..)
  , MempoolCursor(..)
    -- ** Logic of blockchain
  , NewBlock(..)
  , Genesis(..)
  ) where

import HSChain.Mempool
import HSChain.Types.Blockchain
import HSChain.Types.Validators


----------------------------------------------------------------
-- State management
----------------------------------------------------------------

-- | View on current state of blockchain.
data StateView m a = StateView
  { stateHeight        :: Maybe Height
    -- ^ Header of block corresponging to state.
  , newValidators      :: ValidatorSet (Alg a)
    -- ^ Validator set after block evaluation
  , commitState        :: m (StateView m a)
    -- ^ Commit state to a persistent storage (if applicable).
  , validatePropBlock  :: Block a
                       -> ValidatorSet (Alg a)
                       -> m (Either (BChError a) (StateView m a))
    -- ^ Validate block against current state.
  , generateCandidate  :: NewBlock a
                       -> m (a, StateView m a)
    -- ^ Generate new proposal for blockchain
  , stateMempool       :: Mempool m (Alg a) (TX a)
    -- ^ Mempool
  }

----------------------------------------------------------------
-- Blockchain logic
----------------------------------------------------------------


data Genesis a = Genesis
  { genesisBlock  :: Block a
  , genesisValSet :: ValidatorSet (Alg a)
  }

-- | Parameters supplied by consensus engine for block generation
data NewBlock a = NewBlock
  { newBlockHeight   :: !Height
  , newBlockLastBID  :: !(BlockID a)
  , newBlockCommit   :: !(Maybe (Commit a))
  , newBlockEvidence :: ![ByzantineEvidence a]
  , newBlockValSet   :: !(ValidatorSet (Alg a))
  }
