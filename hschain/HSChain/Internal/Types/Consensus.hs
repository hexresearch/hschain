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
  , hoistStateView
  , UncommitedState(..)
  , MempoolHandle(..)
  , MempoolCursor(..)
    -- ** Logic of blockchain
  , NewBlock(..)
  , Genesis(..)
    -- * Evaluation context
  , BChEval(..)
  , ValidatedBlock
  ) where

import Codec.Serialise (Serialise)
import Control.DeepSeq
import Control.Monad.IO.Class
import Data.Functor.Identity
import Data.Aeson as JSON
import GHC.Generics (Generic)

import HSChain.Crypto
import HSChain.Mempool
import HSChain.Types.Merkle.Types
import HSChain.Types.Blockchain
import HSChain.Types.Validators


----------------------------------------------------------------
-- State management
----------------------------------------------------------------

-- | View on current state of blockchain.
data StateView m a = StateView
  { validatePropBlock  :: Block a
                       -> ValidatorSet (Alg a)
                       -> m (Either (BChError a) (UncommitedState m a))
  , generateCandidate  :: NewBlock a
                       -> m (a, UncommitedState m a)
  , stateHeight        :: m (Maybe Height)
  , mempoolHandle      :: MempoolHandle (Alg a) (TX a)
  }

hoistStateView :: (Functor m) => (forall x. m x -> n x) -> StateView m a -> StateView n a
hoistStateView fun StateView{..} = StateView
  { validatePropBlock = (fmap . fmap) (fun . (fmap . fmap) (hoistDict fun)) validatePropBlock
  , generateCandidate = \nb -> fun $ (fmap . fmap) (hoistDict fun) (generateCandidate nb)
  , stateHeight       = fun stateHeight
  , ..
  }

-- | Changes to state that are valid but not persisted to database yet. Could
data UncommitedState m a = UncommitedState
  { commitState   :: m ()
  , newValidators :: ValidatorSet (Alg a)
  }

instance HoistDict UncommitedState where
  hoistDict fun UncommitedState{..} = UncommitedState
    { commitState = fun commitState
    , ..
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


-- | Context for evaluation for blockchain. It's simply triple of
--   blockchain state, set of validators and something else.
data BChEval m a x = BChEval
  { bchValue        :: !x
  , validatorSet    :: !(MerkleNode Identity (Alg a) (ValidatorSet (Alg a)))
  , blockchainState :: !(UncommitedState m a)
  }
  deriving stock (Generic, Functor)

-- | Block which is already validated. It uses same type as
--   'BlockValidation' but validator set and state correspons to the
--   state _after_ evaluation.
type ValidatedBlock  m a = BChEval m a (Block a)
