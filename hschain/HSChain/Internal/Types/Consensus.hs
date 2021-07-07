{-# LANGUAGE DerivingStrategies      #-}
{-# LANGUAGE FlexibleContexts        #-}
{-# LANGUAGE FlexibleInstances       #-}
{-# LANGUAGE RankNTypes              #-}
{-# LANGUAGE TypeFamilies            #-}
{-# LANGUAGE UndecidableInstances    #-}
{-# LANGUAGE UndecidableSuperClasses #-}
-- |
module HSChain.Internal.Types.Consensus (
    -- * Blockchain logic
    StateView(..)
  , AlgOf
  , BlockOf
  , BlockIdOf
  , HeaderOf
  , RunIO(..)
    -- ** Mempool
  , MempoolHandle(..)
  , MempoolCursor(..)
    -- ** Logic of blockchain
  , NewBlock(..)
  , Genesis(..)
  ) where

import HSChain.Mempool
import HSChain.Types.Blockchain
import HSChain.Types.Validators
import GHC.Exts (Constraint)

----------------------------------------------------------------
-- State management
----------------------------------------------------------------

type AlgOf     view = Alg     (BlockType view)
type BlockIdOf view = BlockID (BlockType view)
type BlockOf   view = Block   (BlockType view)
type HeaderOf  view = Header  (BlockType view)

newtype RunIO view = RunIO { runIO :: forall a. MinMonad view a -> IO a }

class ( ViewConstraints view (MinMonad view)
      , BlockData (BlockType view)
      ) => StateView view where
  type BlockType       view :: *
  type MinMonad        view :: * -> *
  type ViewConstraints view :: (* -> *) -> Constraint
  -- | Header of block corresponging to state.
  stateHeight   :: view -> Maybe Height
  -- | Validator set after block evaluation
  newValidators :: view -> ValidatorSet (Alg (BlockType view))
  -- | Commit state to a persistent storage (if applicable).
  commitState   :: (ViewConstraints view m) => view -> m view
  -- | Validate block against current state.
  validatePropBlock  :: (ViewConstraints view m)
                     => view
                     -> BlockOf view
                     -> ValidatorSet (Alg (BlockType view))
                     -> m (Either (BChError (BlockType view)) view)  
  -- | Generate new proposal for blockchain
  generateCandidate  :: (ViewConstraints view m)
                     => view
                     -> NewBlock (BlockType view)
                     -> m (BlockType view, view)
  -- | Generate minimal runner for minimal monad
  makeRunIO :: (ViewConstraints view m) => m (RunIO view)


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
