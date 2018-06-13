{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE RecordWildCards #-}
-- |
-- Data types for storage of blockchain
module Thundermint.Blockchain.Types (
    -- * Application state
    AppState(..)
  , hoistAppState
  , Validator(..)
  , PrivValidator(..)
    -- * Messages and channels
  , MessageRx(..)
  , MessageTx(..)
  , AppChans(..)
  , newAppChans
  ) where

import Control.Concurrent.STM
import Thundermint.Crypto
import Thundermint.Crypto.Containers
import Thundermint.Consensus.Types
import Thundermint.Store



----------------------------------------------------------------
--
----------------------------------------------------------------

-- | Full state of application.
data AppState m alg a = AppState
  { appStorage        :: BlockStorage 'RW m alg a
    -- ^ Persistent storage for blockchain and related data
    --
    -- FIXME: Is IO good enough or do we need some other guarantees?
  , appPropStorage    :: ProposalStorage 'RW m alg a
    -- ^ Storage for proposed blocks

  , appBlockGenerator :: m a
    -- ^ Generate fresh block for proposal.
  , appValidator      :: PrivValidator alg
    -- ^ Private validator for node
  , appValidationFun  :: a -> m Bool
    -- ^ Function for validation of proposed block data
  , appValidatorsSet  :: ValidatorSet alg
    -- ^ Set of all validators including our own
  , appMaxHeight      :: Maybe Height
  }

hoistAppState :: (forall x. m x -> n x) -> AppState m alg a -> AppState n alg a
hoistAppState fun AppState{..} = AppState
  { appStorage        = hoistBlockStorageRW fun appStorage
  , appPropStorage    = hoistPropStorageRM  fun appPropStorage
  , appBlockGenerator = fun appBlockGenerator
  , appValidationFun  = fun . appValidationFun
  , ..
  }

-- | Our own validator
data PrivValidator alg = PrivValidator
  { validatorPrivKey  :: PrivKey alg
  }

----------------------------------------------------------------
-- Messages
----------------------------------------------------------------

-- | Message received by main application
data MessageRx ty alg a
  = RxPreVote   (Signed ty alg (Vote 'PreVote   alg a))
  | RxPreCommit (Signed ty alg (Vote 'PreCommit alg a))
  | RxProposal  (Signed ty alg (Proposal alg a))
  | RxTimeout   Timeout
  | RxBlock     (Block alg a)
  deriving (Show)

-- | Message sent by main application
data MessageTx alg a
  = TxPreVote   (Signed 'Verified alg (Vote 'PreVote   alg a))
  | TxPreCommit (Signed 'Verified alg (Vote 'PreCommit alg a))
  | TxProposal  (Signed 'Verified alg (Proposal alg a))
  | TxAnnHasVote Height Round VoteType (Address alg)
  deriving (Show)

-- | Application connection to outer world
data AppChans alg a = AppChans
  { appChanRx   :: TChan (MessageRx 'Unverified alg a)
    -- ^ TChan for sending messages to the main application
  , appChanTx   :: TChan (MessageTx alg a)
    -- ^ TChan for broadcasting messages to the peers
  , appTMState  :: TVar  (Maybe (Height, TMState alg a))
    -- ^ Current state of consensus. It includes current height, state
    --   machine status and known blocks which should be exposed in
    --   read-only manner for gossip with peers.
  }

newAppChans :: IO (AppChans alg a)
newAppChans =
  AppChans <$> newTChanIO <*> newBroadcastTChanIO <*> newTVarIO Nothing
