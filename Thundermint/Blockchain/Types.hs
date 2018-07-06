{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE DeriveGeneric   #-}
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
  , Announcement(..)
  , AppChans(..)
  , newAppChans
  ) where

import Codec.Serialise        (Serialise)
import Control.Concurrent.STM
import GHC.Generics           (Generic)

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

  , appBlockGenerator :: Height -> m a
    -- ^ Generate fresh block for proposal. It's called each time we
    --   need to create new block for proposal
  , appValidator      :: Maybe (PrivValidator alg)
    -- ^ Private validator for node. It's @Nothing@ if node is not a validator
  , appValidationFun  :: Height -> a -> m Bool
    -- ^ Function for validation of proposed block data.
  , appValidatorsSet  :: ValidatorSet alg
    -- ^ Set of all validators including our own
  , appMaxHeight      :: Maybe Height
  }

hoistAppState :: (forall x. m x -> n x) -> AppState m alg a -> AppState n alg a
hoistAppState fun AppState{..} = AppState
  { appStorage        = hoistBlockStorageRW fun appStorage
  , appPropStorage    = hoistPropStorageRW  fun appPropStorage
  , appBlockGenerator = fun . appBlockGenerator
  , appValidationFun  = \h a -> fun $ appValidationFun h a
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
  = RxPreVote   !(Signed ty alg (Vote 'PreVote   alg a))
  | RxPreCommit !(Signed ty alg (Vote 'PreCommit alg a))
  | RxProposal  !(Signed ty alg (Proposal alg a))
  | RxTimeout   !Timeout
  | RxBlock     !(Block alg a)
  deriving (Show)

-- | Messages which should be delivered to peers immediately. Those
--   are control messages in gossip protocol. Actual proposals, votes
--   and blocks are delivered pure via gossip.
data Announcement alg
  = AnnStep         !FullStep
  | AnnHasProposal  !Height !Round
  | AnnHasBlock     !Height !Round
  | AnnHasPreVote   !Height !Round !(ValidatorIdx alg)
  | AnnHasPreCommit !Height !Round !(ValidatorIdx alg)
  deriving (Show,Generic)
instance Serialise (Announcement alg)

-- | Application connection to outer world
data AppChans alg a = AppChans
  { appChanRx   :: TChan (MessageRx 'Unverified alg a)
    -- ^ TChan for receiving messages related to consensus protocol
    --   from peers (our own votes are also passed on same channel for
    --   uniformity)
  , appChanTx   :: TChan (Announcement alg)
    -- ^ TChan for broadcasting messages to the peers
  , appTMState  :: TVar  (Maybe (Height, TMState alg a))
    -- ^ Current state of consensus. It includes current height, state
    --   machine status and known blocks which should be exposed in
    --   read-only manner for gossip with peers.
  }

newAppChans :: IO (AppChans alg a)
newAppChans =
  AppChans <$> newTChanIO <*> newBroadcastTChanIO <*> newTVarIO Nothing
