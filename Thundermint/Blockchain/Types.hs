{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE RecordWildCards #-}
-- |
-- Data types for storage of blockchain
module Thundermint.Blockchain.Types where

import Control.Concurrent.STM
import           Data.ByteString (ByteString)
import           Data.Map        (Map)

import Thundermint.Crypto
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
  , appChainID        :: ByteString
    -- ^ Chain ID of application. It will be included into every
    --   block.
  , appBlockGenerator :: m a
    -- ^ Generate fresh block for proposal.
  , appValidator      :: PrivValidator alg a
    -- ^ Private validator for node
  , appValidatorsSet  :: Map (Address alg) (Validator alg)
    -- ^ Set of all validators including our own
    --
    --   FIXME: at the moment it's assumed to be immutable but we will
    --          need to add support of changing set as result of
    --          commited block.
  , appMaxHeight      :: Maybe Height
  }

hoistAppState :: (forall x. m x -> n x) -> AppState m alg a -> AppState n alg a
hoistAppState fun AppState{..} = AppState
  { appStorage        = hoistBlockStorageRW fun appStorage
  , appBlockGenerator = fun appBlockGenerator
  , ..
  }

-- | Information about remote validator
data Validator alg = Validator
  { validatorPubKey      :: PublicKey alg
  , validatorVotingPower :: Integer
  }

-- | Our own validator
data PrivValidator alg a = PrivValidator
  { validatorPrivKey  :: PrivKey alg
  , validateBlockData :: a -> Bool
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
