{-# LANGUAGE DataKinds #-}
-- |
-- Data types for storage of blockchain 
module Thundermint.Blockchain.Types where

import Control.Concurrent.STM
import           Data.Map (Map)

import Thundermint.Crypto
import Thundermint.Consensus.Types
import Thundermint.Store


----------------------------------------------------------------
--
----------------------------------------------------------------

-- | Full state of application.
data AppState alg a = AppState
  { appStorage        :: BlockStorage 'RW IO alg a
    -- ^ Persistent storage for blockchain and related data
    --
    -- FIXME: Is IO good enough or do we need some other guarantees?
  , appBlockGenerator :: Maybe (Commit alg a)
                      -> IO (Block alg a)
    -- ^ Generate fresh block for proposa
  , appValidator      :: PrivValidator alg a
    -- ^ Private validator for node
  , appValidatorsSet  :: Map (Address alg) (Validator alg)
    -- ^ Set of all validators including our own
    --
    --   FIXME: at the moment it's assumed to be immutable but we will
    --          need to add support of changing set as result of
    --          commited block.
  , appLogger         :: String -> IO ()
  , appMaxHeight      :: Maybe Height
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
  deriving (Show)

-- | Message sent by main application
data MessageTx alg a
  = TxPreVote   (Signed 'Verified alg (Vote 'PreVote   alg a))
  | TxPreCommit (Signed 'Verified alg (Vote 'PreCommit alg a))
  | TxProposal  (Signed 'Verified alg (Proposal alg a))
  deriving (Show)

-- | Connection handed to process controlling communication with peer
data PeerChans alg a = PeerChans
  { peerChanTx :: STM (MessageTx alg a)
    -- ^ STM action for getting message to send to peer
  , peerChanRx :: MessageRx 'Unverified alg a -> STM ()
    -- ^ STM action for sending message to main application
  }

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
