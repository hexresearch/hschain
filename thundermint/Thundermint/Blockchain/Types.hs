{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE UndecidableInstances #-}
-- |
-- Data types for storage of blockchain
module Thundermint.Blockchain.Types (
    -- * Configuration of blockchain
    Configuration(..)
    -- * Application state
  , AppState(..)
  , hoistAppState
  , Validator(..)
  , PrivValidator(..)
    -- * Messages and channels
  , MessageRx(..)
  , unverifyMessageRx
  , Announcement(..)
  , AppChans(..)
  ) where

import Control.Concurrent.STM
import qualified Data.Aeson as JSON
import GHC.Generics           (Generic)

import Thundermint.Blockchain.Message
import Thundermint.Crypto
import Thundermint.Crypto.Containers
import Thundermint.Consensus.Types
import Thundermint.Store



----------------------------------------------------------------
-- Configuration
----------------------------------------------------------------

-- | Timeouts are given in pairs where first element is default
--   timeout and second is increment. Unit of measurements for time is
--   ms.
data Configuration = Configuration
  { timeoutNewHeight :: (Int,Int)
  , timeoutProposal  :: (Int,Int)
  , timeoutPrevote   :: (Int,Int)
  , timeoutPrecommit :: (Int,Int)
  , gossipDelayVotes :: Int
  , gossipDelayBlocks :: Int
  , gossipDelayMempool :: Int
  , pexMinConnections :: Int
  , pexMaxConnections :: Int
  , pexMinKnownConnections :: Int
  , pexMaxKnownConnections :: Int
  }
  deriving (Show,Generic)
instance JSON.FromJSON Configuration
instance JSON.ToJSON   Configuration

----------------------------------------------------------------
--
----------------------------------------------------------------

-- | Full state of application.
data AppState m alg a = AppState
  { appStorage          :: BlockStorage 'RW m alg a
    -- ^ Persistent storage for blockchain and related data
  , appBlockGenerator   :: Height -> m a
    -- ^ Generate fresh block for proposal. It's called each time we
    --   need to create new block for proposal
  , appValidator        :: Maybe (PrivValidator alg)
    -- ^ Private validator for node. It's @Nothing@ if node is not a validator
  , appValidationFun    :: Height -> a -> m Bool
    -- ^ Function for validation of proposed block data.
  , appNextValidatorSet :: Height -> a -> m (ValidatorSet alg)
    -- ^ Obtain validator set for next block.
  , appCommitCallback   :: Height -> m ()
    -- ^ Function which is called after each commit.
  }

hoistAppState :: (forall x. m x -> n x) -> AppState m alg a -> AppState n alg a
hoistAppState fun AppState{..} = AppState
  { appStorage          = hoistBlockStorageRW fun appStorage
  , appBlockGenerator   = fun . appBlockGenerator
  , appValidationFun    = \h a -> fun $ appValidationFun h a
  , appCommitCallback   = fun . appCommitCallback
  , appNextValidatorSet = \h a -> fun $ appNextValidatorSet h a
  , ..
  }

-- | Our own validator
data PrivValidator alg = PrivValidator
  { validatorPrivKey  :: PrivKey alg
  }

instance Crypto alg => Show (PrivValidator alg) where
  show (PrivValidator k) = show k

instance Crypto alg => JSON.FromJSON (PrivValidator alg) where
  parseJSON = fmap PrivValidator . JSON.parseJSON
instance Crypto alg => JSON.ToJSON   (PrivValidator alg) where
  toJSON = JSON.toJSON . validatorPrivKey


-- | Application connection to outer world
data AppChans m alg a = AppChans
  { appChanRx         :: TBQueue (MessageRx 'Unverified alg a)
    -- ^ Queue for receiving messages related to consensus protocol
    --   from peers.
  , appChanRxInternal :: TQueue (MessageRx 'Unverified alg a)
    -- ^ Queue for sending messages by consensus engine to
    --   itself. Note that it's unbounded since we must not block when
    --   writing there.
  , appChanTx      :: TChan (Announcement alg)
    -- ^ TChan for broadcasting messages to the peers
  , appTMState     :: TVar  (Maybe (Height, TMState alg a))
    -- ^ Current state of consensus. It includes current height, state
    --   machine status and known blocks which should be exposed in
    --   read-only manner for gossip with peers.
  , appPropStorage :: ProposalStorage 'RW m alg a
    -- ^ Storage for proposed blocks
  }
