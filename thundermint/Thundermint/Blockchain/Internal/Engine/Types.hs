{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE UndecidableInstances #-}
-- |
-- Data types for storage of blockchain
module Thundermint.Blockchain.Internal.Engine.Types (
    -- * Configuration of blockchain
    Configuration(..)
    -- * Application state
  , AppState(..)
  , hoistAppState
  , Validator(..)
  , PrivValidator(..)
  , CommitCallback(..)
  , hoistCommitCallback
    -- * Messages and channels
  , MessageRx(..)
  , unverifyMessageRx
  , Announcement(..)
  , AppChans(..)
  , hoistAppChans
  ) where

import Control.Concurrent.STM
import qualified Data.Aeson as JSON
import GHC.Generics           (Generic)

import Thundermint.Blockchain.Internal.Message
import Thundermint.Blockchain.Types
import Thundermint.Crypto
import Thundermint.Crypto.Containers
import Thundermint.Store



----------------------------------------------------------------
-- Configuration
----------------------------------------------------------------

-- | Timeouts are given in pairs where first element is default
--   timeout and second is increment. Unit of measurements for time is
--   ms.
data Configuration = Configuration
  { timeoutNewHeight :: !(Int,Int)
  , timeoutProposal  :: !(Int,Int)
  , timeoutPrevote   :: !(Int,Int)
  , timeoutPrecommit :: !(Int,Int)
  , gossipDelayVotes :: !Int
  , gossipDelayBlocks :: !Int
  , gossipDelayMempool :: !Int
  , pexMinConnections :: !Int
  , pexMaxConnections :: !Int
  , pexMinKnownConnections :: !Int
  , pexMaxKnownConnections :: !Int
  }
  deriving (Show,Generic)
instance JSON.FromJSON Configuration
instance JSON.ToJSON   Configuration

----------------------------------------------------------------
--
----------------------------------------------------------------

-- | Callback which is called right after 
data CommitCallback m alg a
  = SimpleQuery !(Block alg a -> Query 'RW alg a (ValidatorSet alg))
  -- ^ Query for updating user's state and to find out new set of
  --   validators. It's evaluated in the same transaction as block
  --   commit and thus atomic.
  | MixedQuery  !(m (Block alg a -> Query 'RW alg a (ValidatorSet alg, m ())))
  -- ^ Query which allow to mixed database updates with other
  --   actions. If @Query@ succeeds returned action is executed immediately


-- | Full state of application.
data AppState m alg a = AppState
  { appBlockGenerator   :: Height -> m a
    -- ^ Generate fresh block for proposal. It's called each time we
    --   need to create new block for proposal
  , appValidator        :: Maybe (PrivValidator alg)
    -- ^ Private validator for node. It's @Nothing@ if node is not a validator
  , appValidationFun    :: Block alg a -> m Bool
    -- ^ Function for validation of proposed block data.
  , appCommitQuery      :: CommitCallback m alg a
    -- ^ Database query called after block commit in the same
    --   transaction
  , appCommitCallback   :: Block alg a -> m ()
    -- ^ Function which is called after each commit.
  }

hoistCommitCallback
  :: (Functor m)
  => (forall x. m x -> n x) -> CommitCallback m alg a -> CommitCallback n alg a
hoistCommitCallback _   (SimpleQuery f) = SimpleQuery f
hoistCommitCallback fun (MixedQuery  f) =
  MixedQuery $ fun $ (fmap . fmap . fmap . fmap) fun f

hoistAppState :: (Functor m) => (forall x. m x -> n x) -> AppState m alg a -> AppState n alg a
hoistAppState fun AppState{..} = AppState
  { appBlockGenerator   = fun . appBlockGenerator
  , appValidationFun    = fun . appValidationFun
  , appCommitCallback   = fun . appCommitCallback
  , appCommitQuery      = hoistCommitCallback fun appCommitQuery
  , ..
  }

-- | Our own validator
newtype PrivValidator alg = PrivValidator
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

hoistAppChans :: (forall x. m x -> n x) -> AppChans m alg a -> AppChans n alg a
hoistAppChans fun AppChans{..} = AppChans
  { appPropStorage   = hoistPropStorageRW fun appPropStorage
  , ..
  }
