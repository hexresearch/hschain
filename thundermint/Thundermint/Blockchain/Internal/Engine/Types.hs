{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE UndecidableInstances #-}
-- |
-- Data types for storage of blockchain
module Thundermint.Blockchain.Internal.Engine.Types (
    -- * Configuration of blockchain
    Configuration(..)
  , ConsensusCfg(..)
  , NetworkCfg(..)
  , DefaultConfig(..)
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

import Control.Applicative
import Control.Concurrent.STM
import Data.Aeson
import GHC.Generics           (Generic)

import Thundermint.Blockchain.Internal.Message
import Thundermint.Blockchain.Types
import Thundermint.Crypto
import Thundermint.Crypto.Containers
import Thundermint.Store


----------------------------------------------------------------
-- Configuration
----------------------------------------------------------------

data Configuration app = Configuration
  { cfgConsensus :: !ConsensusCfg -- ^ Configuration for consensus. JSON key is "consensus\"
  , cfgNetwork   :: !NetworkCfg   -- ^ Configuration for network. JSON key is \"network\"
  }
  deriving (Show,Generic)

-- | Timeouts are given in pairs where first element is default
--   timeout and second is increment. Unit of measurements for time is
--   ms.
data ConsensusCfg = ConsensusCfg
  { timeoutNewHeight :: !(Int,Int)
  , timeoutProposal  :: !(Int,Int)
  , timeoutPrevote   :: !(Int,Int)
  , timeoutPrecommit :: !(Int,Int)
  }
  deriving (Show,Generic)

-- | Configuration for network parameters. All delays are given in ms
data NetworkCfg = NetworkCfg
  { gossipDelayVotes       :: !Int -- ^ Delay between attempts to gossip votes and proposals
  , gossipDelayBlocks      :: !Int -- ^ Delay between attempts to gossip blocks
  , gossipDelayMempool     :: !Int -- ^ Delay between attempts to gossip transaction in mempool
  , pexMinConnections      :: !Int
  , pexMaxConnections      :: !Int
  , pexMinKnownConnections :: !Int
  , pexMaxKnownConnections :: !Int
  , reconnectionRetries    :: !Int -- ^ Number of retries before abandoning reconnection attempts
  , reconnectionDelay      :: !Int -- ^ Initial delay between attempting to reconnect
  }
  deriving (Show,Generic)


class DefaultConfig app where
  defCfg :: Configuration app

instance DefaultConfig app => FromJSON (Configuration app) where
  parseJSON = withObject "Configuration" $ \obj -> do
    cons <- optional (obj .: "consensus") >>= \case
      Nothing -> pure   (cfgConsensus cfg)
      Just o  -> parseC (cfgConsensus cfg) o
    net  <- optional (obj .: "network") >>= \case
      Nothing -> pure   (cfgNetwork cfg)
      Just o  -> parseN (cfgNetwork cfg) o
    return Configuration { cfgConsensus = cons
                         , cfgNetwork   = net
                         }
    where
      cfg = defCfg :: Configuration app
      --
      parseC ConsensusCfg{..} = withObject "Configuration.ConsesusCfg" $ \o -> do
        tH  <- field "timeoutNewHeight"  timeoutNewHeight o
        tP  <- field "timeoutProposal"   timeoutProposal  o
        tPV <- field "timeoutPrevote"    timeoutPrevote   o
        tPC <- field "timeoutPrecommit"  timeoutPrecommit o
        return ConsensusCfg{ timeoutNewHeight = tH
                           , timeoutProposal  = tP
                           , timeoutPrevote   = tPV
                           , timeoutPrecommit = tPC
                           }
      --
      parseN NetworkCfg{..} = withObject "Configuration.NetworkCfg" $ \o -> do
        gV      <- field "gossipDelayVotes"       gossipDelayVotes        o
        gB      <- field "gossipDelayBlocks"      gossipDelayBlocks       o
        gM      <- field "gossipDelayMempool"     gossipDelayMempool      o
        pexMinC <- field "pexMinConnections"      pexMinConnections       o
        pexMaxC <- field "pexMaxConnections"      pexMaxConnections       o
        pexMinK <- field "pexMinKnownConnections" pexMinKnownConnections  o
        pexMaxK <- field "pexMaxKnownConnections" pexMaxKnownConnections  o
        rR      <- field "reconnectionRetries"    reconnectionRetries     o
        rB      <- field "reconnectionDelay"      reconnectionDelay       o
        return NetworkCfg { gossipDelayVotes       = gV
                          , gossipDelayBlocks      = gB
                          , gossipDelayMempool     = gM
                          , pexMinConnections      = pexMinC
                          , pexMaxConnections      = pexMaxC
                          , pexMinKnownConnections = pexMinK
                          , pexMaxKnownConnections = pexMaxK
                          , reconnectionRetries    = rR
                          , reconnectionDelay      = rB
                          }
      --
      field name def o = optional (o .: name) >>= \case
        Nothing -> pure def
        Just v  -> parseJSON v




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
  { appBlockGenerator   :: Height
                        -> Time
                        -> Maybe (Commit alg a)
                        -> [ByzantineEvidence alg a]
                        -> m a
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
  { appBlockGenerator   = \h t c e -> fun $ appBlockGenerator h t c e
  , appValidationFun    = fun . appValidationFun
  , appCommitCallback   = fun . appCommitCallback
  , appCommitQuery      = hoistCommitCallback   fun appCommitQuery
  , ..
  }

-- | Our own validator
newtype PrivValidator alg = PrivValidator
  { validatorPrivKey  :: PrivKey alg
  }

instance Crypto alg => Show (PrivValidator alg) where
  show (PrivValidator k) = show k

instance Crypto alg => FromJSON (PrivValidator alg) where
  parseJSON = fmap PrivValidator . parseJSON
instance Crypto alg => ToJSON   (PrivValidator alg) where
  toJSON = toJSON . validatorPrivKey


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
