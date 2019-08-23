{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeApplications     #-}
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
  , AppLogic(..)
  , hoistAppLogic
  , AppCallbacks(..)
  , hoistAppCallback
  , Validator(..)
  , PrivValidator(..)
  , AppByzantine(..)
  , hoistAppByzantine
    -- * Messages and channels
  , MessageRx(..)
  , unverifyMessageRx
  , Announcement(..)
  , AppChans(..)
  , hoistAppChans
  ) where

import Control.Applicative
import Control.Monad
import Control.Concurrent.STM
import Data.Aeson
import Data.Coerce
import Data.Monoid            (Any(..))
import Numeric.Natural
import GHC.Generics           (Generic)

import Thundermint.Blockchain.Internal.Types
import Thundermint.Crypto
import Thundermint.Store
import Thundermint.Types.Blockchain
import Thundermint.Types.Validators


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
  { timeoutNewHeight  :: !(Int,Int)
  , timeoutProposal   :: !(Int,Int)
  , timeoutPrevote    :: !(Int,Int)
  , timeoutPrecommit  :: !(Int,Int)
  , timeoutEmptyBlock :: !Int
  , incomingQueueSize :: !Natural
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
        tH  <- field "timeoutNewHeight"  timeoutNewHeight  o
        tP  <- field "timeoutProposal"   timeoutProposal   o
        tPV <- field "timeoutPrevote"    timeoutPrevote    o
        tPC <- field "timeoutPrecommit"  timeoutPrecommit  o
        tE  <- field "timeoutEmptyBlock" timeoutEmptyBlock o
        qs  <- field "incomingQueueSize" incomingQueueSize o
        return ConsensusCfg{ timeoutNewHeight  = tH
                           , timeoutProposal   = tP
                           , timeoutPrevote    = tPV
                           , timeoutPrecommit  = tPC
                           , timeoutEmptyBlock = tE
                           , incomingQueueSize = qs
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

-- | Collection of callbacks which implement actual logic of
--   blockchain. This is most generic form which doesn't expose any
--   underlying structure. It's expected that this structure will be
--   generated from more specialized functions
data AppLogic m alg a = AppLogic
  { appBlockGenerator   :: Block alg a
                        -> BlockchainState alg a
                        -> [TX a]
                        -> m (a, BlockchainState alg a)
    -- ^ Generate fresh block for proposal. It's called each time we
    --   need to create new block for proposal
  , appValidationFun    :: Block alg a
                        -> BlockchainState alg a
                        -> m (Maybe (BlockchainState alg a))
    -- ^ Function for validation of proposed block data. It returns
    --   change of validators for given block if it's valid and
    --   @Nothing@ if it's not.
  , appMempool          :: Mempool m alg (TX a)
    -- ^ Application mempool
  , appBchState         :: BChStore m a
  }

-- | User callbacks which have monoidal strcture
data AppCallbacks m alg a = AppCallbacks
  { appCommitCallback   :: Block alg a -> m ()
    -- ^ Function which is called after each commit.
  , appCanCreateBlock   :: Height -> m (Maybe Bool)
    -- ^ Callback which is called to decide whether we ready to create
    --   new block or whether we should wait
  , appByzantine        :: AppByzantine m alg a
    -- ^ Callbacks for insering byzantine behavior
  }

data AppByzantine m alg a = AppByzantine
  { byzantineBroadcastProposal :: Maybe (Proposal alg a        -> m (Maybe (Proposal alg a)))
  , byzantineCastPrevote       :: Maybe (Vote 'PreVote alg a   -> m (Maybe (Vote 'PreVote alg a)))
  , byzantineCastPrecommit     :: Maybe (Vote 'PreCommit alg a -> m (Maybe (Vote 'PreCommit alg a)))
  }


instance Monad m => Semigroup (AppCallbacks m alg a) where
  AppCallbacks f1 g1 b1 <> AppCallbacks f2 g2 b2 = AppCallbacks
    { appCommitCallback = liftA2 (*>) f1 f2
    , appCanCreateBlock = (liftA2 . liftA2) (coerce ((<>) @(Maybe Any))) g1 g2
    , appByzantine      = b1 <> b2
    }
instance Monad m => Monoid (AppCallbacks m alg a) where
  mempty  = AppCallbacks (\_ -> pure ()) (\_ -> pure Nothing) mempty


instance Monad m => Semigroup (AppByzantine m alg a) where
  bc1 <> bc2 = AppByzantine
    { byzantineBroadcastProposal = semiForField byzantineBroadcastProposal
    , byzantineCastPrevote       = semiForField byzantineCastPrevote
    , byzantineCastPrecommit     = semiForField byzantineCastPrecommit
    }
    where
      semiForField f = f bc1 `seqappl` f bc2


instance (Monad m) => Monoid (AppByzantine m alg a) where
    mempty = AppByzantine Nothing Nothing Nothing

seqappl :: (Monad m)
      => Maybe (a -> m (Maybe a))
      -> Maybe (a -> m (Maybe a))
      -> Maybe (a -> m (Maybe a))
seqappl Nothing Nothing     = Nothing
seqappl Nothing   x         = x
seqappl x         Nothing   = x
seqappl (Just b1) (Just b2) = Just $ b1 >=> maybe (return Nothing) b2

hoistAppLogic :: (Functor n) => (forall x. m x -> n x) -> AppLogic m alg a -> AppLogic n alg a
hoistAppLogic fun AppLogic{..} = AppLogic
  { appBlockGenerator   = (fmap . fmap . fmap) fun appBlockGenerator
  , appValidationFun    = (fmap . fmap)        fun appValidationFun
  , appMempool          = hoistMempool         fun appMempool
  , appBchState         = hoistBChStore        fun appBchState
  }

hoistAppCallback :: (forall x. m x -> n x) -> AppCallbacks m alg a -> AppCallbacks n alg a
hoistAppCallback fun AppCallbacks{..} = AppCallbacks
  { appCommitCallback = fun . appCommitCallback
  , appCanCreateBlock = fun . appCanCreateBlock
  , appByzantine      = hoistAppByzantine fun appByzantine
  }

hoistAppByzantine :: (forall x. m x -> n x) -> AppByzantine m alg a -> AppByzantine n alg a
hoistAppByzantine fun AppByzantine{..} = AppByzantine
  { byzantineBroadcastProposal = (fmap . fmap) fun byzantineBroadcastProposal
  , byzantineCastPrevote       = (fmap . fmap) fun byzantineCastPrevote
  , byzantineCastPrecommit     = (fmap . fmap) fun byzantineCastPrecommit
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
  , appChanTx      :: TChan (MessageTx alg a)
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
