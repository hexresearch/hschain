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
module HSChain.Blockchain.Internal.Engine.Types (
    -- * Configuration of blockchain
    Configuration(..)
  , ConsensusCfg(..)
  , NetworkCfg(..)
  , DefaultConfig(..)
    -- * Application state
  , AppLogic
  , AppStore(..)
  , AppCallbacks(..)
  , Validator(..)
  , PrivValidator(..)
    -- * Messages and channels
  , MessageRx(..)
  , unverifyMessageRx
  , Announcement(..)
  , AppChans(..)
    -- * Proposers
  , randomProposerSHA512
  ) where

import Control.Applicative
import Control.Concurrent.STM
import Control.Monad.Trans.Except
import Data.Aeson
import Data.Coerce
import Data.Bits              (shiftL)
import Data.Monoid            (Any(..))
import Data.Maybe             (fromMaybe)
import qualified Data.ByteString as BS
import Numeric.Natural
import GHC.Generics           (Generic)

import HSChain.Blockchain.Internal.Types
import HSChain.Crypto
import HSChain.Crypto.SHA (SHA512)
import HSChain.Store
import HSChain.Types.Blockchain
import HSChain.Types.Validators


----------------------------------------------------------------
-- Configuration
----------------------------------------------------------------

-- | Configuration of consensus engine. it contains timeouts for
--   consensus engine and parameters for network. Default parameters
--   are provided by 'DefaultConfig' type class. @app@ is phantom type
--   parameter which allows to have different defaults since different
--   blockchains may need different defaults.
--
--   Default definition workds like that: empty JSON dictionary will
--   deserialise to 'defCfg' and every field could be overriden by
--   config. For example
--
--  > { network = { gossipDelayVotes = 10 } }
--
--   Will set field @gossipDelayVotes@ to 10 ms while keeping all
--   other fields as they're defined in
data Configuration app = Configuration
  { cfgConsensus :: !ConsensusCfg -- ^ Configuration for consensus. JSON key is "consensus\"
  , cfgNetwork   :: !NetworkCfg   -- ^ Configuration for network. JSON key is \"network\"
  }
  deriving (Show,Generic)

-- | Timeout and timeouts increments for consensus engine. On each
--    successive round timeout increased by increment. Note that they
--    should be same for all validating nodes in the network. Otherwise
--    network risks divergence. All timeouts are measured in ms.
data ConsensusCfg = ConsensusCfg
  { timeoutNewHeight  :: !Int
    -- ^ Timeout for NEW HEIGHT phase
  , timeoutProposal   :: !(Int,Int)
    -- ^ Timeout and timeout increment for PROPOSE phase
  , timeoutPrevote    :: !(Int,Int)
    -- ^ Timeout and timeout increment for PREVOTE phase
  , timeoutPrecommit  :: !(Int,Int)
    -- ^ Timeout and timeout increment for PRECOMMIT phase
  , timeoutEmptyBlock :: !Int
    -- ^ Timeout between attempts to create block. Only used when
    --   empty block creation is disabled.
  , incomingQueueSize :: !Natural
    -- ^ Maximum queue size for incomiming messages. it's needed to
    --   avoid situation when node is flooded with messages faster
    --   that it's able to handle them. 10 is reasonable default.
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
  , pexConnectionDelay     :: !Int
  , pexAskPeersDelay       :: !Int
  , reconnectionRetries    :: !Int -- ^ Number of retries before abandoning reconnection attempts
  , reconnectionDelay      :: !Int -- ^ Initial delay between attempting to reconnect
  }
  deriving (Show,Generic)

-- | Default configuration for blockchain.
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
        pexD    <- field "pexConnectionDelay"     pexConnectionDelay      o
        pexAskD <- field "pexAskPeersDelay"       pexAskPeersDelay        o
        rR      <- field "reconnectionRetries"    reconnectionRetries     o
        rB      <- field "reconnectionDelay"      reconnectionDelay       o
        return NetworkCfg { gossipDelayVotes       = gV
                          , gossipDelayBlocks      = gB
                          , gossipDelayMempool     = gM
                          , pexMinConnections      = pexMinC
                          , pexMaxConnections      = pexMaxC
                          , pexMinKnownConnections = pexMinK
                          , pexMaxKnownConnections = pexMaxK
                          , pexConnectionDelay     = pexD
                          , pexAskPeersDelay       = pexAskD
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

type AppLogic m a = BChLogic (ExceptT (BChError a) m) a

data AppStore m a = AppStore
  { appMempool          :: Mempool m (Alg a) (TX a)
    -- ^ Application mempool
  , appBchState         :: BChStore m a
    -- ^ Store for the blockchain state
  }

-- | User callbacks which have monoidal strcture
data AppCallbacks m a = AppCallbacks
  { appCommitCallback   :: Block a -> m ()
    -- ^ Function which is called after each commit.
  , appCanCreateBlock   :: Height -> m (Maybe Bool)
    -- ^ Callback which is called to decide whether we ready to create
    --   new block or whether we should wait
  }

instance Monad m => Semigroup (AppCallbacks m a) where
  AppCallbacks f1 g1 <> AppCallbacks f2 g2 = AppCallbacks
    { appCommitCallback = liftA2 (*>) f1 f2
    , appCanCreateBlock = (liftA2 . liftA2) (coerce ((<>) @(Maybe Any))) g1 g2
    }
instance Monad m => Monoid (AppCallbacks m a) where
  mempty  = AppCallbacks (\_ -> pure ()) (\_ -> pure Nothing)

instance HoistDict AppCallbacks where
  hoistDict fun AppCallbacks{..} = AppCallbacks
    { appCommitCallback = fun . appCommitCallback
    , appCanCreateBlock = fun . appCanCreateBlock
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
data AppChans a = AppChans
  { appChanRx  :: TBQueue (MessageRx 'Unverified a)
    -- ^ Queue for receiving messages related to consensus protocol
    --   from peers.
  , appChanTx  :: TChan (MessageTx a)
    -- ^ TChan for broadcasting messages to the peers
  , appTMState :: TVar  (Maybe (Height, TMState a))
    -- ^ Current state of consensus. It includes current height, state
    --   machine status and known blocks which should be exposed in
    --   read-only manner for gossip with peers.
  }


-- | Select proposers using PRNG based on SHA512.
randomProposerSHA512 :: Crypto alg => ValidatorSet alg -> Height -> Round -> ValidatorIdx alg
randomProposerSHA512 valSet h r
  = fromMaybe (error "randomProposerSHA512: invalid index")
  $ indexByIntervalPoint valSet
  $ fromInteger
  -- NOTE: We just compute modulo total voting power. This gives
  --       _biased_ results. But since range of SHA512 is enormous:
  --       2^512 even for voting power on order 2^64 bias will be on
  --       order 10^{-134} that is negligible
  $ (`mod` fromIntegral (totalVotingPower valSet))
  -- Convert hash to integer. We interpret hash as LE integer
  $ BS.foldr' (\w i -> (i `shiftL` 8) + fromIntegral  w) 0 bs
  where
    Hash bs = hash (valSet, h, r) :: Hash SHA512
