{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE UndecidableInstances       #-}
-- |
-- Data types for storage of blockchain
module HSChain.Blockchain.Internal.Engine.Types (
    -- * Configuration of blockchain
    Configuration(..)
  , ConsensusCfg(..)
  , NetworkCfg(..)
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
    -- * Logging
  , LogBlockInfo(..)
  ) where

import Control.Applicative
import Control.Concurrent.STM
import Control.Monad.Trans.Except
import Data.Aeson
import Data.Coerce
import Data.Default.Class
import Data.Bits              (shiftL)
import Data.Monoid            (Any(..))
import Data.Maybe             (fromMaybe)
import Data.Typeable
import qualified Data.ByteString     as BS
import qualified Data.HashMap.Strict as HM
import Numeric.Natural
import qualified Katip
import GHC.Generics           (Generic)

import HSChain.Blockchain.Internal.Types
import HSChain.Crypto
import HSChain.Crypto.SHA (SHA512)
import HSChain.Mempool
import HSChain.Store
import HSChain.Types.Blockchain
import HSChain.Types.Validators
import HSChain.Config (Config(..), DropSmart(..), SnakeCase(..), WithDefault(..))

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
  { cfgConsensus :: !(ConsensusCfg app)
    -- ^ Configuration for consensus. JSON key is "consensus\"
  , cfgNetwork   :: !(NetworkCfg app)
    -- ^ Configuration for network. JSON key is \"network\"
  }
  deriving (Show, Generic)

instance ( Default (ConsensusCfg app)
         , Default (NetworkCfg   app)
         ) => Default (Configuration app) where
  def = Configuration def def

-- | Timeout and timeouts increments for consensus engine. On each
--    successive round timeout increased by increment. Note that they
--    should be same for all validating nodes in the network. Otherwise
--    network risks divergence. All timeouts are measured in ms.
data ConsensusCfg app = ConsensusCfg
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
data NetworkCfg app = NetworkCfg
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


deriving via WithDefault (SnakeCase (DropSmart (Config (Configuration app))))
  instance ( Default (ConsensusCfg app)
           , Default (NetworkCfg app)
           , Typeable app
           ) => FromJSON (Configuration app)

deriving via WithDefault (SnakeCase (Config (NetworkCfg app)))
  instance ( Default (NetworkCfg app)
           , Typeable app
           ) => FromJSON (NetworkCfg app)

deriving via WithDefault (SnakeCase (Config (ConsensusCfg app)))
  instance ( Default (ConsensusCfg app)
           , Typeable app
           ) => FromJSON (ConsensusCfg app)



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


-----------------------------------------------------------------
--- LogBlock
-----------------------------------------------------------------

-- | Wrapper for log data for logging purposes
data LogBlockInfo a = LogBlockInfo !Height !a !Int

instance BlockData a => Katip.ToObject (LogBlockInfo a) where
  toObject (LogBlockInfo (Height h) a ns)
    = HM.insert "H"     (toJSON h)
    $ HM.insert "nsign" (toJSON ns)
    $ logBlockData a

instance BlockData a => Katip.LogItem (LogBlockInfo a) where
  payloadKeys Katip.V0 _ = Katip.SomeKeys ["H"]
  payloadKeys _        _ = Katip.AllKeys
