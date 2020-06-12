{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE DerivingStrategies   #-}
{-# LANGUAGE DerivingVia          #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE UndecidableInstances #-}
-- |
module HSChain.Internal.Types.Config (
    -- * Configuration of blockchain
    Configuration(..)
  , ConsensusCfg(..)
  , NetworkCfg(..)
  ) where

import Data.Aeson
import Data.Default.Class
import Data.Typeable
import Numeric.Natural
import GHC.Generics (Generic)

import HSChain.Config (Config(..), DropSmart(..), SnakeCase(..), WithDefault(..))



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
