{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE DerivingVia          #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}
module HSChain.Mock.Types (
    -- * Helper data types
    Example
  , Topology(..)
    -- ** Dealing with exceptions
  , Abort(..)
  , catchAbort
  , callbackAbortAtH
    -- ** Node and network specification
  , SingleNodeConfig(..)
  , MockClusterConfig(..)
  , NodeSpec(..)
  , RealNetSpec(..)
  , HSChainCfg
    -- * Helpers
  , makeGenesis
  ) where

import Control.Exception   (Exception)
import Control.Monad
import Control.Monad.Catch (MonadCatch(..),MonadThrow(..))
import Data.Default.Class
import Data.Typeable
import Data.Word
import GHC.Generics (Generic)

import qualified Data.Aeson as JSON

import HSChain.Blockchain.Internal.Engine.Types
import HSChain.Config
import HSChain.Internal.Types.Config
import HSChain.Crypto
import HSChain.Logger         (ScribeSpec)
import HSChain.Types
import HSChain.Types.Merkle.Types
import HSChain.Network.Types


----------------------------------------------------------------
-- Default configurations
----------------------------------------------------------------

-- | Default configuration for example blockchains. Defaults are set
--   in order to generate blocks fast (~1b\/s) so it likely not good
--   choice for production.
data Example

instance Default (ConsensusCfg Example) where
  def = ConsensusCfg
    { timeoutNewHeight   = 500
    , timeoutProposal    = (500, 500)
    , timeoutPrevote     = (500, 500)
    , timeoutPrecommit   = (500, 500)
    , timeoutEmptyBlock  = 100
    , incomingQueueSize  = 7
    }

instance Default (NetworkCfg Example) where
  def = NetworkCfg
    { gossipDelayVotes       = 25
    , gossipDelayBlocks      = 25
    , gossipDelayMempool     = 25
    , pexMinConnections      = 3
    , pexMaxConnections      = 20
    , pexMinKnownConnections = 3
    , pexMaxKnownConnections = 20
    , pexConnectionDelay     = 3000
    , pexAskPeersDelay       = 10000
    , reconnectionRetries    = 12
    , reconnectionDelay      = 100
    , mempoolLogInterval     = 1000
    }


----------------------------------------------------------------
-- Helpers
----------------------------------------------------------------

-- | Genesis block has many field with predetermined content so this
--   is convenience function to create genesis block.
makeGenesis
  :: (Crypto (Alg a), CryptoHashable a)
  => a                          -- ^ Block data
  -> ValidatorSet (Alg a)       -- ^ Set of validators for H=0
  -> ValidatorSet (Alg a)       -- ^ Set of validators for H=1
  -> Block a
makeGenesis dat valSet0 valSet1 = Block
  { blockHeight        = Height 0
  , blockPrevBlockID   = Nothing
  , blockValidators    = hashed valSet0
  , blockNewValidators = hashed valSet1
  , blockData          = merkled dat
  , blockPrevCommit    = Nothing
  , blockEvidence      = merkled []
  }


----------------------------------------------------------------
-- Generating node specification
----------------------------------------------------------------

-- | Exception for aborting execution of blockchain
data Abort = Abort
  deriving Show
instance Exception Abort

-- | Callback which aborts execution when blockchain exceed given
--   height. It's done by throwing 'Abort'.
callbackAbortAtH :: MonadThrow m => Height -> AppCallbacks m a
callbackAbortAtH hMax = mempty
  { appCommitCallback = \b ->
      when (blockHeight b > hMax) $ throwM Abort
  }

catchAbort :: MonadCatch m => m () -> m ()
catchAbort act = catch act (\Abort -> return ())

data Topology = All2All
              | Ring
              deriving (Generic,Show)
instance JSON.ToJSON   Topology
instance JSON.FromJSON Topology



----------------------------------------------------------------
-- Specifications
----------------------------------------------------------------

-- | Configuration for a single node 
data SingleNodeConfig b a = SingleNodeConfig
  { snodeNet        :: RealNetSpec           -- ^ Network
  , snodeCfg        :: Configuration Example -- ^ Delays configuration
  , snodeSpec       :: NodeSpec b            -- ^ Consensus parameters
  , snodeValidators :: [PublicKey (Alg b)]   -- ^ Initial validator set
  , snodeBchData    :: a                     -- ^ Blockchain-specific data
  }
  deriving (Generic)
deriving via HSChainCfg (SingleNodeConfig b a)
    instance (JSON.FromJSON a, Typeable a, Typeable b, Typeable (Alg b), Crypto (Alg b)
             ) => JSON.FromJSON (SingleNodeConfig b a)


-- | Specification for mock cluster which is run in single process
data MockClusterConfig b a = MockClusterConfig
  { clusterTopology :: Topology              -- ^ Topology of initial connections
  , clusterCfg      :: Configuration Example -- ^ Delays configuration
  , clusterNodes    :: [NodeSpec b]          -- ^ List of nodes in the network
  , clusterBChData  :: a                     -- ^ Blockchain-specific data
  }
  deriving (Generic)
deriving via HSChainCfg (MockClusterConfig b a)
    instance (JSON.FromJSON a, Typeable a, Typeable b, Typeable (Alg b), Crypto (Alg b)
             ) => JSON.FromJSON (MockClusterConfig b a)


-- | Basic information which is required for any node and independent
--   from implementation details of concrete blockchain.
data NodeSpec a = NodeSpec
  { nspecPrivKey :: !(Maybe (PrivValidator (Alg a)))
    -- ^ Validator's private key
  , nspecDbName  :: !(Maybe FilePath)
    -- ^ Path to database. If value is @Nothing@ in-memory database will be used.
  , nspecLogFile :: ![ScribeSpec]
    -- ^ List of loggers
  }
  deriving (Generic)
deriving via HSChainCfg (NodeSpec a)
    instance (Typeable (Alg a), Crypto (Alg a)) => JSON.FromJSON (NodeSpec a)

-- | Network parameters for real network
data RealNetSpec = RealNetSpec
  { realnetPort           :: !Word16
    -- ^ Port to listen on
  , realnetSeeds          :: [NetAddr]
    -- ^ List of addresses
  , realnetPrometheusPort :: !(Maybe Word16)
    -- ^ Port for promethues monitoring   
  }
  deriving (Generic,Show)
  deriving JSON.FromJSON via HSChainCfg RealNetSpec


-- | Default way to derive FromJSON
type HSChainCfg a = SnakeCase (DropSmart (Config a))

