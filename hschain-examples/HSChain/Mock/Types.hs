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
    -- * Data types
    makeGenesis
    -- ** Node and network specification
  , NodeSpec(..)
  , NetSpec(..)
  , CoinSpecification(..)
  , RunningNode(..)
  , hoistRunningNode
    -- ** Other
  , Topology(..)
  , Abort(..)
  , catchAbort
  , Example
  ) where

import Control.Exception   (Exception)
import Control.Monad.Catch (MonadCatch(..))
import Data.Default.Class
import Data.Typeable
import GHC.Generics (Generic)

import qualified Data.Aeson as JSON

import HSChain.Blockchain.Internal.Engine.Types
import HSChain.Config
import HSChain.Crypto
import HSChain.Logger         (ScribeSpec)
import HSChain.Mempool
import HSChain.Types
import HSChain.Types.Merkle.Types
import HSChain.Store

----------------------------------------------------------------
--
----------------------------------------------------------------

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
    }


-- | Genesis block has many field with predetermined content so this
--   is convenience function to create genesis block.
makeGenesis
  :: (Crypto (Alg a), CryptoHashable a)
  => a                          -- ^ Block data
  -> Hashed (Alg a) (BlockchainState a)
  -> ValidatorSet (Alg a)           -- ^ Set of validators for H=0
  -> ValidatorSet (Alg a)           -- ^ Set of validators for H=1
  -> Block a
makeGenesis dat stateHash valSet0 valSet1 = Block
  { blockHeight        = Height 0
  , blockPrevBlockID   = Nothing
  , blockValidators    = hashed valSet0
  , blockNewValidators = hashed valSet1
  , blockData          = merkled dat
  , blockPrevCommit    = Nothing
  , blockEvidence      = merkled []
  , blockStateHash     = stateHash
  }


----------------------------------------------------------------
-- Generating node specification
----------------------------------------------------------------

data RunningNode m a = RunningNode
  { rnodeState   :: BChStore m a
  , rnodeConn    :: Connection 'RO a
  , rnodeMempool :: Mempool m (Alg a) (TX a)
  }

hoistRunningNode
  :: (Functor n)
  => (forall x. m x -> n x) -> RunningNode m a -> RunningNode n a
hoistRunningNode fun RunningNode{..} = RunningNode
  { rnodeState   = hoistDict    fun rnodeState
  , rnodeMempool = hoistMempool fun rnodeMempool
  , ..
  }


-- | Exception for aborting execution of blockchain
data Abort = Abort
  deriving Show
instance Exception Abort

catchAbort :: MonadCatch m => m () -> m ()
catchAbort act = catch act (\Abort -> return ())

data Topology = All2All
              | Ring
              deriving (Generic,Show)
instance JSON.ToJSON   Topology
instance JSON.FromJSON Topology


-- | Specification of test cluster
data NetSpec a = NetSpec
  { netNodeList  :: [a]                   -- ^ List of nodes
  , netTopology  :: Topology              -- ^ Network topology
  , netNetCfg    :: Configuration Example -- ^ Delay and such
  , netMaxH      :: Maybe Height          -- ^ Maximum height
  }
  deriving (Generic,Show)
  deriving JSON.FromJSON via SnakeCase (DropSmart (Config (NetSpec a)))

data NodeSpec b = NodeSpec
  { nspecPrivKey     :: !(Maybe (PrivValidator (Alg b)))
  , nspecDbName      :: !(Maybe FilePath)
  , nspecLogFile     :: ![ScribeSpec]
  , nspecPersistIval :: !(Maybe Int)
    -- ^ Interval between persisting state to database
  }
  deriving (Generic)

deriving via SnakeCase (DropSmart (Config (NodeSpec b)))
  instance (Typeable (Alg b), Crypto (Alg b)) => JSON.FromJSON (NodeSpec b)

-- | Specifications for mock coin status.
data CoinSpecification = CoinSpecification
 { coinAirdrop        :: !Integer     -- ^ Amount of coins allocated to each wallet
 , coinWallets        :: !Int         -- ^ Number of wallets in use
 , coinWalletsSeed    :: !Int         -- ^ Seed used to generate private keys for wallets
 , coinGeneratorDelay :: !(Maybe Int) -- ^ Delay between TX generation. Nothing means don't generate
 , coinMaxMempoolSize :: !Int         -- ^ If mempool exceeds size new txs won't be generated
 }
 deriving (Generic,Show)
 deriving JSON.FromJSON via SnakeCase (DropSmart (Config CoinSpecification))
