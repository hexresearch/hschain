{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}
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
import GHC.Generics (Generic)

import qualified Data.Aeson as JSON

import HSChain.Blockchain.Internal.Engine.Types
import HSChain.Crypto
import HSChain.Crypto.Ed25519 (Ed25519)
import HSChain.Crypto.SHA     (SHA512)
import HSChain.Logger         (ScribeSpec)
import HSChain.Types
import HSChain.Types.Merkle.Types
import HSChain.Store

----------------------------------------------------------------
--
----------------------------------------------------------------

data Example

instance DefaultConfig Example where
  defCfg = Configuration
    { cfgConsensus         = ConsensusCfg
      { timeoutNewHeight   = 500
      , timeoutProposal    = (500, 500)
      , timeoutPrevote     = (500, 500)
      , timeoutPrecommit   = (500, 500)
      , timeoutEmptyBlock  = 100
      , incomingQueueSize  = 7
      }
    , cfgNetwork               = NetworkCfg
      { gossipDelayVotes       = 25
      , gossipDelayBlocks      = 25
      , gossipDelayMempool     = 25
      , pexMinConnections      = 3
      , pexMaxConnections      = 20
      , pexMinKnownConnections = 3
      , pexMaxKnownConnections = 20
      , reconnectionRetries    = 12
      , reconnectionDelay      = 100
      }
    }

-- | Genesis block has many field with predetermined content so this
--   is convenience function to create genesis block.
makeGenesis
  :: (Crypto alg, CryptoHashable a)
  => a                          -- ^ Block data
  -> Hashed alg (InterpreterState a)
  -> ValidatorSet alg           -- ^ Set of validators for H=0
  -> ValidatorSet alg           -- ^ Set of validators for H=1
  -> Block alg a
makeGenesis dat stateHash valSet0 valSet1 = Block
  { blockHeight        = Height 0
  , blockPrevBlockID   = Nothing
  , blockValidators    = merkled valSet0
  , blockNewValidators = merkled valSet1
  , blockData          = merkled dat
  , blockPrevCommit    = Nothing
  , blockEvidence      = merkled []
  , blockStateHash     = stateHash
  }


----------------------------------------------------------------
-- Generating node specification
----------------------------------------------------------------

data RunningNode m alg a = RunningNode
  { rnodeState   :: BChStore m a
  , rnodeConn    :: Connection 'RO alg a
  , rnodeMempool :: Mempool m alg (TX a)
  }

hoistRunningNode
  :: (Functor n)
  => (forall x. m x -> n x) -> RunningNode m alg a -> RunningNode n alg a
hoistRunningNode fun RunningNode{..} = RunningNode
  { rnodeState   = hoistBChStore fun rnodeState
  , rnodeMempool = hoistMempool  fun rnodeMempool
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

data NodeSpec = NodeSpec
  { nspecPrivKey    :: Maybe (PrivValidator (Ed25519 :& SHA512))
  , nspecDbName     :: Maybe FilePath
  , nspecLogFile    :: [ScribeSpec]
  }
  deriving (Generic,Show)

-- | Specifications for mock coin status.
data CoinSpecification = CoinSpecification
 { coinAridrop        :: !Integer     -- ^ Amount of coins allocated to each wallet
 , coinWallets        :: !Int         -- ^ Number of wallets in use
 , coinWalletsSeed    :: !Int         -- ^ Seed used to generate private keys for wallets
 , coinGeneratorDelay :: !(Maybe Int) -- ^ Delay between TX generation. Nothing means don't generate
 , coinMaxMempoolSize :: !Int         -- ^ If mempool exceeds size new txs won't be generated
 }
 deriving (Generic,Show)

instance JSON.ToJSON   NodeSpec

instance JSON.FromJSON CoinSpecification
instance JSON.FromJSON NodeSpec
instance JSON.FromJSON a => JSON.FromJSON (NetSpec a)
