{-# LANGUAGE DeriveGeneric #-}
module Thundermint.Mock.Types (
    NodeSpec(..)
  , NetSpec(..)
  , Topology(..)
  , Abort(..)
  , Example
  , defCfg
    -- * Standard callbacks
  , abortAtH
  ) where

import Control.Exception (Exception)
import Control.Monad
import Control.Monad.Catch
import Data.Int     (Int64)
import GHC.Generics (Generic)

import qualified Data.Aeson as JSON

import Thundermint.Types
import Thundermint.Blockchain.Internal.Engine.Types
import Thundermint.Crypto.Ed25519 (Ed25519_SHA512)
import Thundermint.Logger         (ScribeSpec)



data Example

instance DefaultConfig Example where
  defCfg = Configuration
    { cfgConsensus         = ConsensusCfg
      { timeoutNewHeight   = (500, 500)
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
      , pexMinKnownConnections = 15
      , pexMaxKnownConnections = 20
      , reconnectionRetries    = 12
      , reconnectionDelay      = 100
      }
    }



----------------------------------------------------------------
-- Generating node specification
----------------------------------------------------------------

-- | Callback which aborts execution of program at given height
abortAtH :: MonadThrow m => Height -> AppCallbacks m alg a
abortAtH hMax = mempty
  { appCommitCallback = \b ->
      when (headerHeight (blockHeader b) > hMax) $ throwM Abort
  }

-- | Exception for aborting execution of blockchain
data Abort = Abort
  deriving Show
instance Exception Abort

data Topology = All2All
              | Ring
              deriving (Generic,Show)
instance JSON.ToJSON   Topology
instance JSON.FromJSON Topology

data NodeSpec = NodeSpec
  { nspecPrivKey    :: Maybe (PrivValidator Ed25519_SHA512)
  , nspecDbName     :: Maybe FilePath
  , nspecLogFile    :: [ScribeSpec]
  , nspecWalletKeys :: (Int,Int)
  , nspecByzantine  :: Maybe String
  }
  deriving (Generic,Show)

data NetSpec a = NetSpec
  { netNodeList       :: [a]
  , netTopology       :: Topology
  , netInitialDeposit :: Integer
  , netInitialKeys    :: Int
  , netPrefix         :: Maybe String
  , netMaxH           :: Maybe Int64
  , netNetCfg         :: Configuration Example
  }
  deriving (Generic,Show)

instance JSON.ToJSON   NodeSpec
instance JSON.FromJSON NodeSpec
instance JSON.FromJSON a => JSON.FromJSON (NetSpec a)
