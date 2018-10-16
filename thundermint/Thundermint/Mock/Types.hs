{-# LANGUAGE DeriveGeneric #-}
module Thundermint.Mock.Types (
  NodeSpec(..)
, NetSpec(..)
, Topology(..)
, Abort(..)
, ExeConfig(..)
, defCfg
  ) where

import Control.Exception (Exception)
import Data.Int     (Int64)
import GHC.Generics (Generic)

import qualified Data.Aeson as JSON

import Thundermint.Blockchain.Internal.Engine.Types
import Thundermint.Crypto.Ed25519 (Ed25519_SHA512)
import Thundermint.Logger         (ScribeSpec)



defCfg :: Configuration
defCfg = Configuration
  { timeoutNewHeight   = (500, 500)
  , timeoutProposal    = (500, 500)
  , timeoutPrevote     = (500, 500)
  , timeoutPrecommit   = (500, 500)
  , gossipDelayVotes   = 25
  , gossipDelayBlocks  = 25
  , gossipDelayMempool = 25
  , pexMinConnections  = 10
  , pexMaxConnections  = 20
  , pexMinKnownConnections = 15
  , pexMaxKnownConnections = 20
  }


----------------------------------------------------------------
-- Generating node specification
----------------------------------------------------------------

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
  }
  deriving (Generic,Show)

instance JSON.ToJSON   NodeSpec
instance JSON.FromJSON NodeSpec
instance JSON.ToJSON   a => JSON.ToJSON   (NetSpec a)
instance JSON.FromJSON a => JSON.FromJSON (NetSpec a)


data ExeConfig a = ExeConfig
  { netSpec :: NetSpec a
  , netCfg  :: Configuration
  }
  deriving (Generic, Show)

instance JSON.ToJSON a => JSON.ToJSON (ExeConfig a)
instance JSON.FromJSON a => JSON.FromJSON (ExeConfig a)
