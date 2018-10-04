{-# LANGUAGE DeriveGeneric #-}
module Thundermint.Mock.Types (
  NodeSpec(..)
, NetSpec(..)
, Topology(..)
, Abort(..)
  , handleNAbort
, defCfg
  ) where

import Control.Exception (Exception)
import Control.Monad.Catch
import Data.Monoid  (Endo(..))
import GHC.Generics (Generic)

import qualified Data.Aeson as JSON


import Thundermint.Crypto.Ed25519 (Ed25519_SHA512)
import Thundermint.Logger         (ScribeSpec)

import Thundermint.Blockchain.Types



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

-- | Handle up to N @Abort@ exceptions. When we run N blockchains
--   simultaneously we can get up to N exceptions
handleNAbort :: MonadCatch m => Int -> m () -> m ()
handleNAbort n = appEndo $ foldMap Endo $ replicate n $ handle (\Abort -> return ())


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

data NetSpec = NetSpec
  { netNodeList       :: [NodeSpec]
  , netTopology       :: Topology
  , netInitialDeposit :: Integer
  , netInitialKeys    :: Int
  , netPrefix         :: Maybe String
  }
  deriving (Generic,Show)

instance JSON.ToJSON   NodeSpec
instance JSON.ToJSON   NetSpec
instance JSON.FromJSON NodeSpec
instance JSON.FromJSON NetSpec
