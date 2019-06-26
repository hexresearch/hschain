{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}
module Thundermint.Mock.Types (
    -- * Data types
    -- ** Node and network specification
    NodeSpec(..)
  , NetSpec(..)
  , CoinSpecification(..)
    -- ** Other
  , Topology(..)
  , Abort(..)
  , Example
    -- * Anonymous product
  , (:*:)(..)
  , Has(..)
  , (^..)
  ) where

import Control.Exception (Exception)
import Data.Type.Equality
import GHC.Exts     (Proxy#,proxy#)
import GHC.Generics (Generic)

import qualified Data.Aeson as JSON

import Thundermint.Blockchain.Internal.Engine.Types
import Thundermint.Crypto         ((:&))
import Thundermint.Crypto.Ed25519 (Ed25519)
import Thundermint.Crypto.SHA     (SHA512)
import Thundermint.Logger         (ScribeSpec)
import Thundermint.Types          (Height(..))


----------------------------------------------------------------
--
----------------------------------------------------------------

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
      , pexMinKnownConnections = 3
      , pexMaxKnownConnections = 20
      , reconnectionRetries    = 12
      , reconnectionDelay      = 100
      }
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
  { nspecPrivKey    :: Maybe (PrivValidator (Ed25519 :& SHA512))
  , nspecDbName     :: Maybe FilePath
  , nspecLogFile    :: [ScribeSpec]
  , nspecWalletKeys :: (Int,Int)
  }
  deriving (Generic,Show)

-- | Specification of mock cluster
data NetSpec a = NetSpec
  { netNodeList       :: [a]      -- ^ List of nodes
  , netTopology       :: Topology -- ^ Network topology
  , netNetCfg         :: Configuration Example
  , netMaxH           :: Maybe Height

  , netInitialDeposit :: Integer
  , netInitialKeys    :: Int
  }
  deriving (Generic,Show)

-- | Specifications for mock coin status.
data CoinSpecification = CoinSpecification
 { coinAridrop     :: !Integer -- ^ Amount of coins allocated to each wallet
 , coinWallets     :: !Int     -- ^ Number of wallets in use
 , coinWalletsSeed :: !Int     -- ^ Seed used to generate private keys for wallets
 }
 deriving (Generic,Show)

instance JSON.ToJSON   NodeSpec

instance JSON.FromJSON CoinSpecification
instance JSON.FromJSON NodeSpec
instance JSON.FromJSON a => JSON.FromJSON (NetSpec a)



----------------------------------------------------------------
-- Anonymous products
----------------------------------------------------------------

-- | Anonymous products. Main feature is lookup of value by its type
data a :*: b = a :*: b
  deriving (Show,Eq)
infixr 5 :*:

instance (JSON.FromJSON a, JSON.FromJSON b) => JSON.FromJSON (a :*: b) where
  parseJSON = JSON.withObject "Expecting object" $ \o -> do
    a <- JSON.parseJSON (JSON.Object o)
    b <- JSON.parseJSON (JSON.Object o)
    return (a :*: b)


-- | Obtain value from product using its type
class Has a x where
  getT :: a -> x

-- | Lens-like getter
(^..) :: (Has a x) => a -> (x -> y) -> y
a ^.. f = f (getT a)


class HasCase a x (eq :: Bool) where
  getCase :: Proxy# eq -> a -> x

instance {-# OVERLAPPABLE #-} (a ~ b) => Has a b where
  getT = id
instance HasCase (a :*: b) x (a == x) => Has (a :*: b) x where
  getT = getCase (proxy# :: Proxy# (a == x))

instance (a ~ x)   => HasCase (a :*: b) x 'True where
  getCase _ (a :*: _) = a
instance (Has b x) => HasCase (a :*: b) x 'False where
  getCase _ (_ :*: b) = getT b
