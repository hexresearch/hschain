{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeOperators     #-}

module App.Settings where


import Data.Text    (Text)
import GHC.Generics (Generic)

import Thundermint.Blockchain.Internal.Engine.Types

import Thundermint.Crypto
import Thundermint.Crypto.Ed25519 (Ed25519)
import Thundermint.Crypto.SHA     (SHA512)
import Thundermint.Logger         (ScribeSpec)
import Thundermint.P2P            (NetAddr)
import Thundermint.Types

import App.AesonTH

----------------------------------------------------------------
-- Generating node specification
----------------------------------------------------------------


-- | Specification of nodes
data CoinNodeSpec = CoinNodeSpec
  { nspec'privKey        :: Maybe (PrivValidator (Ed25519 :& SHA512))
    -- ^ Private key of validator
  , nspec'validators     :: [PrivValidator (Ed25519 :& SHA512)]
    -- ^ Set of public keys of validator nodes
  , nspec'dbName         :: FilePath
    -- ^ Database name for the node
  , nspec'port           :: Int
    -- ^ Port to listen on
  , nspec'number         :: Int
    -- ^ Node number
  , nspec'count          :: Int
    -- ^ Nodes count in cluster
  , nspec'seeds          :: [NetAddr]
    -- ^ Set of initial addresses
  , nspec'monitoringPort :: Maybe Int
  }
  deriving (Generic,Show)

-- | Specification of logging
data LogSpec = LogSpec
  { logSpec'logFiles       :: [ScribeSpec]
    -- ^ Log files to write to
  , logSpec'hostnameSuffix :: Maybe String
    -- ^ Suffix to append to host name in logs.
  , logSpec'clusterId      :: Maybe Text
    -- ^ Cluster ID will written to @env@ field of logs
  }
  deriving(Show)

-- | Specification of coin node
data CoinSpec = CoinSpec
  { coin'maxH       :: Height
  , coin'delay      :: Int
  , coin'deposit    :: Integer
  , coin'keys       :: Int
  , coin'walletKeys :: (Int,Int)

  , coin'byzantine  :: Maybe String
  }
  deriving (Generic,Show)


-- | Validator settings
data CoinSettings = CoinSettings
  { settings'node :: !CoinNodeSpec
  , settings'coin :: !CoinSpec
  , settings'logs :: !LogSpec
  } deriving (Show)


$(deriveJSON dropPrefixOptions ''LogSpec)
$(deriveJSON dropPrefixOptions ''CoinNodeSpec)
$(deriveJSON dropPrefixOptions ''CoinSpec)
$(deriveJSON dropPrefixOptions ''CoinSettings)

