{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE DerivingStrategies   #-}
{-# LANGUAGE DerivingVia          #-}
{-# LANGUAGE UndecidableInstances #-}
-- |
module TM.Parser where

import Data.Aeson
import Test.Tasty
import Test.Tasty.HUnit
import GHC.Generics

import HSChain.Config
import HSChain.Config.Internal


tests :: TestTree
tests = testGroup "Parser"
  [ testCase "CfgSimple" $ do Just a <- decodeFileStrict "test/data/cfg-simple.json"
                              CfgSimple (Cfg 123 "/tmp") @=? a
  , testCase "CfgDrop"   $ do Just a <- decodeFileStrict "test/data/cfg-drop.json"
                              CfgDrop (Cfg 123 "/tmp") @=? a
  , testCase "Cfg'"      $ do Just a <- decodeFileStrict "test/data/cfg-drop.json"
                              Cfg' 123 "/tmp" @=? a
  , testCase "CfgDropN"  $ do Just a <- decodeFileStrict "test/data/cfg-dropN.json"
                              CfgDropN (Cfg 123 "/tmp") @=? a
  ]


data Cfg = Cfg
  { cfgPort   :: Int
  , cfgPathDB :: String
  }
  deriving (Show,Eq,Generic)

data Cfg' = Cfg'
  { cfg'port   :: Int
  , cfg'pathDB :: String
  }
  deriving (Show,Eq,Generic)
  deriving FromJSON via DropPrefix (Config Cfg')

-- Uses plain Config
newtype CfgSimple = CfgSimple Cfg
  deriving (Show,Eq)
  deriving Generic  via TransparentGeneric Cfg
  deriving FromJSON via Config (Cfg)

-- Uses DropPrefix
newtype CfgDrop = CfgDrop Cfg
  deriving (Show,Eq)
  deriving Generic  via TransparentGeneric Cfg
  deriving FromJSON via DropPrefix (Config (Cfg))

-- Uses DropPrefix
newtype CfgDropN = CfgDropN Cfg
  deriving (Show,Eq)
  deriving Generic  via TransparentGeneric Cfg
  deriving FromJSON via DropN 2 (Config (Cfg))
