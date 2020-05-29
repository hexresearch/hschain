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
  [ testCase "CfgSimple" $ do Just a <- decodeFileStrict "test/data/cfg-1.json"
                              CfgSimple (Cfg 123 "/tmp") @=? a
  , testCase "CfgSimple" $ do Just a <- decodeFileStrict "test/data/cfg-2.json"
                              CfgDrop (Cfg 123 "/tmp") @=? a  
  ]


data Cfg = Cfg
  { cfgPort :: Int
  , cfgPath :: String
  }
  deriving (Show,Eq,Generic)

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
