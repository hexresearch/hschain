{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE DerivingStrategies   #-}
{-# LANGUAGE DerivingVia          #-}
{-# LANGUAGE UndecidableInstances #-}
-- |
module TM.Parser where

import Data.Aeson
import Data.Typeable
import Test.Tasty
import Test.Tasty.HUnit
import GHC.Generics

import HSChain.Config
import HSChain.Config.Internal


tests :: TestTree
tests = testGroup "Parser"
  [ runTest "test/data/cfg-simple.json" $ CfgSimple (Cfg 123 "/tmp")
  , runTest "test/data/cfg-drop.json"   $ CfgDrop (Cfg 123 "/tmp")
  , runTest "test/data/cfg-drop.json"   $ Cfg' 123 "/tmp"
  , runTest "test/data/cfg-dropN.json"  $ CfgDropN (Cfg 123 "/tmp")
  , runTest "test/data/cfg-snake.json"  $ CfgSnakeCase (Cfg 123 "/tmp")
  , runTest "test/data/cfg-ci.json"     $ CfgCI (Cfg 123 "/tmp")
  ]


runTest :: (Eq a, Show a, FromJSON a, Typeable a) => FilePath -> a -> TestTree
runTest path a0 = testCase (show (typeOf a0)) $ do
  r <- eitherDecodeFileStrict path
  case r of
    Left  e -> putStrLn e >> assertFailure "Parse failed"
    Right a -> a0 @=? a

----------------------------------------------------------------
-- Data types
----------------------------------------------------------------

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

-- Uses SnakeCase+DropPrefix
newtype CfgSnakeCase = CfgSnakeCase Cfg
  deriving (Show,Eq)
  deriving Generic  via TransparentGeneric Cfg
  deriving FromJSON via SnakeCase (DropPrefix (Config (Cfg)))

-- Uses CaseInsensitive+SnakeCase+DropPrefix
newtype CfgCI = CfgCI Cfg
  deriving (Show,Eq)
  deriving Generic  via TransparentGeneric Cfg
  deriving FromJSON via CaseInsensitive (SnakeCase (DropPrefix (Config (Cfg))))
