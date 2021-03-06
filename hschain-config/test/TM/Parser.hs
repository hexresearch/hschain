{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE UndecidableInstances       #-}
-- |
module TM.Parser where

import Control.Monad.Trans.State.Strict
import Data.Aeson
import Data.Coerce
import Data.Typeable
import Data.Default.Class
import Test.Tasty
import Test.Tasty.HUnit
import GHC.Generics
import GHC.TypeLits

import HSChain.Config
import HSChain.Config.Internal.Impl
import HSChain.Config.Internal.Classes


testsParser :: TestTree
testsParser = testGroup "Parser"
  [ runTest "test/data/cfg-simple.json" $ CfgSimple (Cfg 123 "/tmp")
  , runTest "test/data/cfg-drop.json"   $ CfgDrop (Cfg 123 "/tmp")
  , runTest "test/data/cfg-drop.json"   $ Cfg' 123 "/tmp"
  , runTest "test/data/cfg-drop.json"   $ Cfg_ 123 "/tmp"
  , runTest "test/data/cfg-drop.json"   $ Cfg_2 123 "/tmp"
  , runTest "test/data/cfg-dropN.json"  $ CfgDropN (Cfg 123 "/tmp")
  , runTest "test/data/cfg-snake.json"  $ CfgSnakeCase (Cfg 123 "/tmp")
  , runTest "test/data/cfg-ci.json"     $ CfgCI (Cfg 123 "/tmp")
  , runTest "test/data/cfg-abc.json"    $ CfgAdd (Cfg 123 "/tmp")
  , runTest "test/data/cfg-prefix.json" $ CfgPref (Cfg 123 "/tmp")
  , runTest "test/data/cfg-empty.json"  $ CfgDef def
  , runTest "test/data/cfg-test2.json"  $ Test 123 "/tmp"
  ]

testsMangler :: TestTree
testsMangler = testGroup "Mangler"
  [ testCase "group 1" $ testMangler
      (manglerCaseInsensitive <> manglerSnakeCase <> manglerDropSmart)
      ["cfgPathDB", "cfgPort"]
      ["path_db", "port"]
  , testCase "simple"  $ testMangler
      (manglerAdd 'a' <> manglerAdd 'b' <> manglerAdd 'c')
      ["cfgPathDB", "cfgPort"]
      ["abccfgPathDB", "abccfgPort"]
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
  deriving FromJSON via DropSmart (Config Cfg')

data Cfg_ = Cfg_
  { _cfgPort   :: Int
  , _cfgPathDB :: String
  }
  deriving (Show,Eq,Generic)
  deriving FromJSON via DropSmart (Config Cfg_)

data Cfg_2 = Cfg_2
  { cfg_Port   :: Int
  , cfg_PathDB :: String
  }
  deriving (Show,Eq,Generic)
  deriving FromJSON via DropSmart (Config Cfg_2)

data Test = Test
  { tstTest    :: Int
  , tstBChData :: String
  }
  deriving (Show,Eq,Generic)
  deriving FromJSON via SnakeCase (DropSmart (Config Test))


-- Uses plain Config
newtype CfgSimple = CfgSimple Cfg
  deriving stock (Show,Eq)
  deriving newtype Generic
  deriving FromJSON via Config (Cfg)

-- Uses DropSmart
newtype CfgDrop = CfgDrop Cfg
  deriving stock   (Show,Eq)
  deriving newtype Generic
  deriving FromJSON via DropSmart (Config (Cfg))

-- Uses DropSmart
newtype CfgPref = CfgPref Cfg
  deriving stock   (Show,Eq)
  deriving newtype Generic
  deriving FromJSON via DropPrefix "cf" (Config (Cfg))

-- Uses DropSmart
newtype CfgDropN = CfgDropN Cfg
  deriving stock   (Show,Eq)
  deriving newtype Generic
  deriving FromJSON via DropN 2 (Config (Cfg))

-- Uses SnakeCase+DropSmart
newtype CfgSnakeCase = CfgSnakeCase Cfg
  deriving stock   (Show,Eq)
  deriving newtype Generic
  deriving FromJSON via SnakeCase (DropSmart (Config (Cfg)))

-- Uses CaseInsensitive+SnakeCase+DropSmart
newtype CfgCI = CfgCI Cfg
  deriving stock   (Show,Eq)
  deriving newtype Generic
  deriving FromJSON via CaseInsensitive (SnakeCase (DropSmart (Config (Cfg))))

newtype CfgAdd = CfgAdd Cfg
  deriving stock   (Show,Eq)
  deriving newtype Generic
  deriving FromJSON via AddChar "a" (AddChar "b" (AddChar "c" (Config (Cfg))))

newtype CfgDef = CfgDef Cfg
  deriving stock   (Show,Eq)
  deriving newtype Generic
  deriving FromJSON via WithDefault (DropSmart (Config (Cfg)))

instance Default Cfg where
  def = Cfg
    { cfgPort   = 5000
    , cfgPathDB = "/db/db"
    }
  

----------------------------------------------------------------
-- Mangler
----------------------------------------------------------------

testMangler :: Mangler -> [String] -> [String] -> IO ()
testMangler m selectors expected = do
  assertEqual "Selectors:" expected sels
  assertEqual "Mangler:  " expected (f <$> selectors)
  where
    (f,sels) = runState (mangleSelector m) selectors

manglerAdd :: Char -> Mangler
manglerAdd c = simpleMangler (c:)


newtype AddChar (s :: Symbol) a = AddChar a

instance (KnownSymbol s, FromConfigJSON a) => FromJSON (AddChar s a) where
  parseJSON = parseConfigJSON mempty Nothing

instance (KnownSymbol s, FromConfigJSON a) => FromConfigJSON (AddChar s a) where
  parseConfigJSON m a
    = coerceParser . parseConfigJSON (m <> manglerAdd c) (coerce a)
    where
      c:_ = symbolVal (Proxy @s)
