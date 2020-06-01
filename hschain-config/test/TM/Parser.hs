{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE DerivingStrategies   #-}
{-# LANGUAGE DerivingVia          #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE UndecidableInstances #-}
-- |
module TM.Parser where

import Control.Monad.Trans.State.Strict
import Data.Aeson
import Data.Typeable
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
  ]

testsMangler :: TestTree
testsMangler = testGroup "Mangler"
  [ testCase "group 1" $ testMangler
      (manglerCaseInsensitive <> manglerSnakeCase <> manglerDropPrefix)
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
  deriving FromJSON via DropPrefix (Config Cfg')

data Cfg_ = Cfg_
  { _cfgPort   :: Int
  , _cfgPathDB :: String
  }
  deriving (Show,Eq,Generic)
  deriving FromJSON via DropPrefix (Config Cfg_)

data Cfg_2 = Cfg_2
  { cfg_Port   :: Int
  , cfg_PathDB :: String
  }
  deriving (Show,Eq,Generic)
  deriving FromJSON via DropPrefix (Config Cfg_2)


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

newtype CfgAdd = CfgAdd Cfg
  deriving (Show,Eq)
  deriving Generic  via TransparentGeneric Cfg
  deriving FromJSON via AddChar "a" (AddChar "b" (AddChar "c" (Config (Cfg))))


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
  deriving Generic via TransparentGeneric a

instance (KnownSymbol s, FromConfigJSON a) => FromJSON (AddChar s a) where
  parseJSON = parseConfigJSON mempty

instance (KnownSymbol s, FromConfigJSON a) => FromConfigJSON (AddChar s a) where
  parseConfigJSON m
    = coerceParser . parseConfigJSON (m <> manglerAdd c)
    where
      c:_ = symbolVal (Proxy @s)
