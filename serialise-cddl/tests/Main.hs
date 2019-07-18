{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
import Codec.Serialise
import Data.Typeable
import Data.Int
import Data.Word
import Data.ByteString.Arbitrary (fromABS)
import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.Text (renderStrict)
-- import Numeric.Natural
import qualified Data.Text            as T
import qualified Data.ByteString      as BS
import qualified Data.ByteString.Lazy as BL
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import qualified Test.QuickCheck                   as QC
import qualified Test.QuickCheck.Arbitrary.Generic as QC
import Text.Megaparsec (runParser)
import GHC.Generics (Generic)

import Codec.Serialise.CDDL.AST
import Codec.Serialise.CDDL.Parser
import Codec.Serialise.CDDL.Class
import Codec.Serialise.CDDL.Check


----------------------------------------------------------------
--
----------------------------------------------------------------

main :: IO ()
main = defaultMain $ testGroup "CDDL"
  [ testGroup "parser" testsParser
  , testGroup "schema roundtrip"
    -- Primitives
    [ propRoundtripSchema @Float
    , propRoundtripSchema @Double
    , propRoundtripSchema @Int
    , propRoundtripSchema @Int8
    , propRoundtripSchema @Int16
    , propRoundtripSchema @Int32
    , propRoundtripSchema @Int64
    , propRoundtripSchema @Word
    , propRoundtripSchema @Word8
    , propRoundtripSchema @Word16
    , propRoundtripSchema @Word32
    , propRoundtripSchema @Word64
    -- , propRoundtripSchema @Bool
    -- , propRoundtripSchema @Char
    -- , propRoundtripSchema @Integer
    -- , propRoundtripSchema @Natural
    -- , propRoundtripSchema @Ordering
    , propRoundtripSchema @()
    , propRoundtripSchema @T.Text
    , propRoundtripSchema @BS.ByteString
    , propRoundtripSchema @BL.ByteString
      -- Derived types
    , propRoundtripSchema @Foo0
    , propRoundtripSchema @Foo1
    , propRoundtripSchema @Foo2
    , propRoundtripSchema @Foo3
    , propRoundtripSchema @Bar1
    , propRoundtripSchema @(Bar2 Foo2)
    ]
  , testGroup "schema checks"
    -- Primitives
    [ testSchemaWorks @Int
    , testSchemaWorks @Float
    , testSchemaWorks @Int
    , testSchemaWorks @Int8
    , testSchemaWorks @Int16
    , testSchemaWorks @Int32
    , testSchemaWorks @Int64
    , testSchemaWorks @Word
    , testSchemaWorks @Word8
    , testSchemaWorks @Word16
    , testSchemaWorks @Word32
    -- , testSchemaWorks @Word64
    -- , testSchemaWorks @Bool
    -- , testSchemaWorks @Char
    -- , testSchemaWorks @Integer
    -- , testSchemaWorks @Natural
    -- , testSchemaWorks @Ordering
    , testSchemaWorks @()
    , testSchemaWorks @T.Text
    , testSchemaWorks @BS.ByteString
    , testSchemaWorks @BL.ByteString
    -- Compound types
    , testSchemaWorks @[Int]
      -- Derived types
    , testSchemaWorks @Foo0
    , testSchemaWorks @Foo1
    , testSchemaWorks @Foo2
    , testSchemaWorks @Foo3
    , testSchemaWorks @Bar1
    , testSchemaWorks @(Bar2 Foo2)
    ]
  ]


testsParser :: [TestTree]
testsParser =
  [ parseExample True "test-data/simple.cddl"
  , parseExample True "test-data/group-occur.cddl"
  , parseExample True "test-data/7071-concise.cddl"
  ]

parseExample :: Bool -> FilePath -> TestTree
parseExample success nm = testCase nm $ do
  str <- readFile nm
  case runParser cddl nm str of
    Right _ | success   -> return ()
            | otherwise -> assertFailure "Expected failure"
    Left  e | success   -> assertFailure (show e)
            | otherwise -> return ()


propRoundtripSchema :: forall a. (Typeable a, CDDL a) => TestTree
propRoundtripSchema
  = testCase (show (typeRep (Proxy @a)))
  $ Right c @=? c'
  where
    c' = runParser cddl "@"
       $ T.unpack . renderStrict . layoutPretty defaultLayoutOptions . pretty
       $ c
    c  = flattenSchema
       $ typeRepToVar <$> cddlSchema (Proxy @a)

----------------------------------------------------------------
-- Check schema
----------------------------------------------------------------

testSchemaWorks :: forall a. (CDDL a, QC.Arbitrary a, Show a, Typeable a) => TestTree
testSchemaWorks
  = testProperty (show (typeRep (Proxy @a))) (propSchemaWorks @a)

propSchemaWorks :: forall a. CDDL a => a -> QC.Property
propSchemaWorks b
  = QC.counterexample str
  $ QC.counterexample (show terms)
  $ check schema terms
  where
    terms  = deserialise $ serialise b
    c      = flattenSchema
           $ typeRepToVar <$> cddlSchema (Proxy @a)
    schema = resolveNames c
    str    = T.unpack . renderStrict . layoutPretty defaultLayoutOptions . pretty
           $ c




----------------------------------------------------------------
-- Data types
----------------------------------------------------------------

data Foo0 = Foo0
  deriving (Show,Generic)
data Foo1 = Foo1 Int
  deriving (Show,Generic)
data Foo2 = Foo2 Int Float
  deriving (Show,Generic)
data Foo3 = Foo3 { foo0 :: Int, foo1 :: Float, foo3 :: Double }
  deriving (Show,Generic)

data Bar1
  = B1 Int
  | B2
  deriving (Show,Generic)

data Bar2 a
  = B11 a
  | B22
  | B33 [Int] Foo2
  deriving (Show,Generic)

instance Serialise Foo0
instance CDDL      Foo0
instance Serialise Foo1
instance CDDL      Foo1
instance Serialise Foo2
instance CDDL      Foo2
instance Serialise Foo3
instance CDDL      Foo3

instance Serialise Bar1
instance CDDL      Bar1
instance Serialise a => Serialise (Bar2 a)
instance CDDL      a => CDDL      (Bar2 a)

instance QC.Arbitrary Foo0 where
  arbitrary = QC.genericArbitrary
instance QC.Arbitrary Foo1 where
  arbitrary = QC.genericArbitrary
instance QC.Arbitrary Foo2 where
  arbitrary = QC.genericArbitrary
instance QC.Arbitrary Foo3 where
  arbitrary = QC.genericArbitrary
instance QC.Arbitrary Bar1 where
  arbitrary = QC.genericArbitrary
instance QC.Arbitrary a => QC.Arbitrary (Bar2 a) where
  arbitrary = QC.genericArbitrary


instance QC.Arbitrary BS.ByteString where
  arbitrary = fromABS <$> arbitrary
  shrink bs = BS.pack <$> shrink (BS.unpack bs)
instance QC.Arbitrary BL.ByteString where
  arbitrary = BL.fromStrict . fromABS <$> arbitrary

instance QC.Arbitrary T.Text where
  arbitrary = T.pack <$> arbitrary
