{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- | Tests for some serialisation things
--
module TM.Serialisation (tests) where

import qualified Codec.Serialise as CBOR
import qualified Data.Aeson      as JSON
import Data.Typeable

import Test.QuickCheck.Arbitrary
import Test.Tasty
import Test.Tasty.QuickCheck

import HSChain.Network.Types  (NetAddr(..))


----------------------------------------------------------------
-- Test tree
----------------------------------------------------------------

tests :: TestTree
tests =
  testGroup "serialisation"
    [ let run :: forall a. ( Arbitrary a, Show a, Typeable a, Eq a, CBOR.Serialise a)
              => TestTree
          run = testProperty (show (typeRep (Proxy @a)))
                                (prop_CBOR_roundtrip @a)
      in testGroup "CBOR"
         [ run @NetAddr
         ]
    , let run :: forall a. ( Arbitrary a, Show a, Typeable a, Eq a
                           , JSON.FromJSON a, JSON.ToJSON a)
              => TestTree
          run = testProperty (show (typeRep (Proxy @a)))
                                (prop_JSON_roundtrip @a)
      in testGroup "JSON"
         [ run @NetAddr
         ]
    , let run :: forall a. (Arbitrary a, Typeable a, Eq a, Show a, Read a)
              => TestTree
          run = testProperty (show (typeRep (Proxy @a)))
                                (prop_ShowRead_roundtrip @a)
      in testGroup "Read/Show"
         [ run @NetAddr
         , testProperty "NetAddr tcp://" $ \(a :: NetAddr) ->
             a == (read . ("tcp://"++) . show) a
         ]
    ]

----------------------------------------------------------------

-- CBOR.deserialise . CBOR.serialise
prop_CBOR_roundtrip :: (Eq a, CBOR.Serialise a) => a -> Bool
prop_CBOR_roundtrip a = a == (CBOR.deserialise . CBOR.serialise) a

-- JSON.decode . JSON.encode = id
prop_JSON_roundtrip :: (Eq a, JSON.FromJSON a, JSON.ToJSON a) => a -> Bool
prop_JSON_roundtrip a = Just a == (JSON.decode . JSON.encode) a

-- Read . show = id
prop_ShowRead_roundtrip :: (Eq a, Show a, Read a) => a -> Bool
prop_ShowRead_roundtrip a = a == (read . show) a


instance Arbitrary NetAddr where
  arbitrary = oneof
    [ NetAddrV4   <$> arbitrary <*> arbitrary
    , NetAddrV6   <$> arbitrary <*> arbitrary
    ]
