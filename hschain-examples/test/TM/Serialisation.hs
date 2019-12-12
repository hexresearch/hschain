{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
-- | Tests for some serialisation things
--
module TM.Serialisation (tests) where

import qualified Codec.Serialise as CBOR
import qualified Data.Aeson      as JSON
import Data.Coerce
import Data.Typeable

import Test.QuickCheck.Arbitrary
import Test.Tasty
import Test.Tasty.QuickCheck

import HSChain.Crypto         ((:&))
import HSChain.Crypto.Ed25519 (Ed25519)
import HSChain.Crypto.SHA     (SHA512)
import HSChain.Types.Blockchain
import HSChain.P2P.Types      (NetAddr)
import HSChain.Mock.KeyVal    (BData(..))

import HSChain.Arbitrary.Instances ()

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
         [ testGroup "roundtrip"
           [ run @(Block  BData)
           , run @(Header BData)
           , run @(Commit BData)
           , run @(ByzantineEvidence BData)
           , run @(Proposal BData)
           , run @(Vote 'PreVote   BData)
           , run @(Vote 'PreCommit BData)
           , run @Height
           , run @Round
           , run @Time
           , run @NetAddr
           ]
         , testGroup "Vote"
           [ testProperty "serialize result for prevote and precommit differs"
             prop_diff_serialize
           , testProperty "cannot deserialise one vote type as other PV->PC"
             (prop_diff_votes_dont_roundtrip @'PreVote)
           , testProperty "cannot deserialise one vote type as other PC->PV"
             (prop_diff_votes_dont_roundtrip @'PreCommit)
           ]
         ]
    , let run :: forall a. ( Arbitrary a, Show a, Typeable a, Eq a
                           , JSON.FromJSON a, JSON.ToJSON a)
              => TestTree
          run = testProperty (show (typeRep (Proxy @a)))
                                (prop_JSON_roundtrip @a)
      in testGroup "JSON"
         [ run @(Block  BData)
         , run @(Header BData)
         , run @(Commit BData)
         , run @(ByzantineEvidence BData)
         , run @(Proposal BData)
         , run @(Vote 'PreVote   BData)
         , run @(Vote 'PreCommit BData)
         , run @Height
         , run @Round
         , run @Time
         , run @NetAddr
         ]
    , let run :: forall a. (Arbitrary a, Typeable a, Eq a, Show a, Read a)
              => TestTree
          run = testProperty (show (typeRep (Proxy @a)))
                                (prop_ShowRead_roundtrip @a)
      in testGroup "Read/Show"
         [ run @Height
         , run @Round
         , run @Time
         , run @NetAddr
         , testProperty "NetAddr tcp://" $ \(a :: NetAddr) ->
             a == (read . ("tcp://"++) . show) a
         ]
    ]

----------------------------------------------------------------
-- Tests
----------------------------------------------------------------

type TestVote ty   = Vote ty BData

deriving newtype instance Arbitrary BData


-- Votes of one type can't be deserialized as other
prop_diff_votes_dont_roundtrip
  :: forall ty. ( CBOR.Serialise (TestVote ty)
                , CBOR.Serialise (TestVote (OtherVote ty)))
  => TestVote ty -> Bool
prop_diff_votes_dont_roundtrip vote =
  case CBOR.deserialiseOrFail (CBOR.serialise vote) of
    Left   _                             -> True
    Right (_ :: TestVote (OtherVote ty)) -> False

type family OtherVote x where
  OtherVote 'PreVote   = 'PreCommit
  OtherVote 'PreCommit = 'PreVote


-- Prevotes and precommits are serialized differently
prop_diff_serialize :: TestVote 'PreVote -> Bool
prop_diff_serialize prevote
  = CBOR.serialise prevote /= CBOR.serialise precommit
  where
    precommit = coerce prevote :: TestVote 'PreCommit

-- CBOR.deserialise . CBOR.serialise
prop_CBOR_roundtrip :: (Eq a, CBOR.Serialise a) => a -> Bool
prop_CBOR_roundtrip a = a == (CBOR.deserialise . CBOR.serialise) a

-- JSON.decode . JSON.encode = id
prop_JSON_roundtrip :: (Eq a, JSON.FromJSON a, JSON.ToJSON a) => a -> Bool
prop_JSON_roundtrip a = Just a == (JSON.decode . JSON.encode) a

-- Read . show = id
prop_ShowRead_roundtrip :: (Eq a, Show a, Read a) => a -> Bool
prop_ShowRead_roundtrip a = a == (read . show) a
