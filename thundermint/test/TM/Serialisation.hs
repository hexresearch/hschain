{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}
-- {-# LANGUAGE TemplateHaskell #-}

-- | Tests for some serialisation things
--
module TM.Serialisation (tests) where

import Codec.Serialise as C
import qualified Data.Aeson as JSON
import Data.Functor.Identity
import Data.Typeable
import qualified Data.ByteString.Lazy as BSL

import Hedgehog as H
import Hedgehog.Internal.Exception as H
import Test.QuickCheck.Arbitrary
import Test.Tasty
import Test.Tasty.Hedgehog
import qualified Test.Tasty.QuickCheck as QC
import qualified Hedgehog.Gen.QuickCheck as Gen

import Thundermint.Crypto         ((:&))
import Thundermint.Crypto.Ed25519 (Ed25519)
import Thundermint.Crypto.SHA     (SHA512)
import Thundermint.Types.Blockchain
import Thundermint.P2P.Types      (NetAddr)

import Thundermint.Arbitrary.Instances ()

type TestCryptoAlg     = Ed25519 :& SHA512
type TestBlock         = String
type TestVote ty       = Vote ty TestCryptoAlg TestBlock
type TestVotePrevote   = Vote 'PreVote TestCryptoAlg TestBlock
type TestVotePrecommit = Vote 'PreCommit TestCryptoAlg TestBlock


testVotes :: Gen (TestVote ty)
testVotes = Gen.arbitrary


convertToOther :: TestVote a -> TestVote b
convertToOther Vote{..} = Vote{..}


prop_only_prevote_and_precommit :: Property
prop_only_prevote_and_precommit = property $
    forAll Gen.arbitrary >>= \case
        PreVote   -> success
        PreCommit -> success


prop_can_serialize :: Property
prop_can_serialize = property $ do
    prop @TestVotePrevote
    prop @TestVotePrecommit
  where
    prop :: forall ty . (Serialise ty, Show ty, Arbitrary ty) => PropertyT IO ()
    prop = do
        (vote :: ty) <- forAll (Gen.arbitrary)
        H.assert $ not $ BSL.null $ C.serialise vote


prop_can_deserialize :: Property
prop_can_deserialize = property $ do
    prop @TestVotePrevote
    prop @TestVotePrecommit
  where
    prop :: forall ty . (Arbitrary ty, Eq ty, Serialise ty, Show ty) => PropertyT IO ()
    prop = do
        (vote :: ty) <- forAll (Gen.arbitrary)
        H.tripping vote (C.serialise) (Identity . C.deserialise)


prop_diff_serialize :: Property
prop_diff_serialize = property $ do
    (votePrevote :: TestVotePrevote) <- forAll testVotes
    let votePrecommit = convertToOther @_ @'PreCommit votePrevote
    C.serialise votePrevote /== C.serialise votePrecommit


prop_diff_cant_serialize_to_other :: Property
prop_diff_cant_serialize_to_other  = property $ do
    (votePrevote :: TestVotePrevote) <- forAll testVotes
    let (votePrecommit :: TestVotePrecommit) = C.deserialise (C.serialise votePrevote)
    either (const success) (const failure) $ tryEvaluate votePrecommit

prop_JSON_roundtrip :: (Eq a, JSON.FromJSON a, JSON.ToJSON a) => a -> Bool
prop_JSON_roundtrip a = Just a == (JSON.decode . JSON.encode) a

prop_ShowRead_roundtrip :: (Eq a, Show a, Read a) => a -> Bool
prop_ShowRead_roundtrip a = a == (read . show) a

tests :: TestTree
tests =
    testGroup "serialisation"
        [ testGroup "Vote"
            [ testProperty "only prevote and precommit"
                           prop_only_prevote_and_precommit
            , testProperty "can serialise"
                           prop_can_serialize
            , testProperty "can deserialise"
                           prop_can_deserialize
            , testProperty "serialize result for prevote and precommit differs"
                           prop_diff_serialize
            , testProperty "can't serialize to other"
                           prop_diff_cant_serialize_to_other
            ]
        , let run :: forall a. ( Arbitrary a, Show a, Typeable a, Eq a
                               , JSON.FromJSON a, JSON.ToJSON a)
                  => TestTree
              run = QC.testProperty (show (typeRep (Proxy @a)))
                                    (prop_JSON_roundtrip @a)
          in testGroup "JSON"
             [ run @(Block  TestCryptoAlg Int)
             , run @(Header TestCryptoAlg Int)
             , run @(Commit TestCryptoAlg Int)
             , run @(ByzantineEvidence TestCryptoAlg Int)
             , run @(Proposal TestCryptoAlg Int)
             , run @(Vote 'PreVote   TestCryptoAlg Int)
             , run @(Vote 'PreCommit TestCryptoAlg Int)
             , run @Height
             , run @Round
             , run @Time
             , run @NetAddr
             ]
        , let run :: forall a. (Arbitrary a, Typeable a, Eq a, Show a, Read a)
                  => TestTree
              run = QC.testProperty (show (typeRep (Proxy @a)))
                                    (prop_ShowRead_roundtrip @a)
          in testGroup "Read/Show"
             [ run @Height
             , run @Round
             , run @Time
             , run @NetAddr
             , QC.testProperty "NetAddr tcp://" $ \(a :: NetAddr) ->
                 a == (read . ("tcp://"++) . show) a
             ]
        ]
