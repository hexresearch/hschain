{-# LANGUAGE AllowAmbiguousTypes            #-}
{-# LANGUAGE DataKinds                      #-}
{-# LANGUAGE LambdaCase                     #-}
{-# LANGUAGE RankNTypes                     #-}
{-# LANGUAGE RecordWildCards                #-}
{-# LANGUAGE ScopedTypeVariables            #-}
{-# LANGUAGE TypeApplications               #-}
{-# OPTIONS_GHC -Werror=incomplete-patterns #-}


-- | Tests for some serialisation things
--
module TM.Serialisation (tests) where


import Codec.Serialise as C
import Data.Functor.Identity
import qualified Data.ByteString.Lazy as BSL

import Hedgehog as H
import Hedgehog.Internal.Exception as H
import Test.QuickCheck.Arbitrary
import Test.Tasty
import Test.Tasty.Hedgehog
import qualified Hedgehog.Gen.QuickCheck as Gen

import Thundermint.Consensus.Types

import TM.Arbitrary.Instances ()


data TestCryptoAlg
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
        ]
