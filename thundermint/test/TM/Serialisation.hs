{-# LANGUAGE AllowAmbiguousTypes            #-}
{-# LANGUAGE DataKinds                      #-}
{-# LANGUAGE LambdaCase                     #-}
{-# LANGUAGE RankNTypes                     #-}
{-# LANGUAGE RecordWildCards                #-}
{-# LANGUAGE ScopedTypeVariables            #-}
{-# LANGUAGE TypeApplications               #-}
{-# OPTIONS_GHC -Werror=incomplete-patterns #-}
-- {-# LANGUAGE TemplateHaskell #-}

-- | Tests for some serialisation things
--
module TM.Serialisation (tests) where

import qualified Data.Aeson as JSON
import Data.Functor.Identity
import Data.Typeable
import Data.SafeCopy
import qualified Data.ByteString.Lazy as BSL

import Hedgehog as H
import Hedgehog.Internal.Exception as H
import Test.QuickCheck.Arbitrary
import Test.Tasty
import Test.Tasty.Hedgehog
import qualified Test.Tasty.QuickCheck as QC
import qualified Hedgehog.Gen.QuickCheck as Gen

import Thundermint.Crypto.Ed25519
import Thundermint.Types.Blockchain

import TM.Arbitrary.Instances ()


type TestCryptoAlg     = Ed25519_SHA512
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
    prop :: forall ty . (SafeCopy ty, Show ty, Arbitrary ty) => PropertyT IO ()
    prop = do
        (vote :: ty) <- forAll (Gen.arbitrary)
        H.assert $ not $ BSL.null $ safeEncode vote


prop_can_deserialize :: Property
prop_can_deserialize = property $ do
    prop @TestVotePrevote
    prop @TestVotePrecommit
  where
    prop :: forall ty . (Arbitrary ty, Eq ty, SafeCopy ty, Show ty) => PropertyT IO ()
    prop = do
        (vote :: ty) <- forAll (Gen.arbitrary)
        H.tripping vote (safeEncode) (Identity . either (error "Bade decode") id . safeDecode)


prop_diff_serialize :: Property
prop_diff_serialize = property $ do
    (votePrevote :: TestVotePrevote) <- forAll testVotes
    let votePrecommit = convertToOther @_ @'PreCommit votePrevote
    safeEncode votePrevote /== safeEncode votePrecommit


prop_diff_cant_serialize_to_other :: Property
prop_diff_cant_serialize_to_other  = property $ do
    (votePrevote :: TestVotePrevote) <- forAll testVotes
    either (const success) (const failure) $ tryEvaluate $ 
      let Right (votePrecommit :: TestVotePrecommit) = safeDecode (safeEncode votePrevote)
      in votePrecommit

prop_JSON_roundtrip :: (Eq a, JSON.FromJSON a, JSON.ToJSON a) => a -> Bool
prop_JSON_roundtrip a = Just a == (JSON.decode . JSON.encode) a

tests :: TestTree
tests =
    testGroup "serialisation"
        -- [ testCase "All tests" $ H.checkSequential $$(discover) >>= assertBool "All tests must be OK" ]
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
        , let run :: forall a. (Arbitrary a, Show a, Typeable a, Eq a, JSON.FromJSON a, JSON.ToJSON a)
                  => Proxy a -> TestTree
              run p = QC.testProperty (show (typeRep p)) (prop_JSON_roundtrip @a)
          in testGroup "JSON"
             [ run (Proxy @(Block  TestCryptoAlg Int))
             , run (Proxy @(Header TestCryptoAlg Int))
             , run (Proxy @(Commit TestCryptoAlg Int))
             , run (Proxy @(ByzantineEvidence TestCryptoAlg Int))
             , run (Proxy @(Proposal TestCryptoAlg Int))
             , run (Proxy @(Vote 'PreVote   TestCryptoAlg Int))
             , run (Proxy @(Vote 'PreCommit TestCryptoAlg Int))
             , run (Proxy @Height)
             , run (Proxy @Round)
             ]
        ]
