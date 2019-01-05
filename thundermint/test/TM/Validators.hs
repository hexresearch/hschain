{-# LANGUAGE RecordWildCards #-}
-- |
module TM.Validators (tests) where

import Test.Tasty
import Test.Tasty.HUnit

import Thundermint.Crypto
import Thundermint.Crypto.Ed25519
import Thundermint.Mock.KeyList (privateKeyList)
import Thundermint.Types.Validators


tests :: TestTree
tests = testGroup "validators"
  [ testCase "No change" $ do
      let Right vsetA = makeValidatorSet [Validator v1 1]
      Just vsetA @=? changeValidators [] vsetA
  , testCase "Simple insert" $ do
      let Right vsetA = makeValidatorSet [Validator v1 1]
          Right vsetB = makeValidatorSet [Validator v1 1, Validator v2 1]
      Just vsetB @=? changeValidators [ChangeValidator v2 1] vsetA
  , testCase "Change voting power" $ do
      let Right vsetA = makeValidatorSet [Validator v1 1]
          Right vsetB = makeValidatorSet [Validator v1 2]
      Just vsetB @=? changeValidators [ChangeValidator v1 2] vsetA
  , testCase "Simple delete" $ do
      let Right vsetA = makeValidatorSet [Validator v1 1, Validator v2 2]
          Right vsetB = makeValidatorSet [Validator v1 1]
      Just vsetB @=? changeValidators [RemoveValidator v2] vsetA
    -- Invalid updades
  , testCase "Delete nonexistent" $ invalid
      [Validator v1 1]
      [RemoveValidator v2]
  , testCase "Invalid voting power" $ invalid
      [Validator v1 1]
      [ChangeValidator v2 0]
  , testCase "Double reference (1)" $ invalid [Validator v1 1, Validator v2 1]
      [RemoveValidator v1, RemoveValidator v1]
  , testCase "Double reference (2)" $ invalid [Validator v1 1, Validator v2 1]
      [ChangeValidator v1 2, RemoveValidator v1]
  , testCase "Double reference (3)" $ invalid [Validator v1 1, Validator v2 1]
      [RemoveValidator v1, ChangeValidator v1 2]
  , testCase "Double reference (4)" $ invalid [Validator v1 1, Validator v2 1]
      [ChangeValidator v1 2, ChangeValidator v1 3]
    -- Subtler invalid cases
  , testCase "Can't leave empty validator sets" $ invalid
      [Validator v1 1]
      [RemoveValidator v1]
  , testCase "Noop is error" $ invalid
      [Validator v1 1]
      [ChangeValidator v1 1]
  ]

invalid :: [Validator Ed25519_SHA512] -> [ValidatorChange Ed25519_SHA512] -> IO ()
invalid vals changes = do
  let Right vset = makeValidatorSet vals
  Nothing @=? changeValidators changes vset


v1,v2 :: PublicKey Ed25519_SHA512
v1:v2:_ = map publicKey privateKeyList
