{-# LANGUAGE RecordWildCards #-}
-- |
module TM.Validators (tests) where

import Control.Applicative
import Control.Monad

import Data.IORef

import qualified Data.Map.Strict as Map

import Test.Tasty
import Test.Tasty.HUnit

import Thundermint.Crypto
import Thundermint.Crypto.Ed25519
import Thundermint.Blockchain.Internal.Engine.Types
import Thundermint.Blockchain.Interpretation
import Thundermint.Mock.KeyList (privateKeyList)
import Thundermint.P2P
import Thundermint.P2P.Network
import Thundermint.Run
import Thundermint.Types.Blockchain
import Thundermint.Types.Validators

import TM.Util.Network


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
  , testGroup "handling in gossip"
      [ testCase "adding and removing validators" $ testAddRemValidators
      ]
  ]

invalid :: [Validator Ed25519_SHA512] -> [ValidatorChange Ed25519_SHA512] -> IO ()
invalid vals changes = do
  let Right vset = makeValidatorSet vals
  Nothing @=? changeValidators changes vset


v1,v2 :: PublicKey Ed25519_SHA512
v1:v2:_ = map publicKey privateKeyList

data ValidatorsTestsState = ValidatorsTestsState
  deriving (Show)

data VTSTx = VTSTx
  deriving (Show)

transitions :: IORef () -> BlockFold ValidatorsTestsState alg [VTSTx]
transitions _log = BlockFold
  { processTx           = const process
  , processBlock        = \_ b s0 -> let h = headerHeight $ blockHeader b
                                   in foldM (flip (process h)) s0 (blockData b)
  , transactionsToBlock = \_ ->
      let selectTx _ []     = []
          selectTx c (t:tx) = case processTransaction t c of
                                Nothing -> selectTx c  tx
                                Just c' -> t : selectTx c' tx
      in selectTx
  , initialState        = ValidatorsTestsState
  }
  where
    process (Height 0) t s = processDeposit t s <|> processTransaction t s
    process _          t s = processTransaction t s
    processTransaction VTSTx ValidatorsTestsState = Just ValidatorsTestsState
    processDeposit _ _ = Just ValidatorsTestsState

testAddRemValidators :: IO ()
testAddRemValidators = createTestNetworkWithConfig (error "A") (error "B")
