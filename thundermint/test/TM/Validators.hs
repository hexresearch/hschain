{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}

-- |
module TM.Validators (tests) where

import Codec.Serialise
import Control.Applicative
import Control.Concurrent.MVar
import Control.Monad
import Control.Monad.Catch
import Control.Monad.Fail
import Control.Monad.IO.Class


import qualified Data.Map.Strict as Map
import qualified Data.Set        as Set

import Data.Typeable (Proxy(..))

import GHC.Generics

import System.IO

import Test.Tasty
import Test.Tasty.HUnit

import Thundermint.Blockchain.Internal.Engine.Types
import Thundermint.Blockchain.Interpretation
import Thundermint.Control
import Thundermint.Crypto
import Thundermint.Crypto.Ed25519
import Thundermint.Debug.Trace
import Thundermint.Logger
import Thundermint.Mock.Coin
import Thundermint.Mock.KeyList (privateKeyList)
import Thundermint.Mock.KeyVal (genesisBlock)
import Thundermint.Mock.Types
import Thundermint.Monitoring
import Thundermint.P2P
import Thundermint.P2P.Network
import Thundermint.P2P.Types
import Thundermint.Run
import Thundermint.Store
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
  deriving (Show, Generic)

deriving instance Serialise ValidatorsTestsState

data VTSTx = VTSTx Height Int
  deriving (Eq, Ord, Show, Generic)

deriving instance Serialise VTSTx

testAddRemValidators :: IO ()
testAddRemValidators = do
  net  <- liftIO newMockNet
  summary <- newMVar Set.empty
  withMany (\descr cont -> withConnection ":memory:" (\c -> cont (c,descr))) desc $ \descrList -> do
    acts <- mapM (mkTestNode net summary) descrList
    catchAbort $ runConcurrently $ join acts
  events <- takeMVar summary
  putStrLn $ "events: "++ show events
  where
    catchAbort act = catch act (\Abort -> return ())
    transitions :: BlockFold ValidatorsTestsState alg [VTSTx]
    transitions = BlockFold
      { processTx           = const process
      , processBlock        = \_ b s0 -> let h = headerHeight $ blockHeader b
                                       in foldM (flip (process h)) s0 (blockData b)
      , transactionsToBlock = \h ->
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
        processTransaction (VTSTx _ _) ValidatorsTestsState = Just ValidatorsTestsState
        processDeposit _ _ = Just ValidatorsTestsState

    cfg = (defCfg :: Configuration Example)
      { cfgConsensus         = ConsensusCfg
        { timeoutNewHeight   = (5, 5)
        , timeoutProposal    = (5, 5)
        , timeoutPrevote     = (5, 5)
        , timeoutPrecommit   = (5, 5)
        , timeoutEmptyBlock  = 10
        , incomingQueueSize  = 7
        }
      }

    extendedListOfValidators = Map.elems testValidators ++ Map.elems extraTestValidators
    nodesIndices = [1 .. Map.size testValidators + 1]
    desc = map mkTestNetLinkDescription (zip nodesIndices extendedListOfValidators)
    extraValidatorPubKey = publicKey $ validatorPrivKey $ snd $ last desc
    mkTestNetLinkDescription (i, pk) = (TestNetLinkDescription i (filter (/=i) nodesIndices) (const $ return ()), pk)

    mkTestNode
      :: (Functor m, MonadMask m, MonadFork m, MonadFail m, MonadTMMonitoring m)
      => MockNet
      -> MVar (Set.Set (Int, VTSTx))
      -> (Connection 'RW Ed25519_SHA512 [VTSTx], (TestNetLinkDescription m, PrivValidator Ed25519_SHA512))
      -> m [m ()]
    mkTestNode net summary (conn, (TestNetLinkDescription{..}, privKey)) = do
        let validatorSet = makeValidatorSetFromPriv testValidators
            nodeIndex = ncFrom
        initDatabase conn Proxy (makeGenesis "TESTVALS" (Time 0) [] validatorSet) validatorSet
        --
        let run = runTracerT ncCallback . runNoLogsT . runDBT conn
        fmap (map run) $ run $ do
            (_,generatedLogic) <- logicFromFold transitions
            let logic = NodeLogic {
                  nodeMempool = nodeMempool generatedLogic
                , nodeCommitQuery = nodeCommitQuery generatedLogic
                , nodeBlockGenerator = \height time maybeCommit byzantineEvidence -> do
                  nodeBlockGenerator generatedLogic height time maybeCommit byzantineEvidence
                , nodeBlockValidation = \block -> do
                  changes <- nodeBlockValidation generatedLogic block
                  let h = headerHeight $ blockHeader block
                  case h of
                    Height 20 -> return $ fmap (ChangeValidator extraValidatorPubKey 10 :) changes
                    Height 40 -> return $ fmap (RemoveValidator extraValidatorPubKey :) changes
                    _         -> return changes
                }
                memPool = nodeMempool logic
            runNode cfg
                BlockchainNet
                  { bchNetwork          = createMockNode net (intToNetAddr ncFrom)
                  , bchLocalAddr        = intToNetAddr ncFrom
                  , bchInitialPeers     = map intToNetAddr ncTo
                  }
                NodeDescription
                  { nodeCommitCallback   = \block -> do
                    let h = headerHeight $ blockHeader block
                    liftIO $ putStrLn $ "Height "++show h++" at "++show nodeIndex
                    when (h > Height 60) $ throwM Abort
                    cursor <- getMempoolCursor memPool
                    pushTransaction cursor (VTSTx h nodeIndex)
                    liftIO $ modifyMVar_ summary $
                      \s -> return $ Set.union s $ Set.fromList (map ((,) nodeIndex) $ blockData block)
                    return ()
                  , nodeValidationKey    = Just privKey
                  , nodeReadyCreateBlock = \_ _ -> return True
                  }
                logic


