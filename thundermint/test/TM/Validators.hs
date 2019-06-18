{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE TypeOperators       #-}

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

import Data.Typeable (Proxy(..))

import GHC.Generics

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

import Thundermint.Blockchain.Internal.Engine.Types
import Thundermint.Blockchain.Interpretation
import Thundermint.Control
import Thundermint.Crypto
import Thundermint.Crypto.Ed25519 (Ed25519)
import Thundermint.Crypto.SHA     (SHA512)
import Thundermint.Debug.Trace
import Thundermint.Logger
import Thundermint.Mock.Coin
import Thundermint.Mock.KeyList (privateKeyList)
import Thundermint.Mock.Types
import Thundermint.Monitoring
import Thundermint.P2P.Network
import Thundermint.Run
import Thundermint.Store
import Thundermint.Types.Blockchain
import Thundermint.Types.Validators
import Thundermint.Arbitrary.Instances

import TM.Util.Network


type VSet = ValidatorSet (Ed25519 :& SHA512)

tests :: TestTree
tests = testGroup "validators"
  [ testProperty "No change 1" $ \(vset :: VSet) ->
      Just vset == changeValidators mempty vset
  , testProperty "No change 2" $ \(vset :: VSet) ->
      mempty == validatorsDifference vset vset
  , testProperty "Roundtrip" $ \(vsetOld :: VSet) vsetNew ->
      let diff = validatorsDifference vsetOld vsetNew
      in Just vsetNew == changeValidators diff vsetOld
  , testProperty "Compose" $ \(vset1 :: VSet) vset2 vset3 ->
      let diff1 = validatorsDifference vset1 vset2
          diff2 = validatorsDifference vset2 vset3
      in id $ counterexample ("diff1 = " ++ show diff1)
            $ counterexample ("diff2 = " ++ show diff2)
            $ counterexample ("union = " ++ show (diff1 <> diff2))
            $ Just vset3 == changeValidators (diff1 <> diff2) vset1
    --
  , testCase "Delete nonexistent" $ do
      let Right vset = makeValidatorSet [Validator v1 1]
          diff       = ValidatorChange $ Map.fromList [(v2,0)]
      Just vset @=? changeValidators diff vset
  , testCase "Attempt to make noop" $ do
      let Right vset = makeValidatorSet [Validator v1 1]
          diff       = ValidatorChange $ Map.fromList [(v1,1)]
      Nothing @=? changeValidators diff vset
  , testCase "Invalid voting power 1" $ do
      let Right vset = makeValidatorSet [Validator v1 1]
          diff       = ValidatorChange $ Map.fromList [(v1, -1)]
      Nothing @=? changeValidators diff vset
  , testCase "Invalid voting power 2" $ do
      let Right vset = makeValidatorSet [Validator v1 1]
          diff       = ValidatorChange $ Map.fromList [(v2, -1)]
      Nothing @=? changeValidators diff vset
  ]

v1,v2 :: PublicKey (Ed25519 :& SHA512)
v1:v2:_ = map publicKey privateKeyList


{-
  -- , testGroup "handling in gossip"
  --     [ testCase "adding and removing validators" $ testAddRemValidators
  --     ]


invalid :: [Validator (Ed25519 :& SHA512)] -> [ValidatorChange (Ed25519 :& SHA512)] -> IO ()
invalid vals changes = do
  let Right vset = makeValidatorSet vals
  Nothing @=? changeValidators changes vset

data ValidatorsTestsState = ValidatorsTestsState
  deriving (Show, Generic)

deriving instance Serialise ValidatorsTestsState

data VTSTx = Add | Del | OriginMark Int
  deriving (Eq, Ord, Show, Generic)

deriving instance Serialise VTSTx
-}

{-
testAddRemValidators :: IO ()
testAddRemValidators = do
  net  <- liftIO newMockNet
  summary <- newMVar False
  withMany (\descr cont -> withConnection ":memory:" (\c -> cont (c,descr))) desc $ \descrList -> do
    acts <- mapM (mkTestNode net summary) descrList
    catchAbort $ runConcurrently $ join acts
  hasBlockFromDynamicOne <- takeMVar summary
  when (not hasBlockFromDynamicOne) $ error "failed to have block from dynamic validator"
  where
    catchAbort act = catch act (\Abort -> return ())
    testTransitions :: BlockFold ValidatorsTestsState alg [VTSTx]
    testTransitions = BlockFold
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
        processTransaction _ ValidatorsTestsState = Just ValidatorsTestsState
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

    testValidatorsCount = 10
    allValidatorsList = take testValidatorsCount $ Map.toList testValidators ++ Map.toList extraTestValidators
    ((_, PrivValidator dynamicValidatorPrivKey) : initialValidatorsList) = allValidatorsList
    dynamicValidatorPubKey = publicKey dynamicValidatorPrivKey
    dynamicValidatorIndex = 1
    initialValidators = Map.fromList $ tail initialValidatorsList
    nodesIndices = [1 .. testValidatorsCount]
    desc = map mkTestNetLinkDescription (zip [1..] $ map snd allValidatorsList)
    mkTestNetLinkDescription (i, pk) = (TestNetLinkDescription i (filter (/=i) nodesIndices) (const $ return ()), pk)
    validatorSet = makeValidatorSetFromPriv initialValidators

    enableHeight = Height 20
    disableHeight = Height 40

    mkTestNode
      :: (Functor m, MonadMask m, MonadFork m, MonadFail m, MonadTMMonitoring m)
      => MockNet
      -> MVar (Bool)
      -> (Connection 'RW (Ed25519 :& SHA512) [VTSTx], (TestNetLinkDescription m, PrivValidator (Ed25519 :& SHA512)))
      -> m [m ()]
    mkTestNode net summary (conn, (TestNetLinkDescription{..}, privKey)) = do
        let nodeIndex = ncFrom
        initDatabase conn Proxy (makeGenesis "TESTVALS" (Time 0) [] validatorSet) validatorSet
        --
        let run = runTracerT ncCallback . runNoLogsT . runDBT conn
        fmap (map run) $ run $ do
            (_,generatedLogic) <- logicFromFold testTransitions
            let logic = NodeLogic {
                  nodeMempool = nodeMempool generatedLogic
                , nodeCommitQuery = case nodeCommitQuery generatedLogic of
                      SimpleQuery cb -> SimpleQuery $ \vset block ->
                          let addChanges = case (elem Add $ blockData block, elem Del $ blockData block) of
                                (True, False) -> [ChangeValidator dynamicValidatorPubKey 10]
                                (False, True) -> [RemoveValidator dynamicValidatorPubKey]
                                _ -> []
                          in (addChanges ++) <$> cb vset block
                      MixedQuery _ -> error "mixed query in test!"
                , nodeBlockGenerator = \height time maybeCommit byzantineEvidence vSet -> do
                  (block', changes) <- nodeBlockGenerator generatedLogic height time maybeCommit byzantineEvidence vSet
                  let block = OriginMark nodeIndex : block'
                  case height of
                    Height 20 -> return (Add : block, ChangeValidator dynamicValidatorPubKey 10 : changes)
                    Height 40 -> return (Del : block, RemoveValidator dynamicValidatorPubKey : changes)
                    _         -> return (block, changes)
                , nodeBlockValidation = \valSet block -> do
                    let header = blockHeader block
                        txs = blockData block
                        h = headerHeight header
                        blockContainsDynamic = not (null txs) && any (== OriginMark dynamicValidatorIndex) txs
                    when ((h < enableHeight || h > disableHeight) && blockContainsDynamic) $
                      error $ "origin mark is dynamic in a prohibited range.\nnodeIndex "++ show nodeIndex ++ ", height "++show h++", txs "++show txs
                    when (h > Height 60) $ throwM Abort
                    when (nodeIndex < 4) $ liftIO $ modifyMVar_ summary $
                      return . (|| blockContainsDynamic)
                    changes <- nodeBlockValidation generatedLogic valSet block
                    let addChanges = case (elem Add $ blockData block, elem Del $ blockData block) of
                          (True, False) -> [ChangeValidator dynamicValidatorPubKey 10]
                          (False, True) -> [RemoveValidator dynamicValidatorPubKey]
                          _ -> []
                    return $ fmap (addChanges ++) changes
                }
            runNode cfg
                BlockchainNet
                  { bchNetwork          = createMockNode net (intToNetAddr ncFrom)
                  , bchLocalAddr        = intToNetAddr ncFrom
                  , bchInitialPeers     = map intToNetAddr ncTo
                  }
                NodeDescription
                  { nodeValidationKey = Just privKey
                  , nodeCallbacks     = mempty
                  }
                logic


-}
