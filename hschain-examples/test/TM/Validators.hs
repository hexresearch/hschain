{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NumDecimals         #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

-- |
module TM.Validators (tests) where

import Codec.Serialise
import Control.DeepSeq
import Control.Monad
import Control.Monad.Trans.Cont
import Control.Monad.Trans.Class
import Control.Monad.Catch
import Control.Monad.IO.Class
import Data.Aeson (ToJSON)
import Data.Maybe
import Data.Default.Class
import qualified Data.Map.Strict as Map
import Katip (LogEnv)
import GHC.Generics (Generic)

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import Test.Tasty.Runners

import HSChain.Blockchain.Internal.Engine.Types
import HSChain.Control.Class
import HSChain.Crypto
import HSChain.Crypto.Classes.Hash
import HSChain.Crypto.Ed25519 (Ed25519)
import HSChain.Crypto.SHA     (SHA1)
import HSChain.Internal.Types.Consensus
import HSChain.Logger
import HSChain.Mempool
import HSChain.Mock.KeyList (makePrivKeyStream)
import HSChain.Mock.Types
import HSChain.Mock
import HSChain.Monitoring
import HSChain.Run
import HSChain.Store
import HSChain.Types
import HSChain.Types.Merkle.Types
import HSChain.Arbitrary.Instances ()
import qualified HSChain.Network.Mock as P2P
import TM.Util.Network
import TM.Util.MockChain (runHSChainT)

type VSet = ValidatorSet (Alg Tx)


tests :: TestTree
tests = testGroup "validators"
  [ testGroup "validator change"
    [ testProperty "valSet = valSet + mempty" $ \(vset :: VSet) ->
        Just vset == changeValidators mempty vset
    , testProperty "mempty = valSet - valSet" $ \(vset :: VSet) ->
        mempty == validatorsDifference vset vset
    , testProperty "val2 = val1 + (val2 - val1)" $ \(vsetOld :: VSet) vsetNew ->
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
  , testGroup "Valset indexing"
    [ testProperty "Equidistribution" samplingEquidistribution
    , testProperty "Invalid lookups 1" $ \(vset :: ValidatorSet Ed25519) ->
        Nothing == indexByIntervalPoint vset (-1)
    , testProperty "Invalid lookups 2" $ \(vset :: ValidatorSet Ed25519) ->
        Nothing == indexByIntervalPoint vset (totalVotingPower vset)
    ]
  , testGroup "Validator set change"
    [ localOption (1 :: NumThreads) $ testCase "In consensus" testValidatorChange
    ]
  ]


v1,v2 :: PublicKey (Ed25519 :& SHA1)
v1:v2:_ = publicKey <$> makePrivKeyStream 1337

samplingEquidistribution :: ValidatorSet Ed25519 -> Property
samplingEquidistribution vset
  = counterexample (show idx)
  $ all isJust idx
 && and [ validatorVotingPower v == p
        | (i,p) <- Map.toList
                 $ Map.fromListWith (+) [ (fromJust i, 1) | i <- idx ]
        , let Just v = validatorByIndex vset i
        ]
  where
    idx = indexByIntervalPoint vset <$> [ 0 .. totalVotingPower vset - 1 ]


----------------------------------------------------------------
-- Test change of validator sets
--
-- We use very simple blockchain with single transaction
----------------------------------------------------------------

testValidatorChange :: IO ()
testValidatorChange = withTimeOut 20e6 $ do
  evalContT $ do
    net       <- liftIO P2P.newMockNet
    resources <- allocNetwork net (clusterTopology spec) (clusterNodes spec)
    conn:_    <- executeNodeSpec  spec resources
    -- Execute nodes for second time!
    _         <- executeNodeSpec  spec resources
    -- Now test that we have correct validator sets for every height
    liftIO $ runHSChainT @Tx conn $ do
      checkVals valSet0 (Height  1)
      checkVals valSet0 (Height  2)
      checkVals valSet0 (Height  3)
      checkVals valSet3 (Height  4)
      checkVals valSet3 (Height  5)
      checkVals valSet3 (Height  6)
      checkVals valSet6 (Height  7)
      checkVals valSet6 (Height  8)
      checkVals valSet8 (Height  9)
      checkVals valSet8 (Height 10)
  where
    checkVals v0 h = do
      v <- queryRO $ mustRetrieveValidatorSet h
      liftIO $ assertEqual (show h) v0 v
    --
    spec = MockClusterConfig
      { clusterTopology = All2All
      , clusterNodes    = [ NodeSpec (Just $ PrivValidator k) Nothing []
                          | k <- privK
                          ]
      , clusterCfg      =
        let c = def
        in  c { cfgConsensus = ConsensusCfg
                { timeoutNewHeight  = 10
                , timeoutProposal   = (100,500)
                , timeoutPrevote    = (100,500)
                , timeoutPrecommit  = (100,500)
                , timeoutEmptyBlock = 100
                , incomingQueueSize = 10
                }
              } `asTypeOf` c
      , clusterBChData = ()
      }


data Tx = AddVal !(PublicKey (Alg Tx)) !Integer
        | RmVal  !(PublicKey (Alg Tx))
        | Noop
  deriving stock    (Show,Eq,Ord,Generic)
  deriving anyclass (NFData,Serialise,ToJSON)

instance CryptoHashable Tx where
  hashStep = genericHashStep "hschain"

data ValErr = ValErr
  deriving stock    (Show,Generic)
  deriving anyclass (Exception,ToJSON)

instance BlockData Tx where
  type TX       Tx = Tx
  type BChError Tx = ValErr
  type Alg      Tx = Ed25519 :& SHA1
  proposerSelection       = ProposerSelection randomProposerSHA512

privK :: [PrivKey (Alg Tx)]
privK = take 4 $ makePrivKeyStream 1337

pk1,pk2,pk3,pk4 :: PublicKey (Alg Tx)
[pk1,pk2,pk3,pk4] = map publicKey privK

valSet0,valSet3,valSet6,valSet8 :: ValidatorSet (Alg Tx)
Right valSet0 = makeValidatorSet [Validator k 1 | k <- [pk1,pk2        ]]
Right valSet3 = makeValidatorSet [Validator k 1 | k <- [pk1,pk2,pk3    ]]
Right valSet6 = makeValidatorSet [Validator k 1 | k <- [pk1,pk2,pk3,pk4]]
Right valSet8 = makeValidatorSet [Validator k 1 | k <- [    pk2,pk3,pk4]]

inMemoryStateView :: (Monad m) => ValidatorSet (Alg Tx) -> StateView m Tx
inMemoryStateView = make Nothing
  where
    make mh vals = viewSt where
      viewSt = StateView
        { stateHeight   = mh
        , newValidators = vals
        , commitState   = return viewSt
        , stateMempool  = nullMempool
        --
        , validatePropBlock = \b _ -> return $ do
            let tx = merkleValue $ blockData b
            case makeValidatorSet $ process tx $ asValidatorList vals of
              Left  _     -> Left ValErr
              Right vals' -> Right $ make (Just $ blockHeight b) vals'
        , generateCandidate = \NewBlock{..} -> do
            let prop = case newBlockHeight of
                  Height 3 -> AddVal pk3 1
                  Height 6 -> AddVal pk4 1
                  Height 8 -> RmVal  pk1
                  _        -> Noop
                Right vals' = makeValidatorSet $ process prop $ asValidatorList vals
            return ( prop
                   , make (Just newBlockHeight) vals'
                   )
        }

-- We're permissive and allow remove nonexiting validator
process :: Tx -> [Validator (Alg Tx)] -> [Validator (Alg Tx)]
process Noop         = id
process (AddVal k i) = (Validator k i :)
                     . filter ((/=k) . validatorPubKey)
process (RmVal  k)   = filter ((/=k) . validatorPubKey)



----------------------------------------------------------------
-- Rather standard harness for the running nodes
----------------------------------------------------------------

interpretSpec
  :: ( MonadDB m, MonadCached Tx m, MonadFork m, MonadMask m, MonadLogger m
     , MonadTMMonitoring m)
  => Genesis Tx
  -> NodeSpec Tx
  -> BlockchainNet
  -> Configuration Example
  -> AppCallbacks m Tx
  -> m (Connection 'RW, [m ()])
interpretSpec genesis nspec bnet cfg cb = do
  acts  <- runNode cfg NodeDescription
    { nodeValidationKey = nspecPrivKey nspec
    , nodeGenesis       = genesis
    , nodeStateView     = inMemoryStateView $ genesisValSet genesis
    , nodeCallbacks     = cb
    , nodeNetwork       = bnet
    }
  conn <- askConnectionRW
  return (conn, acts)

executeNodeSpec
  :: (MonadIO m, MonadMask m, MonadFork m,  MonadTMMonitoring m)
  => MockClusterConfig Tx ()
  -> [(NodeSpec Tx, BlockchainNet, Connection 'RW, LogEnv)]
  -> ContT r m [Connection 'RW]
executeNodeSpec MockClusterConfig{..} resources = do
  -- Start nodes
  rnodes <- lift $ forM resources $ \(nspec, bnet, conn, _logenv) -> do
    (c, acts) <- runHSChainT conn $ interpretSpec
      genesis
      nspec
      bnet
      clusterCfg
      (callbackAbortAtH (Height 10)) 
    return ( c, runHSChainT conn<$> acts )
  -- Actually run nodes
  lift   $ catchAbort $ runConcurrently $ snd =<< rnodes
  return $ fst <$> rnodes
  where
    genesis = Genesis
      { genesisBlock  = makeGenesis Noop valSet0 valSet0
      , genesisValSet = valSet0
      }
