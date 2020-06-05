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
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

-- |
module TM.Validators (tests) where

import Codec.Serialise
import Control.DeepSeq
import Control.Applicative
import Control.Monad
import Control.Monad.Trans.Cont
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Control.Monad.Catch
import Control.Monad.IO.Class
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
import HSChain.Crypto.SHA     (SHA512)
import HSChain.Logger
import HSChain.Mock.KeyList (makePrivKeyStream)
import HSChain.Mock.Types
import HSChain.Mock
import HSChain.Monitoring
import HSChain.Run
import HSChain.Store
import HSChain.Store.STM
import HSChain.Types
import HSChain.Types.Merkle.Types
import HSChain.Arbitrary.Instances ()
import qualified HSChain.Network.Mock as P2P
import TM.Util.Network

type VSet = ValidatorSet (Ed25519 :& SHA512)

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


v1,v2 :: PublicKey (Ed25519 :& SHA512)
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
    resources <- allocNetwork net (netTopology spec) (netNodeList spec)
    nodes     <- executeNodeSpec  spec resources
    -- Execute nodes for second time!
    _         <- executeNodeSpec  spec resources
    -- Now test that we have correct validator sets for every height
    let conn = rnodeConn $ head nodes
    liftIO $ runDBT conn $ do
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
    spec = NetSpec
      { netNodeList = [ NodeSpec (Just $ PrivValidator k) Nothing [] Nothing
                      | k <- privK]
      , netTopology = All2All
      , netNetCfg   =
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
      , netMaxH     = Just $ Height 10
      }


data Tx = AddVal !(PublicKey (Alg Tx)) !Integer
        | RmVal  !(PublicKey (Alg Tx))
        | Noop
  deriving stock    (Show,Eq,Ord,Generic)
  deriving anyclass (NFData,Serialise)

instance CryptoHashable Tx where
  hashStep = genericHashStep "hschain"

data ValErr = ValErr
  deriving stock    (Show)
  deriving anyclass (Exception)

instance BlockData Tx where
  type TX              Tx = Tx
  type BlockchainState Tx = ValidatorSet (Ed25519 :& SHA512)
  type BChError        Tx = ValErr
  type BChMonad        Tx = Maybe
  type Alg             Tx = Ed25519 :& SHA512
  bchLogic                = transitions
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

transitions :: BChLogic Maybe Tx
transitions = BChLogic
  { processTx     = \_ -> empty
  --
  , processBlock  = \BChEval{..} -> fmap (() <$)
                                  $ gen (merkleValue validatorSet)
                                  $ merkleValue $ blockData bchValue
  --
  , generateBlock = \NewBlock{..} _ -> case newBlockHeight of
      Height 3 -> gen newBlockValSet $ AddVal pk3 1
      Height 6 -> gen newBlockValSet $ AddVal pk4 1
      Height 8 -> gen newBlockValSet $ RmVal  pk1
      _        -> gen newBlockValSet $ Noop
  }
  where
    gen vals tx = do
      let adjustment = process tx
      valSet' <- case makeValidatorSet $ adjustment $ asValidatorList vals of
                   Right v -> return v
                   Left  _ -> empty
      return BChEval { bchValue        = tx
                     , blockchainState = merkled valSet'
                     , validatorSet    = merkled valSet'
                     }
    -- We're permissive and allow remove nonexiting validator
    process Noop         = id
    process (AddVal k i) = (Validator k i :)
                         . filter ((/=k) . validatorPubKey)
    process (RmVal  k)   = filter ((/=k) . validatorPubKey)



----------------------------------------------------------------
-- Rather standard harness for the running nodes
----------------------------------------------------------------

interpretSpec
  :: ( MonadDB m Tx, MonadFork m, MonadMask m, MonadLogger m
     , MonadTMMonitoring m)
  => Genesis Tx
  -> NodeSpec Tx
  -> BlockchainNet
  -> Configuration Example
  -> AppCallbacks m Tx
  -> m (RunningNode m Tx, [m ()])
interpretSpec genesis nspec bnet cfg cb = do
  conn  <- askConnectionRO
  store <- newSTMBchStorage $ blockchainState genesis
  let astore = AppStore { appBchState = store
                        , appMempool  = nullMempool
                        }
  acts  <- runNode cfg NodeDescription
    { nodeValidationKey = nspecPrivKey nspec
    , nodeGenesis       = genesis
    , nodeCallbacks     = cb
    , nodeRunner        = maybe (throwE ValErr) return

    , nodeStore         = astore
    , nodeNetwork       = bnet
    }
  return
    ( RunningNode { rnodeState   = store
                  , rnodeConn    = conn
                  , rnodeMempool = appMempool astore
                  }
    , acts
    )


executeNodeSpec
  :: (MonadIO m, MonadMask m, MonadFork m,  MonadTMMonitoring m)
  => NetSpec (NodeSpec Tx)
  -> [(NodeSpec Tx, BlockchainNet, Connection 'RW Tx, LogEnv)]
  -> ContT r m [RunningNode m Tx]
executeNodeSpec NetSpec{..} resources = do
  -- Start nodes
  rnodes    <- lift $ forM resources $ \(nspec, bnet, conn, logenv) -> do
    let run :: DBT 'RW Tx (LoggerT m) x -> m x
        run = runLoggerT logenv . runDBT conn
    (rn, acts) <- run $ interpretSpec
      BChEval { bchValue        = genesis
              , validatorSet    = merkled valSet0
              , blockchainState = merkled valSet0
              }
      nspec
      bnet
      netNetCfg
      (maybe mempty callbackAbortAtH netMaxH)
    return ( hoistRunningNode run rn
           , run <$> acts
           )
  -- Actually run nodes
  lift   $ catchAbort $ runConcurrently $ snd =<< rnodes
  return $ fst <$> rnodes
  where
    genesis = makeGenesis Noop (hashed valSet0) valSet0 valSet0
