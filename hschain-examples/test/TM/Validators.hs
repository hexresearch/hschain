{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE FlexibleContexts    #-}
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
import Control.Monad.Trans.State.Strict
import Control.Monad.Trans.Cont
import Control.Monad.Trans.Class
import Control.Monad.Catch
import Control.Monad.IO.Class
import Data.Maybe
import qualified Data.Map.Strict as Map
import Katip (LogEnv)
import GHC.Generics (Generic)

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

import HSChain.Blockchain.Internal.Engine.Types
import HSChain.Control
import HSChain.Crypto
import HSChain.Crypto.Classes.Hash
import HSChain.Crypto.Ed25519 (Ed25519)
import HSChain.Crypto.SHA     (SHA512)
import HSChain.Debug.Trace
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
import qualified HSChain.P2P.Network as P2P


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
    [ testCase "In consensus" testValidatorChange
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
testValidatorChange = do
  evalContT $ do
    resources <- prepareResources spec
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
      { netNodeList = [ NodeSpec (Just $ PrivValidator k) Nothing []
                      | k <- privK]
      , netTopology = All2All
      , netNetCfg   = defCfg
      , netMaxH     = Just $ Height 10
      }

type Alg = Ed25519 :& SHA512

data Tx = AddVal !(PublicKey Alg) !Integer
        | RmVal  !(PublicKey Alg)
        | Noop
  deriving stock    (Show,Eq,Ord,Generic)
  deriving anyclass (NFData,Serialise)

instance CryptoHashable Tx where
  hashStep = genericHashStep "hschain"

instance BlockData Tx where
  type TX               Tx = Tx
  type InterpreterState Tx = ValidatorSet Alg
  blockTransactions        = pure
  logBlockData             = mempty
  proposerSelection        = ProposerSelection randomProposerSHA512

privK :: [PrivKey Alg]
privK = take 4 $ makePrivKeyStream 1337

pk1,pk2,pk3,pk4 :: PublicKey Alg
[pk1,pk2,pk3,pk4] = map publicKey privK

valSet0,valSet3,valSet6,valSet8 :: ValidatorSet Alg
Right valSet0 = makeValidatorSet [Validator k 1 | k <- [pk1,pk2        ]]
Right valSet3 = makeValidatorSet [Validator k 1 | k <- [pk1,pk2,pk3    ]]
Right valSet6 = makeValidatorSet [Validator k 1 | k <- [pk1,pk2,pk3,pk4]]
Right valSet8 = makeValidatorSet [Validator k 1 | k <- [    pk2,pk3,pk4]]

transitions :: BChLogic (StateT (ValidatorSet Alg) Maybe) Alg Tx
transitions = BChLogic
  { processTx     = process
  , processBlock  = process . merkleValue . blockData
  , generateBlock = \nb _ -> case newBlockHeight nb of
      Height 3 -> gen $ AddVal pk3 1
      Height 6 -> gen $ AddVal pk4 1
      Height 8 -> gen $ RmVal  pk1
      _        -> return Noop
  , initialState  = valSet0
  }
  where
    gen tx = tx <$ process tx
    -- We're permissive and allow remove nonexiting validator
    process Noop         = return ()
    process (AddVal k i) = modifyVal $ (Validator k i :)
                                     . filter ((/=k) . validatorPubKey)
    process (RmVal  k)   = modifyVal $ filter ((/=k) . validatorPubKey)
    --
    modifyVal f = do
      vals <- get
      case makeValidatorSet $ f $ asValidatorList vals of
        Right v -> put v
        Left  _ -> empty

runner :: Monad m => Interpreter (StateT (ValidatorSet Alg) Maybe) m Alg Tx
runner = Interpreter $ \(BlockchainState _ vset) m -> return $ do
  (a,vset') <- runStateT m vset
  return (a, BlockchainState vset' vset')



----------------------------------------------------------------
-- Rather standard harness for the running nodes
----------------------------------------------------------------

interpretSpec
  :: ( MonadDB m Alg Tx, MonadFork m, MonadMask m, MonadLogger m
     , MonadTrace m, MonadTMMonitoring m
     , Has x BlockchainNet
     , Has x NodeSpec
     , Has x (Configuration Example))
  => (Block Alg Tx, ValidatorSet Alg)
  -> x
  -> AppCallbacks m Alg Tx
  -> m (RunningNode m Alg Tx, [m ()])
interpretSpec genesis p cb = do
  conn  <- askConnectionRO
  store <- newSTMBchStorage $ initialState transitions
  logic <- makeAppLogic store transitions runner
  acts  <- runNode (getT p :: Configuration Example) NodeDescription
    { nodeValidationKey = p ^.. nspecPrivKey
    , nodeGenesis       = genesis
    , nodeCallbacks     = cb
    , nodeLogic         = logic
    , nodeNetwork       = getT p
    }
  return
    ( RunningNode { rnodeState   = store
                  , rnodeConn    = conn
                  , rnodeMempool = appMempool logic
                  }
    , acts
    )


prepareResources
  :: (MonadIO m, MonadMask m)
  => NetSpec NodeSpec -> ContT r m [(BlockchainNet :*: NodeSpec, (Connection 'RW alg a, LogEnv))]
prepareResources NetSpec{..} = do
  -- Create mock network and allocate DB handles for nodes
  net <- liftIO P2P.newMockNet
  traverse (\x -> do { r <- allocNode x; return (x,r)})
    $ allocateMockNetAddrs net netTopology
    $ netNodeList


executeNodeSpec
  :: (MonadIO m, MonadMask m, MonadFork m, MonadTrace m, MonadTMMonitoring m)
  => NetSpec NodeSpec
  -> [(BlockchainNet :*: NodeSpec, (Connection 'RW Alg Tx, LogEnv))]
  -> ContT r m [RunningNode m Alg Tx]
executeNodeSpec NetSpec{..} resources = do
  -- Start nodes
  rnodes    <- lift $ forM resources $ \(x, (conn, logenv)) -> do
    let run :: DBT 'RW Alg Tx (LoggerT m) x -> m x
        run = runLoggerT logenv . runDBT conn
    (rn, acts) <- run $ interpretSpec (genesis,valSet0) (netNetCfg :*: x)
      (maybe mempty callbackAbortAtH netMaxH)
    return ( hoistRunningNode run rn
           , run <$> acts
           )
  -- Actually run nodes
  lift   $ catchAbort $ runConcurrently $ snd =<< rnodes
  return $ fst <$> rnodes
  where
    genesis = makeGenesis Noop (hashed valSet0) valSet0 valSet0
