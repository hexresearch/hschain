{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE TypeFamilies               #-}
-- |
module TM.Coin (tests) where

import Control.Monad.State.Strict
import Control.Monad.Reader
import Test.Tasty
import Test.Tasty.HUnit

import HSChain.Internal.Types.Consensus
import HSChain.Crypto
import HSChain.Types
import HSChain.Mock.Coin
import HSChain.Mock.KeyList
import HSChain.Mock.Types   (makeGenesis)
import HSChain.Types.Merkle.Types

import TM.Util.MockChain (HSChainT, withHSChainT, coinSpec)


tests :: TestTree
tests = testGroup "Coin"
  [ runTestSet "In-memory" runInMemoty
  , runTestSet "Dabase"    (runDatabase True)
  , runTestSet "Dabase"    (runDatabase False)
  ]

runTestSet
  :: (StateView view, ViewConstraints view m, BlockType view ~ BData, Monad m)
  => String -> (TestT view m () -> IO ()) -> TestTree
runTestSet name run = testGroup name
  [ testCase "Single TX"     $ run singleSpend
  , testCase "Bad balance"   $ run badSpend
  , testCase "Bad signature" $ run badSignature
  , testCase "Dense  spend chain"  $ run $ spendChain  True
  , testCase "Sparce spend chain"  $ run $ spendChain  False
  , testCase "Dense  double spend" $ run $ doubleSpend True
  , testCase "Sparce double spend" $ run $ doubleSpend False
  ]

singleSpend,badSpend,badSignature
  :: (StateView view, ViewConstraints view m, BlockType view ~ BData, Monad m)
  => TestT view m ()
-- Simple spending
singleSpend = do
  advance [ signTx sk1 TxSend { txInputs  = [ UTXO 0 (hashed dep1) ]
                              , txOutputs = [ Unspent pk2 55
                                            , Unspent pk3 45
                                            ]
                              } ]
-- Coins are not balanced
badSpend = do
  bad [ signTx sk1 TxSend { txInputs  = [ UTXO 0 (hashed dep1) ]
                          , txOutputs = [ Unspent pk2 55
                                        , Unspent pk3 1000
                                        ]
                          } ]
-- Transaction siganture is not valid
badSignature = do
  bad [ signTx sk2 TxSend { txInputs  = [ UTXO 0 (hashed dep1) ]
                          , txOutputs = [ Unspent pk2 100 ]
                          } ]

-- Transfer coins between accounts. We optionally insert empty block between 
spendChain
  :: (StateView view, ViewConstraints view m, BlockType view ~ BData, Monad m)
  => Bool -> TestT view m ()
spendChain dense = do
  let txA = signTx sk1 TxSend { txInputs  = [ UTXO 0 (hashed dep1) ]
                              , txOutputs = [ Unspent pk2 100 ]
                              }
  advance [ txA ]
  unless dense $ advance []
  let txB = signTx sk2 TxSend { txInputs  = [ UTXO 0 (hashed txA) ]
                              , txOutputs = [ Unspent pk3 100 ]
                              }
  advance [ txB ]
  unless dense $ advance []
  let txC = signTx sk3 TxSend { txInputs  = [ UTXO 0 (hashed txB) ]
                              , txOutputs = [ Unspent pk3 100 ]
                              }
  advance [ txC ]

-- Attempt to double spend coins
doubleSpend
  :: (StateView view, ViewConstraints view m, BlockType view ~ BData, Monad m)
  => Bool -> TestT view m ()
doubleSpend dense = do
  let txA = signTx sk1 TxSend { txInputs  = [ UTXO 0 (hashed dep1) ]
                              , txOutputs = [ Unspent pk2 100 ]
                              }
  advance [ txA ]
  -- First spend
  let txB = signTx sk2 TxSend { txInputs  = [ UTXO 0 (hashed txA) ]
                              , txOutputs = [ Unspent pk3 100 ]
                              }
  advance [txB]
  unless dense $ advance []
  bad [txB]


signTx :: PrivKey (Alg BData) -> TxSend -> Tx
signTx k tx = Send (publicKey k) (signHashed k tx) tx

----------------------------------------------------------------
-- Monad for state tests
----------------------------------------------------------------

newtype TestT view m a = TestT (ReaderT Bool (StateT view m) a)
  deriving newtype (Functor, Applicative, Monad)

runInMemoty :: TestT CoinInMemory IO a -> IO a
runInMemoty (TestT test) = do
  (sv0,_,_) <- inMemoryStateView coinSpec valSet
  Right sv1 <- validatePropBlock sv0 genesis valSet
  evalStateT (runReaderT test False) sv1

runDatabase :: Bool -> TestT CoinDB (HSChainT BData IO) a -> IO a
runDatabase doCommit (TestT test) = withHSChainT $ do
  (sv0, _)  <- databaseStateView coinSpec valSet
  Right sv1 <- validatePropBlock sv0 genesis valSet
  evalStateT (runReaderT test doCommit) sv1

-- Create new block and advance state by one step
advance
  :: (StateView view, ViewConstraints view m, BlockType view ~ BData, Monad m)
  => Monad m => [Tx] -> TestT view m ()
advance txs = mintBlock txs >>= \case
  Left  e  -> error $ show e
  Right () -> return ()

-- Create new block which should not pass validation
bad
  :: (StateView view, ViewConstraints view m, BlockType view ~ BData, Monad m)
  => [Tx] -> TestT view m ()
bad txs = mintBlock txs >>= \case
  Left  _  -> return ()
  Right () -> error "Unexpected success"

mintBlock
  :: (StateView view, ViewConstraints view m, BlockType view ~ BData, Monad m)
  => [Tx] -> TestT view m (Either (BChError BData) ())
mintBlock txs = TestT $ do
  sv <- get
  lift (lift (validatePropBlock sv (newBlock sv) valSet)) >>= \case
    Left  e   -> return $ Left e
    Right sv' -> do
      ask >>= \case
        False -> put sv'
        True  -> (lift . lift) (commitState sv') >>= put
      return $ Right ()
  where
    -- We create new block and fill most of field with junk. State
    -- transitions doesn't check them so there's no need to fake them
    newBlock v = Block
      { blockHeight        = maybe (Height 0) succ $ stateHeight v
      , blockPrevBlockID   = Nothing
      , blockValidators    = hashed valSet
      , blockNewValidators = hashed valSet
      , blockPrevCommit    = Nothing
      , blockEvidence      = merkled []
      , blockData          = merkled $ BData txs
      }

genesis :: Block BData
genesis = makeGenesis (BData deposits) valSet valSet

sk1,sk2,sk3 :: PrivKey   (Alg BData)
pk1,pk2,pk3 :: PublicKey (Alg BData)
(pk1,sk1):(pk2,sk2):(pk3,sk3):_ = map (\k -> (publicKey k, k)) $ makePrivKeyStream 1337

valSet :: ValidatorSet (Alg BData)
Right valSet = makeValidatorSet [ Validator pk1 1 ]

deposits :: [Tx]
dep1     :: Tx
deposits@[dep1] =
  [ Deposit pk1 100
  ]
