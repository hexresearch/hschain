{-# LANGUAGE LambdaCase    #-}
{-# LANGUAGE TypeOperators #-}
-- |
module TM.Mempool (tests) where

import Control.Concurrent.STM
import Control.Monad
import Control.Monad.IO.Class
import Data.List

import Test.Tasty
import Test.Tasty.Hedgehog
import Hedgehog
import Hedgehog.Gen.QuickCheck (arbitrary)

import Thundermint.Crypto         ((:&), petrify, pet)
import Thundermint.Crypto.Ed25519 (Ed25519)
import Thundermint.Crypto.SHA     (SHA512)
import Thundermint.Store
import Thundermint.Store.STM
import TM.Arbitrary.Instances ()


tests :: TestTree
tests = testGroup "Mempool"
  [ testProperty "Duplicate TX are discarded" $ propDuplicate
  , testProperty "Mempool works correctly"    $ propSelfCheck
  ]


----------------------------------------------------------------
-- Interpretation of mempool
--
-- We use very simple model of mempool. Transactions are just Ints and
-- they are valid if they're large then "blockchain" value which is
-- just a simple variable.

-- Create mempool and callback for incrementing blockchain value
createTestMempool :: MonadIO m => m (m (), Mempool m (Ed25519 :& SHA512) Int)
createTestMempool = do
  varBch  <- liftIO $ newTVarIO 0
  let checkTx i = liftIO $ do n <- readTVarIO varBch
                              return (pet i > n)
  mempool <- newMempool checkTx
  return ( liftIO $ atomically $ modifyTVar' varBch (+10)
         , mempool
         )


propSelfCheck :: Property
propSelfCheck = property $ do
  -- Parametsrs
  txsSets <-  zipWith (\off -> map (petrify . (+off)))
              (iterate (+10) 0)
          <$> forAll arbitrary
  -- Create mempool
  (commit,mempool) <- createTestMempool
  cursor           <- getMempoolCursor mempool
  --
  forM_ txsSets $ \txs -> do
    annotate $ show txs
    -- Add transactions to mempool
    mapM_ (pushTransaction cursor) txs
    commit
    mempoolSelfTest mempool >>= \case
        []   -> success
        errs -> mapM_ annotate errs >> failure
    filterMempool mempool
    mempoolSelfTest mempool >>= \case
        []   -> success
        errs -> mapM_ annotate errs >> failure

--   forM_ txsSets 
propDuplicate :: Property
propDuplicate = property $ do
  -- Parameters
  txs <- forAll arbitrary
  -- Create mempool and push TX
  (_,mempool) <- createTestMempool
  cursor      <- getMempoolCursor mempool
  mapM_ (pushTransaction cursor) txs
  -- TX should be in same order, positive, and duplicates should be removed
  txs' <- peekNTransactions mempool Nothing
  unless (txs' == nub (filter ((>0) . pet) txs)) failure
