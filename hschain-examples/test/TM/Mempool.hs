{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeOperators              #-}
-- |
module TM.Mempool (tests) where

import Control.Concurrent.STM
import Control.Monad
import Control.Monad.IO.Class
import Data.Foldable
import Data.List
import Data.Maybe

import Test.Tasty
import Test.Tasty.Hedgehog
import Hedgehog
import Hedgehog.Gen.QuickCheck (arbitrary)
import Hedgehog.Gen            (resize)

import HSChain.Crypto         (CryptoHashable)
import HSChain.Crypto.SHA     (SHA512)
import HSChain.Types.Merkle.Types
import HSChain.Mempool


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
createTestMempool :: MonadIO m => m ( IO (), Int -> IO Bool
                                    )
createTestMempool = liftIO $ do
  varBch  <- newTVarIO 0
  let checkTx i = do n <- readTVarIO varBch
                     return (i > n)
  return ( atomically $ modifyTVar' varBch (+10)
         , checkTx
         )


propSelfCheck :: Property
propSelfCheck = property $ do
  -- Parametsrs
  txsSets :: [[Int]] <-  zipWith (\n -> map (+n)) (iterate (+10) 0)
                     <$> forAll (resize 10 arbitrary)
  -- Create mempool.
  (commit,checkTx) <- createTestMempool
  let selfTest m = case mempoolSelfTest m of
                     []   -> success
                     errs -> mapM_ annotate errs >> failure
  let commitTxSet m txs = do
        annotate $ show txs
        -- Add transactions to mempool 
        m' <- liftIO $ foldM (doPush checkTx) m txs
        liftIO $ commit
        selfTest m'
        -- Filter mempool
        m'' <- liftIO $ mempoolFilterTX checkTx m'
        selfTest m''
        return m''
  void $ foldM commitTxSet emptyMempoolState txsSets

propDuplicate :: Property
propDuplicate = property $ do
  -- Parameters
  txs :: [Int] <- forAll arbitrary
  -- Create mempool and push TX
  (_,checkTx) <- createTestMempool
  mempool     <- liftIO $ foldM (doPush checkTx) emptyMempoolState txs
  -- TX should be in same order, positive, and duplicates should be removed
  unless (toList mempool == nub (filter (>0) txs)) failure

doPush
  :: (MonadIO m, CryptoHashable tx)
  => (tx -> m Bool)
  -> MempoolState SHA512 tx
  -> tx
  -> m (MempoolState SHA512 tx)
doPush f m tx
  = fromMaybe m <$> mempoolAddTX (const True) f (merkled tx) m

