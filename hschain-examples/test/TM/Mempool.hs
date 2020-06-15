{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeOperators              #-}
-- |
module TM.Mempool (tests) where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad
import Control.Monad.IO.Class
import Data.List

import Test.Tasty
import Test.Tasty.Hedgehog
import Hedgehog
import Hedgehog.Gen.QuickCheck (arbitrary)
import Hedgehog.Gen            (resize)

import HSChain.Crypto         ((:&))
import HSChain.Crypto.Ed25519 (Ed25519)
import HSChain.Crypto.SHA     (SHA512)
import HSChain.Mempool


tests :: TestTree
tests = testGroup "Mempool"
  [ testProperty "Duplicate TX are discarded" $ propDuplicate
  , testProperty "Mempool works correctly"    $ propSelfCheck
  ]


-- deriving instance MonadMask (TestT m)

----------------------------------------------------------------
-- Interpretation of mempool
--
-- We use very simple model of mempool. Transactions are just Ints and
-- they are valid if they're large then "blockchain" value which is
-- just a simple variable.

-- Create mempool and callback for incrementing blockchain value
createTestMempool :: MonadIO m => m ( IO ()
                                    , (Mempool IO (Ed25519 :& SHA512) Int
                                      , IO ()
                                      ))
createTestMempool = liftIO $ do
  varBch  <- newTVarIO 0
  let checkTx i = do n <- readTVarIO varBch
                     return (i > n)
  mempool <- newMempool checkTx
  return ( atomically $ modifyTVar' varBch (+10)
         , mempool
         )


propSelfCheck :: Property
propSelfCheck = property $ do
  -- Parametsrs
  txsSets <-  zipWith (\n -> map (+n)) (iterate (+10) 0)
          <$> forAll (resize 10 arbitrary)
  -- Create mempool.
  --
  -- NOTE: We may leak threads but during normal execution we will
  --       kill thread. Otherwise it will hopefully be kille by
  --       "blocked indefinitely on MVar" exception
  (commit,(mempool,thread)) <- createTestMempool
  cursor                    <- liftIO $ getMempoolCursor mempool
  tid                       <- liftIO $ forkIO thread
  --
  forM_ txsSets $ \txs -> do
    annotate $ show txs
    -- Add transactions to mempool
    liftIO $ mapM_ (pushTransaction cursor) txs
    liftIO $ commit
    liftIO (mempoolSelfTest mempool) >>= \case
        []   -> success
        errs -> mapM_ annotate errs >> failure
    liftIO $ filterMempool mempool
    liftIO (mempoolSelfTest mempool) >>= \case
        []   -> success
        errs -> mapM_ annotate errs >> failure
  --
  liftIO $ killThread tid

propDuplicate :: Property
propDuplicate = property $ do
  -- Parameters
  txs <- forAll arbitrary
  -- Create mempool and push TX
  (_,(mempool, thread)) <- createTestMempool
  cursor                <- liftIO $ getMempoolCursor mempool
  tid                   <- liftIO $ forkIO thread  
  liftIO $ mapM_ (pushTransaction cursor) txs
  -- TX should be in same order, positive, and duplicates should be removed
  txs' <- liftIO $ peekNTransactions mempool
  unless (txs' == nub (filter (>0) txs)) failure
  liftIO $ killThread tid
