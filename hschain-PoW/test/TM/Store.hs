-- |
module TM.Store (tests) where

import Control.Monad
import Control.Monad.IO.Class
import Test.Tasty
import Test.Tasty.HUnit

import HSChain.PoW.Consensus
import HSChain.PoW.Types
import HSChain.PoW.Node
import HSChain.Examples.Simple (KV)
import HSChain.Store.Query (withConnection)
import TM.Util.Mockchain

tests :: TestTree
tests = testGroup "Block store"
  [ testCase "idempotence/In-memory" $ testIdempotence =<< inMemoryDB genesis
  , testCase "idempotence/DB"
    $ withConnection "" $ \conn -> runHSChainT conn $ testIdempotence =<< blockDatabase genesis
  ]

testIdempotence :: MonadIO m => BlockDB m (KV MockChain) -> m ()
testIdempotence db = do
  -- Genesis
  fetch "Genesis" genesis
  storeBlock db genesis
  fetch "Genesis" genesis
  -- Rest of blocks
  forM_ (take 3 $ drop 1 mockchain) $ \b -> do
    nofetch (show (blockHeight b)) b
  -- Store and retrieve
  forM_ (take 3 $ drop 1 mockchain) $ \b -> do
    storeBlock db b
    fetch ("Stored: "++show (blockHeight b)) b
  -- Idempotency of store
  forM_ (take 3 $ drop 1 mockchain) $ \b -> do
    storeBlock db b
    fetch ("Idempotence: "++show (blockHeight b)) b
  -- Fetchall
  liftIO . assertEqual "All headers" (take 4 $ map toHeader $ mockchain) =<< retrieveAllHeaders db
  where
    fetch   nm b = liftIO . assertEqual ("Yes: " ++ nm) (Just b)
               =<< retrieveBlock db (blockID b)
    nofetch nm b = liftIO . assertEqual ("No: " ++ nm) (Nothing)
               =<< retrieveBlock db (blockID b)

