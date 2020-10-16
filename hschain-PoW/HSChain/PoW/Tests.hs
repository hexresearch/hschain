-- |
-- Generic invariants for writing tests.
module HSChain.PoW.Tests
  ( testIdempotence
  ) where

import Control.Monad
import Control.Monad.IO.Class

import HSChain.PoW.Consensus
import HSChain.PoW.Types
import HSChain.Types.Merkle.Types



-- | Test that storage works correctly
testIdempotence
  :: (MonadIO m, BlockData b, Eq (b Proxy), Eq (b Identity), Show (b Proxy), Show (b Identity))
  => [Block b]     -- ^ Chain of blocks. At least 4 block long
  -> BlockDB m b   -- ^ Database object being tested
  -> m ()
testIdempotence chain db = do
  -- Genesis
  fetch "Genesis" (head chain)
  storeBlock db   (head chain)
  fetch "Genesis" (head chain)
  -- Rest of blocks
  forM_ (take 3 $ drop 1 chain) $ \b -> do
    nofetch (show (blockHeight b)) b
  -- Store and retrieve
  forM_ (take 3 $ drop 1 chain) $ \b -> do
    storeBlock db b
    fetch ("Stored: "++show (blockHeight b)) b
  -- Idempotency of store
  forM_ (take 3 $ drop 1 chain) $ \b -> do
    storeBlock db b
    fetch ("Idempotence: "++show (blockHeight b)) b
  -- Fetchall
  liftIO . assertEqual "All headers" (take 4 $ map toHeader $ chain) =<< retrieveAllHeaders db
  where
    fetch nm b = do
      assertEqual ("Yes: " ++ nm) (Just b)            =<< retrieveBlock  db (blockID b)
      assertEqual ("Yes: " ++ nm) (Just (toHeader b)) =<< retrieveHeader db (blockID b)
    nofetch nm b = do
      assertEqual ("No: " ++ nm) (Nothing) =<< retrieveBlock  db (blockID b)
      assertEqual ("No: " ++ nm) (Nothing) =<< retrieveHeader db (blockID b)

assertEqual :: (MonadIO m, Show a, Eq a) => String -> a -> a -> m ()
assertEqual msg expected got
  = when (expected /= got)
  $ error $ unlines [ msg
                    , "  expected: " ++ show expected
                    , "  got:      " ++ show got
                    ]
