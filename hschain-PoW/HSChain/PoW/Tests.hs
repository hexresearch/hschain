{-# LANGUAGE CPP       #-}
{-# LANGUAGE DataKinds #-}
-- |
-- Generic invariants for writing tests.
module HSChain.PoW.Tests
  ( -- * Test monad
    HSChainT(..)
  , runHSChainT
  , withHSChainT
  , withHSChainTDB
    -- * Generic tests
  , testIdempotence
  ) where

import Control.Monad
import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.Reader
#if !MIN_VERSION_base(4,13,0)
import Control.Monad.Fail
#endif

import HSChain.Control.Class
import HSChain.Logger
import HSChain.PoW.Consensus
import HSChain.PoW.Types
import HSChain.Store.Query
import HSChain.Types.Merkle.Types


----------------------------------------------------------------
-- Test monad
----------------------------------------------------------------

-- | Monad transformer for use in tests. It provides necessary
--   instances for running node. In particular logging is not present.
newtype HSChainT m x = HSChainT (ReaderT (Connection 'RW) m x)
  deriving newtype (Functor,Applicative,Monad,MonadIO,MonadFail)
  deriving newtype (MonadThrow,MonadCatch,MonadMask,MonadFork)
  deriving newtype (MonadReader (Connection 'RW))
  -- HSChain instances
  deriving MonadLogger            via NoLogsT          (HSChainT m)
  deriving (MonadReadDB, MonadDB) via DatabaseByReader (HSChainT m)

-- | Run HSChainT given a connection
runHSChainT :: Connection 'RW -> HSChainT m x -> m x
runHSChainT c (HSChainT m) = do
  runReaderT m c

-- | Run HSChainT using in-memory database.
withHSChainT :: (MonadIO m, MonadMask m) => HSChainT m a -> m a
withHSChainT m = withConnection "" $ \c -> runHSChainT c m

-- | Open connection to database stored in file. If file exists
--   existing database will be used. Mostly useful for debugging when
--   one want to inspect final state of database.
withHSChainTDB :: (MonadIO m, MonadMask m) => FilePath -> HSChainT m a -> m a
withHSChainTDB db m = withConnection db $ \c -> runHSChainT c m


----------------------------------------------------------------
-- Connections
----------------------------------------------------------------

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
