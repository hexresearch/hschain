{-# LANGUAGE RecordWildCards #-}
-- |
-- This module provides data structures which add assertions to method
-- dictionaries in order to catch bugs early
module HSChain.PoW.Asserts where

import Control.Monad
import Data.Functor.Identity
import HSChain.PoW.Store
import HSChain.PoW.Types

-- | Add assertions to the 'BlockDB' method dictionary. It checks that
--   retrieve functions fetch block\/header with same BID as requested
--   and 'storeBlock' immediately tries to fetch block in order to
--   check it's possible.
--
--   Due to performance impact this function is intended for debugging
assertBlockDB
  :: (Eq (b Identity), BlockData b, Monad m)
  => BlockDB m b -> BlockDB m b
assertBlockDB BlockDB{..} = BlockDB
  { storeBlock     = \b -> do
      storeBlock b
      mb <- retrieveBlock (blockID b)
      when (mb /= Just b) $ error "assertBlockDB.storeBlock: Cannot fetch block back"
  , retrieveBlock  = \bid -> do
      mb <- retrieveBlock bid
      forM_ mb $ \b -> do
        let bid' = blockID b
        when (bid' /= bid) $ error $ unlines
          [ "assertBlockDB.retrieveBlock: BID mimatch"
          , "  expected:" ++ show bid
          , "  got:     " ++ show bid'
          ]
      return mb
  , retrieveHeader = \bid -> do
      mh <- retrieveHeader bid
      forM_ mh $ \h -> do
        let bid' = blockID h
        when (bid' /= bid) $ error $ unlines
          [ "assertBlockDB.retrieveheader: BID mimatch"
          , "  expected:" ++ show bid
          , "  got:     " ++ show bid'
          ]
      return mh
  , ..
  }
