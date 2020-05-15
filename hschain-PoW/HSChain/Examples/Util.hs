{-# LANGUAGE FlexibleContexts #-}
-- |
module HSChain.Examples.Util where

import Control.Monad.IO.Class
import Data.IORef
import qualified Data.Map.Strict as Map

import HSChain.PoW.Types
import HSChain.PoW.Consensus
import HSChain.Types.Merkle.Types
import HSChain.Examples.Simple

-- | Simple in-memory implementation of DB
inMemoryView
  :: (Monad m, BlockData b, Show (BlockID b))
  => (Block b -> s -> Maybe s)  -- ^ Step function 
  -> s                          -- ^ Initial state
  -> BlockID b
  -> StateView m b
inMemoryView step = make (error "No revinding past genesis")
  where
    make previous s bid = view
      where
        view = StateView
          { stateBID    = bid
          , applyBlock  = \b -> case step b s of
              Nothing -> return Nothing
              Just s' -> return $ Just $  make view s' (blockID b)
          , revertBlock = return previous
          , flushState  = return ()
          }

viewKV :: (KVConfig cfg, Monad m) => BlockID (KV cfg) -> StateView m (KV cfg)
viewKV bid = inMemoryView step Map.empty bid
  where
    step b m
      | or [ k `Map.member` m | (k,_) <- txs ] = Nothing
      | otherwise                              = Just $ Map.fromList txs <> m
      where
        txs = merkleValue $ kvData $ blockData b

inMemoryDB
  :: (MonadIO m, MonadIO n, BlockData b)
  => m (BlockDB n b)
inMemoryDB = do
  var <- liftIO $ newIORef Map.empty
  return BlockDB
    { storeBlock     = \b   -> liftIO $ modifyIORef' var $ Map.insert (blockID b) b
    , retrieveBlock  = \bid -> liftIO $ Map.lookup bid <$> readIORef var
    , retrieveHeader = \bid -> liftIO $ fmap toHeader . Map.lookup bid <$> readIORef var
    }
