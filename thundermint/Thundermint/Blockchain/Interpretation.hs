{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes      #-}
-- |
-- Encoding of application-specific logic for blockchain
module Thundermint.Blockchain.Interpretation (
    BlockFold(..)
  , BChState(..)
  , newBChState
  , hoistBChState
  ) where

import Control.Concurrent.MVar
import Control.Monad.Catch
import Control.Monad.IO.Class

import Thundermint.Control
import Thundermint.Store
import Thundermint.Blockchain.Types



-- | Data structure which encapsulate interpretation of
--   blockchain. Type parameters have following meaning:
--
--   * @s@  is type of state of blockchain
--   * @tx@ is type of single transaction
--   * @a@  is type of block
--
--   Each block in blockchain change current state of blockchain (in
--   case of coins it's amount of coins available for each
--   wallet). Transition functions double as validation functions
--   e.g. if transaction or block is not valid it will return
--   @Nothing@.
data BlockFold s tx a = BlockFold
  { processTx    :: Height -> tx -> s -> Maybe s
    -- ^ Try to process single transaction. Nothing indicates that
    --   transaction is invalid. This function will called very
    --   frequently so it need not to perform every check but should
    --   rule out invalid blocks.
    --
    --   FIXME: figure out exact semantics for Height parameter
  , processBlock :: Height -> a  -> s -> Maybe s
    -- ^ Try to process whole block. Here application should perform
    --   complete validation of block
  , transactionsToBlock :: Height -> s -> [tx] -> a
    -- ^ Create block at given height from list of transactions. Not
    --   input could contain invalid transaction and they must be
    --   filtered out so that block is valid.
  , initialState :: s
    -- ^ State of blockchain BEFORE genesis block.
  }


----------------------------------------------------------------
-- State wrapper
----------------------------------------------------------------

-- | Wrapper for obtaining state of blockchain. It's quite limited
--   calls must be done with in nondecreasing height.
data BChState m s = BChState
  { currentState :: m s
  , stateAtH     :: Height -> m s
  }

-- | Create block storage backed by MVar
newBChState
  :: (MonadMask m, MonadIO m)
  => BlockFold s tx a           -- ^ Updating function
  -> BlockStorage 'RO m alg a  -- ^ Store of blocks
  -> m (BChState m s)
newBChState BlockFold{..} BlockStorage{..} = do
  state <- liftIO $ newMVar (Height 0, initialState)
  let ensureHeight hBlk = do
        (st,flt) <- modifyMVarM state $ \st@(h,s) ->
          case h `compare` hBlk of
            GT -> error "newBChState: invalid parameter"
            EQ -> return (st, (s,False))
            LT -> do Just Block{..} <- retrieveBlock h
                     case processBlock h blockData s of
                       Just st' -> return ((succ h, st'), (st',True))
                       Nothing  -> error "OOPS! Blockchain is not valid!!!"
        case flt of
          True  -> ensureHeight hBlk
          False -> return st
  return BChState
    { currentState = withMVarM state (return . snd)
    , stateAtH     = ensureHeight
    }

hoistBChState :: (forall a . m a -> n a) -> BChState m s -> BChState n s
hoistBChState f BChState{..} = BChState
  { currentState = f currentState
  , stateAtH     = f . stateAtH }
