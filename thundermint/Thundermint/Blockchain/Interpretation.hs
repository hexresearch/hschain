{-# LANGUAGE BangPatterns    #-}
{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies    #-}
-- |
-- Encoding of application-specific logic for blockchain
module Thundermint.Blockchain.Interpretation (
    -- * Pure state
    BlockFold(..)
  , BChState(..)
  , newBChState
  , hoistBChState
  ) where

import Control.Concurrent.MVar
import Control.Monad (when,(<=<))
import Control.Monad.Catch
import Control.Monad.IO.Class

import Thundermint.Crypto
import Thundermint.Control
import Thundermint.Exceptions
import Thundermint.Store
import Thundermint.Types.Blockchain



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
data BlockFold alg a = BlockFold
  { processTx    :: !(CheckSignature -> Height -> TX a -> BlockchainState a -> Maybe (BlockchainState a))
    -- ^ Try to process single transaction. Nothing indicates that
    --   transaction is invalid. This function will called very
    --   frequently so it need not to perform every check but should
    --   rule out invalid blocks.
    --
    --   FIXME: figure out exact semantics for Height parameter
  , processBlock :: !(CheckSignature -> Block alg a -> BlockchainState a -> Maybe (BlockchainState a))
    -- ^ Try to process whole block. Here application should perform
    --   complete validation of block
  , transactionsToBlock :: !(Height -> BlockchainState a -> [TX a] -> a)
    -- ^ Create block at given height from list of transactions. Not
    --   input could contain invalid transaction and they must be
    --   filtered out so that block is valid.
  , initialState :: !(BlockchainState a)
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
  :: (MonadMask m, MonadIO m, MonadDB m alg a, BlockData a, Crypto alg)
  => BlockFold alg a             -- ^ Updating function
  -> m (BChState m (BlockchainState a))
newBChState BlockFold{..} = do
  maybeState <- queryRO retrieveSavedState
  state <- liftIO $ newMVar $ case maybeState of
    Just (h, s) -> (h,        s)
    _ ->           (Height 0, initialState)
  let ensureHeight hBlk = do
        (st,flt) <- modifyMVarM state $ \st@(h,s) ->
          case h `compare` hBlk of
            GT -> error "newBChState: invalid parameter"
            EQ -> return (st, (s,False))
            LT -> do b <- throwNothing (DBMissingBlock h) <=< queryRO
                        $ retrieveBlock h
                     let !checkSignature | succ h == hBlk = CheckSignature
                                         | otherwise      = AlreadyChecked
                     case processBlock checkSignature b s of
                       Just st' -> do
                         let h' = succ h
                         let Height hToCheck = h
                         when (mod hToCheck 200 == 0) $ do
                            _ <- queryRW $ storeStateSnapshot h' st'
                            return ()
                         return ((h', st'), (st',True))
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
