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
    -- * DB persisted state
  , PersistentState(..)
  ) where

import Codec.Serialise (Serialise)
import Control.Concurrent.MVar
import Control.Monad (when)
import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.Fail
import Data.SafeCopy (SafeCopy)

import Thundermint.Crypto
import Thundermint.Control
import Thundermint.Store
import Thundermint.Store.SQL
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
data BlockFold s alg a = BlockFold
  { processTx    :: !(CheckSignature -> Height -> Pet (TX a) -> s -> Maybe s)
    -- ^ Try to process single transaction. Nothing indicates that
    --   transaction is invalid. This function will called very
    --   frequently so it need not to perform every check but should
    --   rule out invalid blocks.
    --
    --   FIXME: figure out exact semantics for Height parameter
  , processBlock :: !(CheckSignature -> Block alg a -> s -> Maybe s)
    -- ^ Try to process whole block. Here application should perform
    --   complete validation of block
  , transactionsToBlock :: !(Height -> s -> [Pet (TX a)] -> a)
    -- ^ Create block at given height from list of transactions. Not
    --   input could contain invalid transaction and they must be
    --   filtered out so that block is valid.
  , initialState :: !s
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
  :: (MonadMask m, MonadIO m, MonadDB m alg a, SafeCopy a, Crypto alg, MonadFail m, Serialise s)
  => BlockFold s alg a             -- ^ Updating function
  -> m (BChState m s)
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
            LT -> do Just b <- queryRO $ retrieveBlock h
                     let checkSignature = if succ h == hBlk then CheckSignature else AlreadyChecked
                     checkSignature `seq` case processBlock checkSignature (pet b) s of
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


----------------------------------------------------------------
-- DB persisted state
----------------------------------------------------------------

data PersistentState dct alg a = PersistentState
  { processTxDB           :: !(Height -> Pet (TX a) -> EphemeralQ alg a dct ())
  , processBlockDB        :: !(forall q. (ExecutorRW q, Monad (q dct), MonadFail (q dct))
                          => Block alg a -> q dct ())
  , transactionsToBlockDB :: !(Height -> [Pet (TX a)] -> EphemeralQ alg a dct a)
  , persistedData         :: !(dct Persistent)
  }
