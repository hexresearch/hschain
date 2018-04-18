{-# LANGUAGE DeriveFunctor   #-}
{-# LANGUAGE RecordWildCards #-}
-- |
-- Full blockchain application
module Thundermint.Blockchain.App where

import Control.Monad
import Control.Concurrent.STM
import           Data.Function
import qualified Data.Map        as Map
import           Data.Map          (Map)

import Thundermint.Blockchain.Types
import Thundermint.Crypto
import Thundermint.Consensus.Algorithm
import Thundermint.Consensus.Types
import Thundermint.P2P


----------------------------------------------------------------

data AppChans alg a = AppChans
  { appChanRx          :: TChan (MessageRx alg a)
  , appChanRxBroadcast :: TChan (MessageRx alg a)
  , appChanTx          :: TChan (MessageTx alg a)
  }

-- | Main loop for application. Here we update state machine and
--   blockchain in response to incoming messages.
--
--   * INVARIANT: Only this function can write to blockchain
runApplication
  :: ()
  => AppState alg a
     -- ^ Get initial state of the application
  -> AppChans alg a
  -> IO x
runApplication as ac
  = forever $ heightLoop as ac

-- Loop where we decide which block we need to commit at given height
heightLoop
  :: ()
  => AppState alg a
  -> AppChans alg a
  -> IO ()
heightLoop AppState{..} AppChans{..}
  = loop tmState0
  where
    -- FIXME: We need interpreter for ConsensusM monad. Or do we have
    --        saner implementation
    --
    loop tm = do
      msg <- atomically $ readTChan appChanRx
      case msg of
        RxPreVote   v -> undefined
        RxPreCommit v -> undefined
        RxProposal  p -> undefined
        RxTimeout   t -> undefined
    --
    hParams  = HeightParameres {}
    -- FIXME: we need to construct state and enter arrange state
    --        machine to enter propose immediately!
    tmState0 = undefined


----------------------------------------------------------------
-- State transition in response to message
----------------------------------------------------------------

data ConsensusM alg a x
  = OK x
  | Tranquility
  | Misdeed
    -- Hand rolled free monad
  | DoTimeout       Timeout                       (ConsensusM alg a x)
  | DoCastPrevote   Round (Maybe (BlockID alg a)) (ConsensusM alg a x)
  | DoCastPrecommit Round (Maybe (BlockID alg a)) (ConsensusM alg a x)
  | DoMakeProposal  (BlockID alg a -> ConsensusM alg a x)
  | DoPropose       (BlockID alg a)               (ConsensusM alg a x)
  deriving (Functor)

instance Applicative (ConsensusM alg a) where
  pure  = OK
  (<*>) = ap

instance Monad (ConsensusM alg a) where
  return = OK
  OK a        >>= f = f a
  Tranquility >>= _ = Tranquility
  Misdeed     >>= _ = Misdeed
  --
  DoTimeout t m         >>= f = DoTimeout t         (m >>= f)
  DoCastPrevote r b m   >>= f = DoCastPrevote r b   (m >>= f)
  DoCastPrecommit r b m >>= f = DoCastPrecommit r b (m >>= f)
  DoMakeProposal g      >>= f = DoMakeProposal      (fmap (>>= f) g)
  DoPropose b m         >>= f = DoPropose b         (m >>= f)

instance ConsensusMonad (ConsensusM alg a) where
  tranquility = Tranquility
  misdeed     = Misdeed
  panic       = error
