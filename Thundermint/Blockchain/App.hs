{-# LANGUAGE DeriveFunctor   #-}
{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}
-- |
-- Full blockchain application
module Thundermint.Blockchain.App where

import Control.Monad
import Control.Concurrent
import Control.Concurrent.STM
import           Data.Foldable
import           Data.Function
import qualified Data.Map        as Map
import           Data.Map          (Map)

import Thundermint.Blockchain.Types
import Thundermint.Crypto
import Thundermint.Consensus.Algorithm
import Thundermint.Consensus.Types
import Thundermint.P2P


----------------------------------------------------------------



----------------------------------------------------------------


-- | Main loop for application. Here we update state machine and
--   blockchain in response to incoming messages.
--
--   * INVARIANT: Only this function can write to blockchain
runApplication
  :: (Crypto alg, Serializable a)
  => AppState alg a
     -- ^ Get initial state of the application
  -> AppChans alg a
     -- ^ Channels for communication with peers
  -> IO x
runApplication as ac
  -- NOTE: blockchain state is stored in TVar so there's no need to
  --       thread it explicitly.
  = forever $ heightLoop as ac

-- Loop where we decide which block we need to commit at given height
heightLoop
  :: (Crypto alg, Serializable a)
  => AppState alg a
  -> AppChans alg a
  -> IO ()
heightLoop appSt@AppState{..} appCh@AppChans{..} = do
  h <- readTVarIO appBlockchain >>= \case
    Cons    b _ -> return $ headerHeight $ blockHeader b
    Genesis b   -> return $ headerHeight $ blockHeader b
  loop HeightParameres
    { currentH = h
    , scheduleTimeout = \t -> DoTimeout t (return ())
    , castPrevote     = \r b -> DoCastPrevote r b (return ())
    , castPrecommit   = \r b -> DoCastPrecommit r b (return ())
    , proposeBlock    = \r b -> DoPropose r b (return ())
    , makeProposal    = DoMakeProposal return
      -- FIXME: this is some random algorithms that should probably
      --        work (for some definition of work)
    , areWeProposers  = \(Round r) ->
        let Height h' = h
            n         = Map.size appValidatorsSet
            i         = (h' + r) `mod` fromIntegral n
            addr      = Map.keys appValidatorsSet !! fromIntegral i
        in addr == address (publicKey (validatorPrivKey appValidator))
    , commitBlock     = flip DoCommit
    } tmState0
  where
    loop hParams tm = do
      msg <- atomically $ readTChan appChanRx
      let pkLookup a = validatorPubKey <$> Map.lookup a appValidatorsSet
          verify x cont = case verifySignature pkLookup x of
            Just x' -> cont x'
            Nothing -> loop hParams tm
      let recur m =
            runConsesusM (currentH hParams) appSt appCh (tendermintTransition hParams m tm) >>= \case
              Success tm' -> loop hParams tm'
              Tranquility -> loop hParams tm
              Misdeed     -> loop hParams tm
      case msg of
        RxPreVote   v -> verify v (recur . PreVoteMsg)
        RxPreCommit v -> verify v (recur . PreCommitMsg)
        RxProposal  p -> verify p $ \sp ->
          -- FIXME: we need to separate wheat (GoodProposal) from chaff (InvalidProposal)
          let Proposal{..} = signedValue sp
          in recur $ ProposalMsg propHeight propRound (GoodProposal propBlockID)
        RxTimeout t -> recur (TimeoutMsg t)
    -- FIXME: we need to construct state and arrange state
    --        machine to enter propose immediately!
    tmState0 = undefined


----------------------------------------------------------------
-- State transition in response to message
----------------------------------------------------------------

runConsesusM
  :: (Crypto alg, Serializable a)
  => Height
  -> AppState alg a
  -> AppChans alg a
  -> ConsensusM alg a x
  -> IO (ConsensusResult x)
runConsesusM h appSt@AppState{..} appCh@AppChans{..} = go
  where
    go = \case
      OK x         -> return (Success x)
      TranquilityM -> return Tranquility
      MisdeedM     -> return Misdeed
      --
      DoTimeout t m         -> do void $ forkIO $ do
                                    threadDelay (10*1000*1000)
                                    atomically $ writeTChan appChanRx $ RxTimeout t
                                  go m
      DoCastPrevote r b m   -> do let pk   = validatorPrivKey appValidator
                                      vote = Vote { voteHeight  = h
                                                  , voteRound   = r
                                                  , voteTime    = Time 0
                                                  , voteBlockID = b
                                                  }
                                      svote  = signValue pk vote
                                  atomically $ writeTChan appChanTx (TxPreVote svote)
                                  go m
      DoCastPrecommit r b m -> do let pk   = validatorPrivKey appValidator
                                      vote = Vote { voteHeight  = h
                                                  , voteRound   = r
                                                  , voteTime    = Time 0
                                                  , voteBlockID = b
                                                  }
                                      svote  = signValue pk vote
                                  atomically $ writeTChan appChanTx (TxPreVote svote)
                                  go m
      DoMakeProposal g      -> do p <- atomically appProposalMaker
                                  atomically $ modifyTVar appBlockStore $ \m ->
                                    Map.insert (propBlockID p) (propBlock p) m
                                  go (g (propBlockID p))
      DoPropose    r b m    -> do let pk   = validatorPrivKey appValidator
                                      prop = Proposal { propHeight    = h
                                                      , propRound     = r
                                                      , propTimestamp = Time 0
                                                      , propPOL       = Nothing
                                                      , propBlockID   = b
                                                        -- FIXME: proposal block?
                                                      }
                                      sprop  = signValue pk prop
                                  atomically $ writeTChan appChanTx (TxProposal sprop)
                                  go m
      DoCommit b st         -> undefined -- do atomically $ writeTVar appLastCommit $
                                    

data ConsensusResult a
  = Success a
  | Tranquility
  | Misdeed

data ConsensusM alg a x
  = OK x
  | TranquilityM
  | MisdeedM
    -- Hand rolled free monad
  | DoTimeout       Timeout                       (ConsensusM alg a x)
  | DoCastPrevote   Round (Maybe (BlockID alg a)) (ConsensusM alg a x)
  | DoCastPrecommit Round (Maybe (BlockID alg a)) (ConsensusM alg a x)
  | DoMakeProposal  (BlockID alg a -> ConsensusM alg a x)
  | DoPropose       Round (BlockID alg a)         (ConsensusM alg a x)
  | DoCommit        (BlockID alg a) (TMState alg a)
  deriving (Functor)

instance Applicative (ConsensusM alg a) where
  pure  = OK
  (<*>) = ap

instance Monad (ConsensusM alg a) where
  return = OK
  OK a         >>= f = f a
  TranquilityM >>= _ = TranquilityM
  MisdeedM     >>= _ = MisdeedM
  --
  DoTimeout t m         >>= f = DoTimeout t         (m >>= f)
  DoCastPrevote r b m   >>= f = DoCastPrevote r b   (m >>= f)
  DoCastPrecommit r b m >>= f = DoCastPrecommit r b (m >>= f)
  DoMakeProposal g      >>= f = DoMakeProposal      (fmap (>>= f) g)
  DoPropose r b m       >>= f = DoPropose r b       (m >>= f)
  DoCommit  b tm        >>= _ = DoCommit b tm

instance ConsensusMonad (ConsensusM alg a) where
  tranquility = TranquilityM
  misdeed     = MisdeedM
  panic       = error
