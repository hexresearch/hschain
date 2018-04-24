{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveFunctor   #-}
{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}
-- |
-- Full blockchain application
module Thundermint.Blockchain.App where

import Control.Monad
import Control.Monad.IO.Class
import Control.Concurrent
import Control.Concurrent.STM
import           Data.Foldable
import           Data.Function
import qualified Data.Map        as Map
import           Data.Map          (Map)

import Thundermint.Blockchain.Types
import Thundermint.Crypto
import Thundermint.Crypto.Containers
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
  -- FIXME: at the moment we start from genesis block but we need to
  --        pass valid last commit
  = forever $ heightLoop as ac Nothing

-- Loop where we decide which block we need to commit at given height
heightLoop
  :: (Crypto alg, Serializable a)
  => AppState alg a
  -> AppChans alg a
  -> Maybe (Commit alg a)
  -> IO ()
heightLoop appSt@AppState{..} appCh@AppChans{..} lastCommt = do
  hParam <- makeHeightParametes appSt appCh
  let totalPower       = sum $ fmap validatorVotingPower appValidatorsSet
      votingPower addr = case Map.lookup addr appValidatorsSet of
        Just i  -> validatorVotingPower i
        Nothing -> 0
      tmState0 = TMState
        { smRound         = Round 0
        , smStep          = StepProposal
        , smPrevotesSet   = emptySignedSetMap votingPower totalPower
        , smPrecommitsSet = emptySignedSetMap votingPower totalPower
        , smProposals     = Map.empty
        , smLockedBlock   = Nothing
        , smLastCommit    = lastCommt
        }
  mtm <- runConsesusM (newHeight hParam tmState0)
  case mtm of
    Success tm -> loop hParam tm
    _          -> error "FIXME: what to do? We cannot fail here!"
  where
    loop hParams tm = do
      msg <- atomically $ readTChan appChanRx
      let pkLookup a = validatorPubKey <$> Map.lookup a appValidatorsSet
          verify x cont = case verifySignature pkLookup x of
            Just x' -> cont x'
            Nothing -> loop hParams tm
      let recur m =
            runConsesusM (tendermintTransition hParams m tm) >>= \case
              Success tm'    -> loop hParams tm'
              Tranquility    -> loop hParams tm
              Misdeed        -> loop hParams tm
              DoCommit tm' b -> do
                -- 1. Write block to blockchain
                -- 2. return commit for current block 
                undefined
      case msg of
        RxPreVote   v -> verify v (recur . PreVoteMsg)
        RxPreCommit v -> verify v (recur . PreCommitMsg)
        RxProposal  p -> verify p $ \sp ->
          -- FIXME: we need to separate wheat (GoodProposal) from chaff (InvalidProposal)
          let Proposal{..} = signedValue sp
          in recur $ ProposalMsg propHeight propRound (GoodProposal propBlockID)
        RxTimeout t -> recur (TimeoutMsg t)



----------------------------------------------------------------
-- Concrete implementation of ConsensusMonad
----------------------------------------------------------------

-- | Analog of @ExceptT Err IO@
newtype ConsensusM alg a b = ConsensusM
  { runConsesusM :: IO (ConsensusResult alg a b) }
  deriving (Functor)

data ConsensusResult alg a b
  = Success b
  | Tranquility
  | Misdeed
  | DoCommit  (TMState alg a) (BlockID alg a)
  deriving (Functor)

instance Applicative (ConsensusM alg a) where
  pure  = return
  (<*>) = ap

instance Monad (ConsensusM alg a) where
  return = ConsensusM . return . Success
  ConsensusM m >>= f = ConsensusM $ m >>= \case
    Success a     -> runConsesusM (f a)
    Tranquility   -> return Tranquility
    Misdeed       -> return Misdeed
    DoCommit tm b -> return $ DoCommit tm b

instance MonadIO (ConsensusM alg a) where
  liftIO = ConsensusM . fmap Success

instance ConsensusMonad (ConsensusM alg a) where
  tranquility = ConsensusM $ return Tranquility
  misdeed     = ConsensusM $ return Misdeed
  panic       = error

makeHeightParametes
  :: (Crypto alg, Serializable a)
  => AppState alg a
  -> AppChans alg a
  -> IO (HeightParameres (ConsensusM alg a) alg a)
makeHeightParametes AppState{..} AppChans{..} = do
  h <- readTVarIO appBlockchain >>= \case
    Cons    b _ -> return $ headerHeight $ blockHeader b
    Genesis b   -> return $ headerHeight $ blockHeader b
  return HeightParameres
    { currentH        = h
    --
    , scheduleTimeout = \t -> liftIO $ void $ forkIO $ do
        threadDelay (4*1000*1000)
        atomically $ writeTChan appChanRx $ RxTimeout t
    --
    , castPrevote     = \r b -> liftIO $ do
        let pk   = validatorPrivKey appValidator
            vote = Vote { voteHeight  = h
                        , voteRound   = r
                        , voteTime    = Time 0
                        , voteBlockID = b
                        }
            svote  = signValue pk vote
        atomically $ writeTChan appChanTx (TxPreVote svote)
    --
    , castPrecommit   = \r b -> liftIO $ do
        let pk   = validatorPrivKey appValidator
            vote = Vote { voteHeight  = h
                        , voteRound   = r
                        , voteTime    = Time 0
                        , voteBlockID = b
                        }
            svote  = signValue pk vote
        atomically $ writeTChan appChanTx (TxPreCommit svote)
    --
    , proposeBlock    = \r b -> liftIO $ do
        blockStore <- readTVarIO appBlockStore
        blck       <- case b `Map.lookup` blockStore of
          Just x  -> return x
          Nothing -> error "FIXME: internal error: missing block"
        let pk   = validatorPrivKey appValidator
            prop = Proposal { propHeight    = h
                            , propRound     = r
                            , propTimestamp = Time 0
                            , propPOL       = Nothing
                            , propBlockID   = b
                            , propBlock     = blck
                            }
            sprop  = signValue pk prop
        atomically $ writeTChan appChanTx (TxProposal sprop)
    --
    , makeProposal    = undefined
      -- FIXME: this is some random algorithms that should probably
      --        work (for some definition of work)
    , areWeProposers  = \(Round r) ->
        let Height h' = h
            n         = Map.size appValidatorsSet
            i         = (h' + r) `mod` fromIntegral n
            addr      = Map.keys appValidatorsSet !! fromIntegral i
        in addr == address (publicKey (validatorPrivKey appValidator))
    , commitBlock     = \tm b -> ConsensusM $ return $ DoCommit tm b
    }
