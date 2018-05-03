{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE DeriveFunctor    #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE TupleSections    #-}
-- |
-- Core of blockchain application. This module provides function which
-- continuously updates blockchain using consensus algorithm and
-- communicates with outside world using STM channels.
module Thundermint.Blockchain.App (
    runApplication
  ) where

import Codec.Serialise (Serialise)
import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe
import Control.Concurrent
import Control.Concurrent.STM
-- import           Data.Foldable
import           Data.Function
import qualified Data.Map        as Map
import           Data.Map          (Map)
import Text.Groom

import Thundermint.Blockchain.Types
import Thundermint.Crypto
import Thundermint.Crypto.Containers
import Thundermint.Consensus.Algorithm
import Thundermint.Consensus.Types
import Thundermint.Store
-- import Thundermint.P2P


----------------------------------------------------------------
--
----------------------------------------------------------------


-- | Main loop for application. Here we update state machine and
--   blockchain in response to incoming messages.
--
--   * INVARIANT: Only this function can write to blockchain
runApplication
  :: (Crypto alg, Serialise a, Show a)
  => AppState alg a
     -- ^ Get initial state of the application
  -> AppChans alg a
     -- ^ Channels for communication with peers
  -> IO ()
runApplication appSt@AppState{..} appCh =
  fix (\loop commit -> do
          cm <- decideNewBlock appSt appCh commit
          -- ASSERT: We successfully commited next block
          --
          -- FIXME: do we need to communicate with peers about our
          --        commit? (Do we need +2/3 commits to proceed
          --        further)
          h  <- blockchainHeight appStorage
          case appMaxHeight of
            Just h' | h > h' -> return ()
            _                -> loop (Just cm)
      ) =<< retrieveLastCommit appStorage

-- This function uses consensus algorithm to decide which block we're
-- going to commit at current height, then stores it in database and
-- returns commit.
--
-- FIXME: we should write block and last commit in transaction!
decideNewBlock
  :: (Crypto alg, Serialise a, Show a)
  => AppState alg a
  -> AppChans alg a
  -> Maybe (Commit alg a)
  -> IO (Commit alg a)
decideNewBlock appSt@AppState{..} appCh@AppChans{..} lastCommt = do
  -- Create initial state for consensus state machine
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
  -- Enter PREVOTE of round 0
  --
  -- FIXME: encode that we cannot fail here!
  appLogger $ "NEW HEIGHT: " ++ show (currentH hParam)
  Success tm0 <- runConsesusM $ newHeight hParam tmState0
  -- Handle incoming messages until we decide on next block.
  flip fix (tm0, Map.empty) $ \loop (tm, blocks) -> do
    -- Make current state of consensus available for gossip
    appLogger $ unlines [ "### Next transition"
                        , groom tm
                        ]
    atomically $ writeTVar appTMState $ Just ( currentH hParam
                                             , tm
                                             , blocks
                                             )
    -- Receive message
    msg <- atomically $ readTChan appChanRx
    appLogger $ unlines [ "### Received message"
                        , groom msg
                        ]
    -- Handle message
    res <- runMaybeT
        $ lift . handleVerifiedMessage hParam (tm,blocks)
      =<< verifyMessageSignature appSt msg
    case res of
      Nothing             -> loop (tm, blocks)
      Just (Success r)    -> loop r
      Just Tranquility    -> loop (tm, blocks)
      Just Misdeed        -> loop (tm, blocks)
      Just (DoCommit cmt) -> do
        storeCommit appStorage cmt (undefined "lookup actual block by block ID")
        return cmt


-- Handle message and perform state transitions for both
handleVerifiedMessage
  :: (Crypto alg)
  => HeightParameres (ConsensusM alg a) alg a
  -> (TMState alg a, BlockMap alg a)
  -> MessageRx 'Verified alg a
  -> IO (ConsensusResult alg a (TMState alg a, BlockMap alg a))
handleVerifiedMessage hParam (tm, blocks) = \case
  -- FIXME: check that proposal comes from correct proposer
  RxProposal  p -> do tm' <- runConsesusM $ tendermintTransition hParam (ProposalMsg (signedValue p)) tm
                      return $ (, blocks) <$> tm'
  RxPreVote   v -> do tm' <- runConsesusM $ tendermintTransition hParam (PreVoteMsg v) tm
                      return $ (, blocks) <$> tm'
  RxPreCommit v -> do tm' <- runConsesusM $ tendermintTransition hParam (PreCommitMsg v) tm
                      return $ (, blocks) <$> tm'
  RxTimeout   t -> do tm' <- runConsesusM $ tendermintTransition hParam (TimeoutMsg t) tm
                      return $ (, blocks) <$> tm'
  -- We update block storage


-- Verify signature of message. If signature is not correct message is
-- simply discarded
--
-- NOTE: set of validators may change so we may lack public key of
--       some validators from height different from current. But since
--       we ignore messages from wrong height anyway it doesn't matter
verifyMessageSignature
  :: (Crypto alg, Serialise a)
  => AppState alg a
  -> MessageRx 'Unverified alg a
  -> MaybeT IO (MessageRx 'Verified alg a)
verifyMessageSignature AppState{..} = \case
  RxPreVote   sv -> verify RxPreVote   sv
  RxPreCommit sv -> verify RxPreCommit sv
  RxProposal  sp -> verify RxProposal  sp
  RxTimeout   t  -> return $ RxTimeout t
  where
    verify con sx = case verifySignature pkLookup sx of
      Just sx' -> return $ con sx'
      Nothing  -> empty
    pkLookup a = validatorPubKey <$> Map.lookup a appValidatorsSet



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
  | DoCommit  (Commit alg a)
  deriving (Functor)

instance Applicative (ConsensusM alg a) where
  pure  = return
  (<*>) = ap

instance Monad (ConsensusM alg a) where
  return = ConsensusM . return . Success
  ConsensusM m >>= f = ConsensusM $ m >>= \case
    Success a   -> runConsesusM (f a)
    Tranquility -> return Tranquility
    Misdeed     -> return Misdeed
    DoCommit cm -> return $ DoCommit cm

instance MonadIO (ConsensusM alg a) where
  liftIO = ConsensusM . fmap Success

instance ConsensusMonad (ConsensusM alg a) where
  tranquility = ConsensusM $ return Tranquility
  misdeed     = ConsensusM $ return Misdeed
  panic       = error

makeHeightParametes
  :: (Crypto alg, Serialise a, Show a)
  => AppState alg a
  -> AppChans alg a
  -> IO (HeightParameres (ConsensusM alg a) alg a)
makeHeightParametes AppState{..} AppChans{..} = do
  h <- blockchainHeight appStorage
  return HeightParameres
    { currentH        = h
      -- FIXME: this is some random algorithms that should probably
      --        work (for some definition of work)
    , areWeProposers  = \(Round r) ->
        let Height h' = h
            n         = Map.size appValidatorsSet
            i         = (h' + r) `mod` fromIntegral n
            addr      = Map.keys appValidatorsSet !! fromIntegral i
        in addr == address (publicKey (validatorPrivKey appValidator))
    , validateBlock = \_ -> return UnseenProposal

    --
    , broadcastProposal = \r bid -> liftIO $ do
        let pk   = validatorPrivKey appValidator
            prop = Proposal { propHeight    = h
                            , propRound     = r
                            , propTimestamp = Time 0
                            , propPOL       = Nothing
                            , propBlockID   = bid
                            }
            sprop  = signValue pk prop
        appLogger $ unlines [ ">>> SENDING PROPOSAL"
                            , groom sprop
                            ]
        atomically $ do
          writeTChan appChanTx (TxProposal sprop)
          writeTChan appChanRx (RxProposal $ unverifySignature sprop)
    --
    , scheduleTimeout = \t -> liftIO $ void $ forkIO $ do
        threadDelay (1*1000*1000)
        atomically $ writeTChan appChanRx $ RxTimeout t
    -- FIXME: Do we need to store cast votes to WAL as well?
    , castPrevote     = \r b -> liftIO $ do
        let pk   = validatorPrivKey appValidator
            vote = Vote { voteHeight  = h
                        , voteRound   = r
                        , voteTime    = Time 0
                        , voteBlockID = b
                        }
            svote  = signValue pk vote
        appLogger $ unlines [ ">>> SENDING PREVOTE"
                            , groom svote
                            ]
        atomically $ do
          writeTChan appChanTx (TxPreVote svote)
          writeTChan appChanRx (RxPreVote $ unverifySignature svote)
    --
    , castPrecommit   = \r b -> liftIO $ do
        let pk   = validatorPrivKey appValidator
            vote = Vote { voteHeight  = h
                        , voteRound   = r
                        , voteTime    = Time 0
                        , voteBlockID = b
                        }
            svote  = signValue pk vote
        appLogger $ unlines [ ">>> SENDING PRECOMMIT"
                            , groom svote
                            ]
        atomically $ do
          writeTChan appChanTx (TxPreCommit svote)
          writeTChan appChanRx (RxPreCommit $ unverifySignature svote)

    , createProposal    = \r cm -> liftIO $ do
        b <- appBlockGenerator cm
        storePropBlock appStorage h b
        return $ blockHash b

    , commitBlock     = \cm -> ConsensusM $ return $ DoCommit cm

    , logger = liftIO . appLogger
    }
