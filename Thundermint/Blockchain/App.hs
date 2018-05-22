{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NumDecimals       #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TupleSections     #-}
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
import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe
import Control.Concurrent
import Control.Concurrent.STM
-- import           Data.Foldable
import           Data.Function
import           Data.Monoid       ((<>))
import qualified Data.Map        as Map
-- import           Data.Map          (Map)
import Text.Groom

import Thundermint.Blockchain.Types
import Thundermint.Crypto
import Thundermint.Consensus.Algorithm
import Thundermint.Consensus.Types
import Thundermint.Store
import Thundermint.Logger

import Katip (Severity(..), showLS, logStr)

----------------------------------------------------------------
--
----------------------------------------------------------------


-- | Main loop for application. Here we update state machine and
--   blockchain in response to incoming messages.
--
--   * INVARIANT: Only this function can write to blockchain
runApplication
  :: (MonadIO m, MonadCatch m, MonadLogger m, Crypto alg, Serialise a, Show a)
  => AppState m alg a
     -- ^ Get initial state of the application
  -> AppChans alg a
     -- ^ Channels for communication with peers
  -> m ()
runApplication appSt@AppState{..} appCh
  = logOnException
  $ fix (\loop commit -> do
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
  :: (MonadIO m, MonadLogger m, Crypto alg, Serialise a, Show a)
  => AppState m alg a
  -> AppChans alg a
  -> Maybe (Commit alg a)
  -> m (Commit alg a)
decideNewBlock appSt@AppState{..} appCh@AppChans{..} lastCommt = do
  -- Enter NEW HEIGHT and create initial state for consensus state
  -- machine
  hParam <- makeHeightParametes appSt appCh
  let totalPower       = sum $ fmap validatorVotingPower appValidatorsSet
      votingPower addr = case Map.lookup addr appValidatorsSet of
        Just i  -> validatorVotingPower i
        Nothing -> 0
  --
  -- FIXME: encode that we cannot fail here!
  Success tm0 <- runConsesusM $ newHeight hParam lastCommt votingPower totalPower
  -- Handle incoming messages until we decide on next block.
  flip fix tm0 $ \loop tm -> do
    logger DebugS ("TM =\n" <> logStr (groom tm)) ()
    -- Make current state of consensus available for gossip
    liftIO $ atomically $ writeTVar appTMState $ Just (currentH hParam , tm)
    -- Receive message
    msg <- liftIO $ atomically $ readTChan appChanRx
    logger DebugS ("Recv: " <> showLS msg) ()
    -- Handle message
    res <- runMaybeT
        $ lift . handleVerifiedMessage appStorage hParam tm
      =<< verifyMessageSignature appSt msg
    case res of
      Nothing             -> loop tm
      Just (Success r)    -> loop r
      Just Tranquility    -> loop tm
      Just Misdeed        -> loop tm
      Just (DoCommit cmt) -> do
        blocks <- retrievePropBlocks appStorage (currentH hParam)
        logger DebugS ("COMMIT: \n" <> logStr (groom cmt)) ()
        logger DebugS ("BLOCKMAP: \n" <> logStr (groom blocks)) ()
        -- FIXME: handle partiality
        let b = case commitBlockID cmt `Map.lookup` blocks of
                  Just x  -> x
                  Nothing -> error $ "Cannot commit: " ++ show cmt
        storeCommit appStorage cmt b
        return cmt


-- Handle message and perform state transitions for both
handleVerifiedMessage
  :: (MonadLogger m, Crypto alg)
  => BlockStorage 'RW m alg a
  -> HeightParameres (ConsensusM alg a m) alg a
  -> TMState alg a
  -> MessageRx 'Verified alg a
  -> m (ConsensusResult alg a (TMState alg a))
handleVerifiedMessage BlockStorage{..} hParam tm = \case
  -- FIXME: check that proposal comes from correct proposer
  RxProposal  p -> runConsesusM $ tendermintTransition hParam (ProposalMsg  p) tm
  RxPreVote   v -> runConsesusM $ tendermintTransition hParam (PreVoteMsg   v) tm
  RxPreCommit v -> runConsesusM $ tendermintTransition hParam (PreCommitMsg v) tm
  RxTimeout   t -> runConsesusM $ tendermintTransition hParam (TimeoutMsg   t) tm
  -- We update block storage
  RxBlock     b -> do storePropBlock (currentH hParam) b
                      return (Success tm)

-- Verify signature of message. If signature is not correct message is
-- simply discarded
--
-- NOTE: set of validators may change so we may lack public key of
--       some validators from height different from current. But since
--       we ignore messages from wrong height anyway it doesn't matter
verifyMessageSignature
  :: (Monad m, Crypto alg, Serialise a)
  => AppState m alg a
  -> MessageRx 'Unverified alg a
  -> MaybeT m (MessageRx 'Verified alg a)
verifyMessageSignature AppState{..} = \case
  RxPreVote   sv -> verify RxPreVote   sv
  RxPreCommit sv -> verify RxPreCommit sv
  RxProposal  sp -> verify RxProposal  sp
  RxTimeout   t  -> return $ RxTimeout t
  RxBlock     b  -> return $ RxBlock   b
  where
    verify con sx = case verifySignature pkLookup sx of
      Just sx' -> return $ con sx'
      Nothing  -> empty
    pkLookup a = validatorPubKey <$> Map.lookup a appValidatorsSet



----------------------------------------------------------------
-- Concrete implementation of ConsensusMonad
----------------------------------------------------------------

-- | Analog of @ExceptT Err IO@
newtype ConsensusM alg a m b = ConsensusM
  { runConsesusM :: m (ConsensusResult alg a b) }
  deriving (Functor)

data ConsensusResult alg a b
  = Success b
  | Tranquility
  | Misdeed
  | DoCommit  (Commit alg a)
  deriving (Functor)

instance Monad m => Applicative (ConsensusM alg a m) where
  pure  = return
  (<*>) = ap

instance Monad m => Monad (ConsensusM alg a m) where
  return = ConsensusM . return . Success
  ConsensusM m >>= f = ConsensusM $ m >>= \case
    Success a   -> runConsesusM (f a)
    Tranquility -> return Tranquility
    Misdeed     -> return Misdeed
    DoCommit cm -> return $ DoCommit cm

instance MonadIO m => MonadIO (ConsensusM alg a m) where
  liftIO = ConsensusM . fmap Success . liftIO

instance Monad m => ConsensusMonad (ConsensusM alg a m) where
  tranquility = ConsensusM $ return Tranquility
  misdeed     = ConsensusM $ return Misdeed
  panic       = error

instance MonadLogger m => MonadLogger (ConsensusM alg a m) where
  logger s l a = lift $ logger s l a
  localNamespace f (ConsensusM action) = ConsensusM $ localNamespace f action

instance MonadTrans (ConsensusM alg a) where
  lift = ConsensusM . fmap Success

makeHeightParametes
  :: (MonadIO m, MonadLogger m, Crypto alg, Serialise a, Show a)
  => AppState m alg a
  -> AppChans alg a
  -> m (HeightParameres (ConsensusM alg a m) alg a)
makeHeightParametes AppState{..} AppChans{..} = do
  h <- blockchainHeight appStorage
  let proposerChoice (Round r) =
        let Height h' = h
            n         = Map.size appValidatorsSet
            i         = (h' + r) `mod` fromIntegral n
        in  Map.keys appValidatorsSet !! fromIntegral i
  --
  return HeightParameres
    { currentH        = h
      -- FIXME: this is some random algorithms that should probably
      --        work (for some definition of work)
    , areWeProposers  = \r ->
        proposerChoice r == address (publicKey (validatorPrivKey appValidator))
    , proposerForRound = proposerChoice
    --
    , validateBlock = \bid -> do
        blocks <- lift $ retrievePropBlocks appStorage h
        case bid `Map.lookup` blocks of
          Nothing -> return UnseenProposal
          Just b
            | validateBlockData appValidator (blockData b)
              -> return GoodProposal
            | otherwise
              -> return InvalidProposal
    --
    , broadcastProposal = \r bid lockInfo -> do
        let pk   = validatorPrivKey appValidator
            prop = Proposal { propHeight    = h
                            , propRound     = r
                            , propTimestamp = Time 0
                            , propPOL       = lockInfo
                            , propBlockID   = bid
                            }
            sprop  = signValue pk prop
        blockMap <- lift $ retrievePropBlocks appStorage h
        logger InfoS ("Sending proposal for " <> showLS r <> " " <> showLS bid) ()
        liftIO $ atomically $ do
          writeTChan appChanTx (TxProposal sprop)
          writeTChan appChanRx (RxProposal $ unverifySignature sprop)
          case bid `Map.lookup` blockMap of
            Nothing -> return ()
            Just b  -> writeTChan appChanRx (RxBlock b)
    --
    , scheduleTimeout = \t@(Timeout _ (Round r) _) -> do
        logger InfoS ("Scheduling timeout: " <> showLS t) ()
        liftIO $ void $ forkIO $ do
          let baseT = 500e3
              delta = 500e3
          threadDelay $ baseT + delta * fromIntegral r
          atomically $ writeTChan appChanRx $ RxTimeout t
    -- FIXME: Do we need to store cast votes to WAL as well?
    , castPrevote     = \r b -> do
        let pk   = validatorPrivKey appValidator
            vote = Vote { voteHeight  = h
                        , voteRound   = r
                        , voteTime    = Time 0
                        , voteBlockID = b
                        }
            svote  = signValue pk vote
        logger InfoS ("Sending prevote for " <> showLS r <> " (" <> showLS b <> ")") ()
        liftIO $ atomically $ do
          writeTChan appChanTx (TxPreVote svote)
          writeTChan appChanRx (RxPreVote $ unverifySignature svote)
    --
    , castPrecommit   = \r b -> do
        let pk   = validatorPrivKey appValidator
            vote = Vote { voteHeight  = h
                        , voteRound   = r
                        , voteTime    = Time 0
                        , voteBlockID = b
                        }
            svote  = signValue pk vote
        logger InfoS ("Sending precommit for " <> showLS r <> " (" <> showLS b <> ")") ()
        liftIO $ atomically $ do
          writeTChan appChanTx (TxPreCommit svote)
          writeTChan appChanRx (RxPreCommit $ unverifySignature svote)

    , createProposal = \commit -> lift $ do
        bData          <- appBlockGenerator
        Just lastBlock <- retrieveBlock appStorage
                      =<< blockchainHeight appStorage
        let block = Block
              { blockHeader     = Header
                  { headerChainID     = appChainID
                  , headerHeight      = next h
                  , headerTime        = Time 0
                  , headerLastBlockID = Just (blockHash lastBlock)
                  }
              , blockData       = bData
              , blockLastCommit = commit
              }
        storePropBlock appStorage h block
        return $ blockHash block

    , commitBlock     = \cm -> ConsensusM $ return $ DoCommit cm
    }
