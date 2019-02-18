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
module Thundermint.Blockchain.Internal.Engine (
    newAppChans
  , runApplication
  ) where

import Codec.Serialise           (Serialise)
import Control.Applicative
import Control.Concurrent        (forkIO, threadDelay)
import Control.Concurrent.STM
import Control.Monad
import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import           Data.Maybe    (fromMaybe)
import           Data.Function
import           Data.Monoid   ((<>))
import Data.Text             (Text)
import Pipes                 (Pipe,runEffect,yield,await,(>->))

import Thundermint.Blockchain.Internal.Engine.Types
import Thundermint.Blockchain.Internal.Algorithm
import Thundermint.Types.Blockchain
import Thundermint.Crypto
import Thundermint.Logger
import Thundermint.Store
import Thundermint.Store.STM
import Thundermint.Store.Internal.BlockDB
import Thundermint.Monitoring
import Thundermint.Types.Validators

import Katip (Severity(..), sl)

----------------------------------------------------------------
--
----------------------------------------------------------------

newAppChans :: (MonadIO m, Crypto alg, Serialise a) => ConsensusCfg -> m (AppChans m alg a)
newAppChans ConsensusCfg{incomingQueueSize = sz} = do
  -- 7 is magical no good reason to use 7 but no reason against it either
  appChanRx         <- liftIO $ newTBQueueIO sz
  appChanRxInternal <- liftIO   newTQueueIO
  appChanTx         <- liftIO   newBroadcastTChanIO
  appTMState        <- liftIO $ newTVarIO Nothing
  appPropStorage    <- newSTMPropStorage
  return AppChans{..}

-- | Main loop for application. Here we update state machine and
--   blockchain in response to incoming messages.
--
--   * INVARIANT: Only this function can write to blockchain
runApplication
  :: ( MonadDB m alg a, MonadCatch m, MonadLogger m, MonadTMMonitoring m, Crypto alg, Show a, BlockData a)
  => ConsensusCfg
     -- ^ Configuration
  -> (Height -> Time -> m Bool)
     -- ^ Whether application is ready to create new block
  -> AppState m alg a
     -- ^ Get initial state of the application
  -> AppChans m alg a
     -- ^ Channels for communication with peers
  -> m ()
runApplication config ready appSt@AppState{..} appCh@AppChans{..} = logOnException $ do
  logger InfoS "Starting consensus engine" ()
  height <- queryRO $ blockchainHeight
  lastCm <- queryRO $ retrieveLocalCommit height
  advanceToHeight appPropStorage $ succ height
  void $ flip fix lastCm $ \loop commit -> do
    cm <- decideNewBlock config ready appSt appCh commit
    loop (Just cm)


-- This function uses consensus algorithm to decide which block we're
-- going to commit at current height, then stores it in database and
-- returns commit.
--
-- FIXME: we should write block and last commit in transaction!
decideNewBlock
  :: ( MonadDB m alg a, MonadLogger m, MonadTMMonitoring m, Crypto alg, Show a, BlockData a)
  => ConsensusCfg
  -> (Height -> Time -> m Bool)
  -> AppState m alg a
  -> AppChans m alg a
  -> Maybe (Commit alg a)
  -> m (Commit alg a)
decideNewBlock config ready appSt@AppState{..} appCh@AppChans{..} lastCommt = do
  -- Enter NEW HEIGHT and create initial state for consensus state
  -- machine
  hParam <- makeHeightParameters config ready appSt appCh
  -- Get rid of messages in WAL that are no longer needed and replay
  -- all messages stored there.
  walMessages <- fmap (fromMaybe [])
               $ queryRW
               $ resetWAL (currentH hParam)
              *> readWAL  (currentH hParam)
  -- Producer of messages. First we replay messages from WAL. This is
  -- very important and failure to do so may violate protocol
  -- safety and possibly liveness.
  --
  -- Without WAL we losing consensus state for current round which
  -- mean loss of lock on block if we were locked on it.
  let messageSrc = do
        forM_ walMessages yield
        -- NOTE: We try to read internal messages first. This is
        --       needed to ensure that timeouts are delivered in
        --       timely manner
        forever $ yield =<< liftIO (atomically $  readTQueue  appChanRxInternal
                                              <|> readTBQueue appChanRx)
  -- Main loop of message handler and message consumer. We process
  -- every message even after we decided to commit block. This is
  -- needed to
  --
  --  1. If we're catching up it's possible that we don't have block
  --     we want to commit yet.
  --  2. Collect stragglers precommits.
  let msgHandlerLoop mCmt tm = do
        -- Make current state of consensus available for gossip
        liftIO $ atomically $ writeTVar appTMState $ Just (currentH hParam , tm)
        -- Write message to WAL and handle it after that
        msg <- await
        _   <- lift $ queryRW $ writeToWAL (currentH hParam) (unverifyMessageRx msg)
        res <- lift $ handleVerifiedMessage appPropStorage hParam tm msg
        case res of
          Tranquility      -> msgHandlerLoop mCmt tm
          Misdeed          -> msgHandlerLoop mCmt tm
          Success  tm'     -> checkForCommit mCmt       tm'
          DoCommit cmt tm' -> checkForCommit (Just cmt) tm'
      --
      checkForCommit Nothing    tm = msgHandlerLoop Nothing tm
      checkForCommit (Just cmt) tm = do
        lift (retrievePropByID appPropStorage (currentH hParam) (commitBlockID cmt)) >>= \case
          Nothing -> msgHandlerLoop (Just cmt) tm
          Just b  -> lift $ do
            logger InfoS "Actual commit" $ LogBlockInfo (currentH hParam) (blockData b)
            case appCommitQuery of
              SimpleQuery callback -> do
                r <- queryRW $ do storeCommit cmt b
                                  storeValSet b =<< callback b
                case r of
                  Nothing -> error "Cannot write commit into database"
                  Just () -> return ()
              --
              MixedQuery mcall -> do
                callback <- mcall
                r <- queryRW $ do storeCommit cmt b
                                  (vset,action) <- callback b
                                  storeValSet b vset
                                  return action
                case r of
                  Just action -> action
                  Nothing     -> error "Cannot write commit into database"
            advanceToHeight appPropStorage . succ =<< queryRO blockchainHeight
            appCommitCallback b
            return cmt
  --
  -- FIXME: encode that we cannot fail here!
  Success tm0 <- runConsesusM $ newHeight hParam lastCommt
  runEffect $ messageSrc
          >-> verifyMessageSignature appSt hParam
          >-> msgHandlerLoop Nothing tm0



-- Handle message and perform state transitions for both
handleVerifiedMessage
  :: (MonadLogger m, Crypto alg)
  => ProposalStorage 'RW m alg a
  -> HeightParameters (ConsensusM alg a m) alg a
  -> TMState alg a
  -> MessageRx 'Verified alg a
  -> m (ConsensusResult alg a (TMState alg a))
handleVerifiedMessage ProposalStorage{..} hParam tm = \case
  -- FIXME: check that proposal comes from correct proposer
  RxProposal  p -> runConsesusM $ tendermintTransition hParam (ProposalMsg  p) tm
  RxPreVote   v -> runConsesusM $ tendermintTransition hParam (PreVoteMsg   v) tm
  RxPreCommit v -> runConsesusM $ tendermintTransition hParam (PreCommitMsg v) tm
  RxTimeout   t -> runConsesusM $ tendermintTransition hParam (TimeoutMsg   t) tm
  -- We update block storage
  RxBlock     b -> do storePropBlock b
                      return (Success tm)

-- Verify signature of message. If signature is not correct message is
-- simply discarded.
verifyMessageSignature
  :: (MonadLogger m, Crypto alg, Serialise a)
  => AppState m alg a
  -> HeightParameters n alg a
  -> Pipe (MessageRx 'Unverified alg a) (MessageRx 'Verified alg a) m r
verifyMessageSignature AppState{..} HeightParameters{..} = forever $ do
  await >>= \case
    RxPreVote   sv
      | h      == currentH -> verify "prevote"   RxPreVote   sv
      | otherwise          -> return ()
      where h = voteHeight $ signedValue sv
    RxPreCommit sv
      -- For messages from previous height we validate them against
      -- correct validator set
      | h      == currentH -> verify    "precommit" RxPreCommit sv
      | succ h == currentH -> verifyOld "precommit" RxPreCommit sv
      | otherwise          -> return ()
      where h = voteHeight $ signedValue sv
    RxProposal  sp
      | h == currentH -> verify "proposal"  RxProposal  sp
      | otherwise     -> return ()
      where h = propHeight $ signedValue sp
    RxTimeout   t  -> yield $ RxTimeout t
    RxBlock     b  -> yield $ RxBlock   b
  where
    verify    con sx = verifyAny (Just validatorSet) con sx
    verifyOld con sx = verifyAny oldValidatorSet     con sx
    verifyAny vset name con sx = case verifySignature (pkLookup vset) sx of
      Just sx' -> yield $ con sx'
      Nothing  -> lift $ logger WarningS "Invalid signature"
        (  sl "name" (name::Text)
        <> sl "addr" (show (signedAddr sx))
        )
    pkLookup mvset a = do vset <- mvset
                          validatorPubKey <$> validatorByAddr vset a



----------------------------------------------------------------
-- Concrete implementation of ConsensusMonad
----------------------------------------------------------------

-- | Analog of @ExceptT Err IO@
newtype ConsensusM alg a m b = ConsensusM
  { runConsesusM :: m (ConsensusResult alg a b) }
  deriving (Functor)

data ConsensusResult alg a b
  = Success !b
  | Tranquility
  | Misdeed
  | DoCommit  !(Commit alg a) !(TMState alg a)
  deriving (Functor)

instance Monad m => Applicative (ConsensusM alg a m) where
  pure  = return
  (<*>) = ap

instance Monad m => Monad (ConsensusM alg a m) where
  return = ConsensusM . return . Success
  ConsensusM m >>= f = ConsensusM $ m >>= \case
    Success a     -> runConsesusM (f a)
    Tranquility   -> return Tranquility
    Misdeed       -> return Misdeed
    DoCommit cm r -> return $ DoCommit cm r

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

makeHeightParameters
  :: (MonadDB m alg a, MonadLogger m, MonadTMMonitoring m, Crypto alg, Serialise a, Show a)
  => ConsensusCfg
  -> (Height -> Time -> m Bool)
  -> AppState m alg a
  -> AppChans m alg a
  -> m (HeightParameters (ConsensusM alg a m) alg a)
makeHeightParameters ConsensusCfg{..} ready AppState{..} AppChans{..} = do
  h            <- queryRO $ blockchainHeight
  Just valSet  <- queryRO $ retrieveValidatorSet (succ h)
  oldValSet    <- queryRO $ retrieveValidatorSet  h
  Just genesis <- queryRO $ retrieveBlock        (Height 0)
  bchTime      <- do Just b <- queryRO $ retrieveBlock h
                     return $ headerTime $ blockHeader b
  let proposerChoice (Round r) =
        let Height h' = h
            n         = validatorSetSize valSet
            i         = (h' + r) `mod` fromIntegral n
            Just v    = validatorByIndex valSet (ValidatorIdx (fromIntegral i))
        in  address (validatorPubKey v)
  --
  return HeightParameters
    { currentH        = succ h
    , currentTime     = bchTime
    , validatorSet    = valSet
    , oldValidatorSet = oldValSet
      -- FIXME: this is some random algorithms that should probably
      --        work (for some definition of work)
    , areWeProposers  = \r -> case appValidator of
        Nothing                 -> False
        Just (PrivValidator pk) -> proposerChoice r == address (publicKey pk)
    , proposerForRound = proposerChoice
    , readyCreateBlock = lift $ ready h bchTime
    --
    , validateBlock = \bid -> do
        let nH = succ h
        lift (retrievePropByID appPropStorage nH bid) >>= \case
          Nothing -> return UnseenProposal
          Just b  -> do
            inconsistencies <- lift $ checkProposedBlock nH b
            blockOK         <- lift $ appValidationFun b
            case () of
              _| not (null inconsistencies) -> do
                   logger ErrorS "Proposed block has inconsistencies"
                     (  sl "H" nH
                     <> sl "errors" (map show inconsistencies)
                     )
                   return InvalidProposal
               -- We don't put evidence into blocks yet so there shouldn't be any
               | _:_ <- blockEvidence b -> return InvalidProposal
               | blockOK                -> return GoodProposal
               | otherwise              -> return InvalidProposal
    --
    , broadcastProposal = \r bid lockInfo ->
        forM_ appValidator $ \(PrivValidator pk) -> do
          t <- getCurrentTime
          let prop = Proposal { propHeight    = succ h
                              , propRound     = r
                              , propTimestamp = t
                              , propPOL       = lockInfo
                              , propBlockID   = bid
                              }
              sprop  = signValue pk prop
          mBlock <- lift $ retrievePropByID appPropStorage h bid
          logger InfoS "Sending proposal"
            (   sl "R"    r
            <>  sl "BID" (show bid)
            )
          liftIO $ atomically $ do
            writeTQueue appChanRxInternal (RxProposal $ unverifySignature sprop)
            case mBlock of
              Nothing -> return ()
              Just b  -> writeTQueue appChanRxInternal (RxBlock b)
    --
    , scheduleTimeout = \t@(Timeout _ (Round r) step) ->
        liftIO $ void $ forkIO $ do
          let (baseT,delta) = case step of
                StepNewHeight     -> timeoutNewHeight
                StepProposal      -> timeoutProposal
                StepPrevote       -> timeoutPrevote
                StepPrecommit     -> timeoutPrecommit
                StepAwaitCommit _ -> (0, 0)
          threadDelay $ 1000 * (baseT + delta * fromIntegral r)
          atomically $ writeTQueue appChanRxInternal $ RxTimeout t
    --
    , castPrevote     = \r b ->
        forM_ appValidator $ \(PrivValidator pk) -> do
          t@(Time ti) <- getCurrentTime
          let vote = Vote { voteHeight  = succ h
                          , voteRound   = r
                          , voteTime    = if t > bchTime then t else Time (ti + 1)
                          , voteBlockID = b
                          }
              svote  = signValue pk vote
          logger InfoS "Sending prevote"
            (  sl "R"    r
            <> sl "bid" (show b)
            )
          liftIO $ atomically $
            writeTQueue appChanRxInternal (RxPreVote $ unverifySignature svote)
    --
    , castPrecommit   = \r b ->
        forM_ appValidator $ \(PrivValidator pk) -> do
          t <- getCurrentTime
          let vote = Vote { voteHeight  = succ h
                          , voteRound   = r
                          , voteTime    = t
                          , voteBlockID = b
                          }
              svote  = signValue pk vote
          logger InfoS "Sending precommit"
            (  sl "R" r
            <> sl "bid" (show b)
            )
          liftIO $ atomically $ writeTQueue appChanRxInternal $ RxPreCommit $ unverifySignature svote
    --
    , acceptBlock = \r bid -> do
        liftIO $ atomically $ writeTChan appChanTx $ AnnHasProposal (succ h) r
        lift $ allowBlockID appPropStorage r bid
    --
    , announceHasPreVote   = \sv -> do
        let Vote{..} = signedValue sv
        forM_ (indexByValidator valSet (signedAddr sv)) $ \v ->
          liftIO $ atomically $ writeTChan appChanTx $ AnnHasPreVote voteHeight voteRound v
    --
    , announceHasPreCommit = \sv -> do
        let Vote{..} = signedValue sv
        forM_ (indexByValidator valSet (signedAddr sv)) $ \v ->
          liftIO $ atomically $ writeTChan appChanTx $ AnnHasPreCommit voteHeight voteRound v
    --
    , announceStep    = liftIO . atomically . writeTChan appChanTx . AnnStep
    , updateMetricsHR = \curH curR -> lift $ do
        usingGauge prometheusHeight curH
        usingGauge prometheusRound  curR
    --
    , createProposal = \r commit -> lift $ do
        lastBID  <- queryRO $ retrieveBlockID =<< blockchainHeight
        -- Calculate time for block.
        currentT <- case h of
          -- For block at H=1 we in rather arbitrary manner take time
          -- of genesis + 1s
          Height 0 -> do Just b <- queryRO $ retrieveBlock (Height 0)
                         let Time t = headerTime $ blockHeader b
                         return $! Time (t + 1000)
          -- Otherwise we take time from commit and if for some reason
          -- we can't we have corrupted commit for latest block and
          -- can't continue anyway.
          _        -> case join $ liftA3 commitTime oldValSet (pure bchTime) commit of
            Just t  -> return t
            Nothing -> error "Corrupted commit. Cannot generate block"
        bData    <- appBlockGenerator (succ h) currentT commit []
        let block = Block
              { blockHeader     = Header
                  { headerChainID        = headerChainID $ blockHeader genesis
                  , headerHeight         = succ h
                  , headerTime           = currentT
                  , headerLastBlockID    = lastBID
                  , headerValidatorsHash = hash valSet
                  , headerDataHash       = hash bData
                  }
              , blockData       = bData
              , blockLastCommit = commit
              , blockEvidence   = []
              }
            bid   = blockHash block
        allowBlockID   appPropStorage r bid
        storePropBlock appPropStorage block
        return bid

    , commitBlock     = \cm r -> ConsensusM $ return $ DoCommit cm r
    }
