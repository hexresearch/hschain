{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE MultiWayIf        #-}
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
import Pipes                 (Pipe,Consumer,Proxy,runEffect,yield,await,(>->))

import Thundermint.Blockchain.Internal.Engine.Types
import Thundermint.Blockchain.Internal.Types
import Thundermint.Blockchain.Internal.Algorithm
import Thundermint.Types.Blockchain
import Thundermint.Control (throwNothing,throwNothingM)
import Thundermint.Crypto
import Thundermint.Exceptions
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

newAppChans :: (MonadIO m, Crypto alg) => ConsensusCfg -> m (AppChans m alg a)
newAppChans ConsensusCfg{incomingQueueSize = sz} = do
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
  :: ( MonadDB m alg a
     , MonadIO m
     , MonadMask m
     , MonadLogger m
     , MonadTMMonitoring m
     , Crypto alg, BlockData a)
  => ConsensusCfg
     -- ^ Configuration
  -> Maybe (PrivValidator alg)
     -- ^ Private key of validator
  -> AppLogic m alg a
     -- ^ Get initial state of the application
  -> AppCallbacks m alg a
  -> AppChans m alg a
     -- ^ Channels for communication with peers
  -> m ()
runApplication config appValidatorKey appSt@AppLogic{..} appCall appCh@AppChans{..} = logOnException $ do
  logger InfoS "Starting consensus engine" ()
  height <- queryRO $ blockchainHeight
  lastCm <- queryRO $ retrieveLocalCommit height
  advanceToHeight appPropStorage $ succ height
  void $ flip fix lastCm $ \loop commit -> do
    cm <- decideNewBlock config appValidatorKey appSt appCall appCh commit
    loop (Just cm)


-- This function uses consensus algorithm to decide which block we're
-- going to commit at current height, then stores it in database and
-- returns commit.
--
-- FIXME: we should write block and last commit in transaction!
decideNewBlock
  :: ( MonadDB m alg a
     , MonadIO m
     , MonadMask m
     , MonadLogger m
     , MonadTMMonitoring m
     , Crypto alg, BlockData a)
  => ConsensusCfg
  -> Maybe (PrivValidator alg)
  -> AppLogic     m alg a
  -> AppCallbacks m alg a
  -> AppChans     m alg a
  -> Maybe (Commit alg a)
  -> m (Commit alg a)
decideNewBlock config appValidatorKey appLogic@AppLogic{..} appCall@AppCallbacks{..} appCh@AppChans{..} lastCommt = do
  -- Enter NEW HEIGHT and create initial state for consensus state
  -- machine
  --
  -- FIXME: we don't want duplication! (But pipes & producer does not unify)
  hParam  <- makeHeightParameters config appValidatorKey appLogic appCall appCh
  hParam0 <- makeHeightParameters config appValidatorKey appLogic appCall appCh
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
        res <- handleVerifiedMessage appPropStorage hParam tm msg
        case res of
          Tranquility      -> msgHandlerLoop mCmt tm
          Misdeed          -> msgHandlerLoop mCmt tm
          Success  tm'     -> checkForCommit mCmt       tm'
          DoCommit cmt tm' -> checkForCommit (Just cmt) tm'
      --
      checkForCommit Nothing    tm = msgHandlerLoop Nothing tm
      checkForCommit (Just cmt) tm = do
        mBlk <- lift
              $ retrievePropByID appPropStorage (currentH hParam) (commitBlockID cmt)
        case mBlk of
          UnknownBlock           -> msgHandlerLoop (Just cmt) tm
          InvalidBlock           -> error "Trying to commit invalid block!"
          GoodBlock _ b st' val' -> lift $ performCommit b st' val'
          UntestedBlock _ b      -> do
            st <- throwNothingM BlockchainStateUnavalable
                $ lift
                $ bchStoreRetrieve appBchState $ pred (currentH hParam)
            lift (appValidationFun (validatorSet hParam) b st) >>= \case
              Nothing         -> error "Trying to commit invalid block!"
              Just (val',st') -> lift $ performCommit b st' val'
        where
          performCommit b st' val' = do
            logger InfoS "Actual commit" $ LogBlockInfo
              (currentH hParam)
              (blockData b)
              (maybe 0 (length . commitPrecommits) (blockLastCommit b))
            case appCommitQuery of
              SimpleQuery callback -> do
                r <- queryRW $ do storeCommit cmt b
                                  storeValSet b val'
                                  callback (validatorSet hParam) b
                case r of
                  Nothing -> error "Cannot write commit into database"
                  Just () -> return ()
              --
              MixedQuery callback -> do
                r <- queryRWT $ do storeCommit cmt b
                                   storeValSet b val'
                                   callback (validatorSet hParam) b
                case r of
                  Nothing -> error "Cannot write commit into database"
                  Just () -> return ()
            let h = headerHeight $ blockHeader b
            advanceToHeight appPropStorage (succ h)
            -- FIXME: work duplication
            bchStoreStore appBchState h st'
            appCommitCallback b
            return cmt
  runEffect $ do
    -- FIXME: encode that we cannot fail here!
    tm0 <- (  runConsesusM (newHeight hParam0 lastCommt)
          >-> handleEngineMessage config appCh
           ) >>= \case
      Success t -> return t
      _         -> throwM ImpossibleError
    messageSrc
      >-> verifyMessageSignature appLogic hParam
      >-> msgHandlerLoop Nothing tm0
      >-> handleEngineMessage config appCh


-- Handle message and perform state transitions for both
handleVerifiedMessage
  :: (MonadLogger m, Crypto alg)
  => ProposalStorage 'RW m alg a
  -> HeightParameters (ConsensusM alg a (Pipe x (EngineMessage alg a) m)) alg a
  -> TMState alg a
  -> MessageRx 'Verified alg a
  -> Pipe x (EngineMessage alg a) m (ConsensusResult alg a (TMState alg a))
handleVerifiedMessage ProposalStorage{..} hParam tm = \case
  -- FIXME: check that proposal comes from correct proposer
  RxProposal  p -> runConsesusM $ tendermintTransition hParam (ProposalMsg  p) tm
  RxPreVote   v -> runConsesusM $ tendermintTransition hParam (PreVoteMsg   v) tm
  RxPreCommit v -> runConsesusM $ tendermintTransition hParam (PreCommitMsg v) tm
  RxTimeout   t -> runConsesusM $ tendermintTransition hParam (TimeoutMsg   t) tm
  -- We update block storage
  RxBlock     b -> do lift $ storePropBlock b
                      return (Success tm)

-- Verify signature of message. If signature is not correct message is
-- simply discarded.
verifyMessageSignature
  :: (MonadLogger m, Crypto alg)
  => AppLogic m alg a
  -> HeightParameters n alg a
  -> Pipe (MessageRx 'Unverified alg a) (MessageRx 'Verified alg a) m r
verifyMessageSignature AppLogic{..} HeightParameters{..} = forever $ do
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
        <> sl "addr" (show (signedKeyInfo sx))
        )
    pkLookup mvset a = do vset <- mvset
                          validatorPubKey <$> validatorByIndex vset a

handleEngineMessage
  :: (MonadIO m, MonadTMMonitoring m)
  => ConsensusCfg
  -> AppChans m alg a
  -> Consumer (EngineMessage alg a) m r
handleEngineMessage ConsensusCfg{..} AppChans{..} = forever $ await >>= \case
  -- Timeout
  EngTimeout t@(Timeout _ (Round r) step) ->
    liftIO $ void $ forkIO $ do
      let (baseT,delta) = case step of
            StepNewHeight     -> timeoutNewHeight
            StepProposal      -> timeoutProposal
            StepPrevote       -> timeoutPrevote
            StepPrecommit     -> timeoutPrecommit
            StepAwaitCommit _ -> (0, 0)
      threadDelay $ 1000 * (baseT + delta * fromIntegral r)
      atomically $ writeTQueue appChanRxInternal $ RxTimeout t
  -- Announcements
  EngAnnPreVote sv -> do
    let Vote{..} = signedValue   sv
        i        = signedKeyInfo sv
    liftIO $ atomically $ writeTChan appChanTx $ AnnHasPreVote voteHeight voteRound i
  EngAnnPreCommit sv -> do
    let Vote{..} = signedValue sv
        i        = signedKeyInfo sv
    liftIO $ atomically $ writeTChan appChanTx $ AnnHasPreCommit voteHeight voteRound i
  EngAnnStep s ->
    liftIO $ atomically $ writeTChan appChanTx $ AnnStep s
  -- Metrics
  EngMetricsHR h r -> lift $ do
    usingGauge prometheusHeight h
    usingGauge prometheusRound  r


----------------------------------------------------------------
-- Concrete implementation of ConsensusMonad
----------------------------------------------------------------

makeHeightParameters
  :: ( MonadDB m alg a
     , MonadIO m
     , MonadThrow m
     , MonadLogger m
     , MonadTMMonitoring m
     , Crypto alg, Serialise a)
  => ConsensusCfg
  -> Maybe (PrivValidator alg)
  -> AppLogic     m alg a
  -> AppCallbacks m alg a
  -> AppChans     m alg a
  -> m (HeightParameters (ConsensusM alg a (Proxy x x' y y' m)) alg a)
makeHeightParameters ConsensusCfg{..} appValidatorKey AppLogic{..} AppCallbacks{..} AppChans{..} = do
  let AppByzantine{..} = appByzantine
  h         <- queryRO $ blockchainHeight
  valSet    <- throwNothing (DBMissingValSet (succ h)) <=< queryRO
             $ retrieveValidatorSet (succ h)
  oldValSet <- queryRO $ retrieveValidatorSet  h
  genesis   <- throwNothing DBMissingGenesis <=< queryRO
             $ retrieveBlock (Height 0)
  bchTime   <- do b <- throwNothing (DBMissingBlock h) <=< queryRO
                     $ retrieveBlock h
                  return $! headerTime $ blockHeader b
  let ourIndex = indexByValidator valSet . publicKey . validatorPrivKey
             =<< appValidatorKey
  let proposerChoice (Round r) =
        let Height h' = h
            n         = validatorSetSize valSet
        in ValidatorIdx $! fromIntegral $ (h' + r) `mod` fromIntegral n
  --
  return HeightParameters
    { currentH        = succ h
    , currentTime     = bchTime
    , validatorSet    = valSet
    , oldValidatorSet = oldValSet
      -- FIXME: this is some random algorithms that should probably
      --        work (for some definition of work)
    , areWeProposers  = \r -> Just (proposerChoice r) == ourIndex
    , proposerForRound = proposerChoice
    , readyCreateBlock = lift $ lift $ fromMaybe True <$> appCanCreateBlock h bchTime
    -- --
    , validateBlock = \bid -> do
        let nH = succ h
        lift (lift (retrievePropByID appPropStorage nH bid)) >>= \case
          UnknownBlock      -> return UnseenProposal
          InvalidBlock      -> return InvalidProposal
          GoodBlock{}       -> return GoodProposal
          UntestedBlock _ b -> do
            let invalid = InvalidProposal <$ lift (lift (setPropValidation appPropStorage bid Nothing))
            inconsistencies <- lift $ lift $ checkProposedBlock nH b
            st              <- throwNothingM BlockchainStateUnavalable
                             $ lift $ lift $ bchStoreRetrieve appBchState h
            mvalSet'        <- lift $ lift $ appValidationFun valSet b st
            if | not (null inconsistencies) -> do
               -- Block is not internally consistent
                   logger ErrorS "Proposed block has inconsistencies"
                     (  sl "H" nH
                     <> sl "errors" (map show inconsistencies)
                     )
                   invalid
               -- We don't put evidence into blocks yet so there shouldn't be any
               | _:_ <- blockEvidence b -> invalid
               -- Block is correct and validators change is correct as
               -- well
               | Just (valSet',st') <- mvalSet'
               , validatorSetSize valSet' > 0
               , blockValChange b == validatorsDifference valSet valSet'
                 -> do lift $ lift $ setPropValidation appPropStorage bid $ Just (st',valSet')
                       return GoodProposal
               | otherwise
                 -> invalid
    --
    , broadcastProposal = \r bid lockInfo ->
        forM_ (liftA2 (,) appValidatorKey ourIndex) $ \(PrivValidator pk, idx) -> do
          t <- getCurrentTime
          let prop = Proposal { propHeight    = succ h
                              , propRound     = r
                              , propTimestamp = t
                              , propPOL       = lockInfo
                              , propBlockID   = bid
                              }
          mBlock <- lift $ lift $ retrievePropByID appPropStorage h bid
          logger InfoS ("Sending proposal" <> byzantineMark byzantineBroadcastProposal)
            (   sl "R"    r
            <>  sl "BID" (show bid)
            )
          tryByzantine byzantineBroadcastProposal prop $ \prop' ->
            liftIO $ atomically $ do
              writeTQueue appChanRxInternal (RxProposal $ unverifySignature $ signValue idx pk prop')
              case blockFromBlockValidation mBlock of
                Just (_,b) -> writeTQueue appChanRxInternal (RxBlock b)
                Nothing    -> return ()
    --
    , castPrevote     = \r b ->
        forM_ (liftA2 (,) appValidatorKey ourIndex) $ \(PrivValidator pk, idx) -> do
          t@(Time ti) <- getCurrentTime
          let vote = Vote { voteHeight  = succ h
                          , voteRound   = r
                          , voteTime    = if t > bchTime then t else Time (ti + 1)
                          , voteBlockID = b
                          }
          logger InfoS ("Sending prevote" <> byzantineMark byzantineCastPrevote)
            (  sl "R"    r
            <> sl "bid" (show b)
            )
          tryByzantine byzantineCastPrevote vote $ \vote' ->
            liftIO $ atomically $
              writeTQueue appChanRxInternal (RxPreVote $ unverifySignature $ signValue idx pk $ vote')
    --
    , castPrecommit   = \r b ->
        forM_ (liftA2 (,) appValidatorKey ourIndex) $ \(PrivValidator pk, idx) -> do
          t <- getCurrentTime
          let vote = Vote { voteHeight  = succ h
                          , voteRound   = r
                          , voteTime    = t
                          , voteBlockID = b
                          }
          logger InfoS ("Sending precommit" <> byzantineMark byzantineCastPrecommit)
            (  sl "R" r
            <> sl "bid" (show b)
            )
          tryByzantine byzantineCastPrecommit vote $ \vote' ->
            liftIO $ atomically $
              writeTQueue appChanRxInternal $ RxPreCommit $ unverifySignature $ signValue idx pk $ vote'
    --
    , acceptBlock = \r bid -> do
        liftIO $ atomically $ writeTChan appChanTx $ AnnHasProposal (succ h) r
        lift $ lift $ allowBlockID appPropStorage r bid
    --
    , createProposal = \r commit -> lift $ lift $ do
        lastBID  <- queryRO $ retrieveBlockID =<< blockchainHeight
        -- Calculate time for block.
        currentT <- case h of
          -- For block at H=1 we in rather arbitrary manner take time
          -- of genesis + 1s
          Height 0 -> do b <- throwNothing DBMissingGenesis <=< queryRO
                            $ retrieveBlock (Height 0)
                         let Time t = headerTime $ blockHeader b
                         return $! Time (t + 1000)
          -- Otherwise we take time from commit and if for some reason
          -- we can't we have corrupted commit for latest block and
          -- can't continue anyway.
          _        -> case join $ liftA3 commitTime oldValSet (pure bchTime) commit of
            Just t  -> return t
            Nothing -> error "Corrupted commit. Cannot generate block"
        -- Create dummy block in order to pass it to generator
        let headerDummy = Header
              { headerChainID        = headerChainID $ blockHeader genesis
              , headerHeight         = succ h
              , headerTime           = currentT
              , headerLastBlockID    = lastBID
              , headerValidatorsHash = hashed valSet
              , headerDataHash       = Hashed $ Hash "" 
              , headerValChangeHash  = hashed mempty
              , headerLastCommitHash = hashed commit
              , headerEvidenceHash   = hashed []
              }
            blockDummy = Block
              { blockHeader     = headerDummy
              , blockData       = error "Block data is not yet generated"
              , blockValChange  = mempty
              , blockLastCommit = commit
              , blockEvidence   = []
              }
        -- Call block generator
        st               <- throwNothingM BlockchainStateUnavalable
                          $ bchStoreRetrieve appBchState h
        (bData, valSet') <- appBlockGenerator valSet blockDummy st
                        =<< peekNTransactions appMempool
        let valCh = validatorsDifference valSet valSet'
            block = blockDummy
              { blockHeader = headerDummy
                  { headerDataHash      = hashed bData
                  , headerValChangeHash = hashed valCh
                  }
              , blockData      = bData
              , blockValChange = valCh
              }
            bid   = blockHash block
        allowBlockID   appPropStorage r bid
        storePropBlock appPropStorage block
        return bid
    --
    , commitBlock     = \cm r -> ConsensusM $ return $ DoCommit cm r
    }
  where
    -- | Add marks for analyzing logs
    byzantineMark = maybe mempty (const " (byzantine!)")
    -- | If 'modifier' exists, modify 'arg' with it and run 'action' with modified argument;
    --   else run 'action' with original argument.
    tryByzantine :: (Monad m)
                 => Maybe (a -> m (Maybe a))
                 -> a
                 -> (a -> ConsensusM alg b (Proxy x x' y y' m) ())
                 -> ConsensusM alg b (Proxy x x' y y' m) ()
    tryByzantine Nothing         arg action = action arg
    tryByzantine (Just modifier) arg action =
        lift (lift (modifier arg)) >>= maybe (return ()) action
