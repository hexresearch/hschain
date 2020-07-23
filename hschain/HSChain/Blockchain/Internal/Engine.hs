{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE MultiWayIf          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE NumDecimals         #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeApplications    #-}
-- |
-- Core of blockchain application. This module provides function which
-- continuously updates blockchain using consensus algorithm and
-- communicates with outside world using STM channels.
module HSChain.Blockchain.Internal.Engine (
    newAppChans
  , initializeBlockchain
  , runApplication
  ) where

import Control.Applicative
import Control.Concurrent        (forkIO, threadDelay)
import Control.Concurrent.STM
import Control.Monad
import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import           Data.Maybe    (fromMaybe)
import           Data.List     (nub)
import Data.Text             (Text)
import Data.Function         (on)
import Pipes                 (Pipe,Producer,Consumer,runEffect,yield,await,(>->))
import qualified Pipes.Prelude as Pipes

import HSChain.Blockchain.Internal.Engine.Types
import HSChain.Blockchain.Internal.Types
import HSChain.Blockchain.Internal.Algorithm
import HSChain.Types.Blockchain
import HSChain.Types.Merkle.Types
import HSChain.Control      (iterateM)
import HSChain.Control.Util (throwNothing,throwNothingM,throwLeftM,throwLeft,atomicallyIO)
import HSChain.Crypto
import HSChain.Exceptions
import HSChain.Logger
import HSChain.Store
import HSChain.Store.Internal.BlockDB
import HSChain.Store.Internal.Proposals
import HSChain.Monitoring
import HSChain.Types.Validators

import Katip (sl)

----------------------------------------------------------------
--
----------------------------------------------------------------

newAppChans :: (MonadIO m) => ConsensusCfg app -> m (AppChans a)
newAppChans ConsensusCfg{incomingQueueSize = sz} = do
  appChanRx         <- liftIO $ newTBQueueIO sz
  appChanTx         <- liftIO   newBroadcastTChanIO
  appTMState        <- liftIO $ newTVarIO Nothing
  return AppChans{..}

rewindBlockchainState
  :: ( MonadDB a m, MonadIO m, MonadThrow m
     , Crypto (Alg a), BlockData a)
  => AppStore m a
  -> AppLogic m a
  -> m ()
rewindBlockchainState AppStore{..} BChLogic{..} = do
  -- We need to generate validator set for H=1. We do so in somewhat
  -- fragile way.
  --
  -- We only could have no validator set at H=1 when we just started
  -- and only wrote genesis into database. So blockchain state must be
  -- at initial state. Therefore we can write validator state during
  -- evaluation of blocks.
  hChain   <- queryRO blockchainHeight
  (h0,st0) <- bchCurrentState appBchState >>= \case
    (Nothing, s) -> return (Height 0, s)
    (Just h,  s) -> return (succ h  , s)
  void $ foldr (>=>) return
    (interpretBlock <$> [h0 .. hChain])
    st0
  where
    interpretBlock h st = do
      blk    <- queryRO $ mustRetrieveBlock        h
      valSet <- queryRO $ mustRetrieveValidatorSet h
      BChEval{..}
        <- throwLeftM . runExceptT
         $ withExceptT CannotRewindState
         $ processBlock BChEval { bchValue        = blk
                                , validatorSet    = merkled valSet
                                , blockchainState = st
                                }
      -- Check validator set consistency
      do let Hashed expectedValue = blockStateHash blk
             Hashed actualValue   = merkleHashed blockchainState
         unless (expectedValue == actualValue)
           $ throwM $ InconsistenceWhenRewinding h
                      "Hash of calculated state doesn't match hash in block"
                      Mismatch{..}
      do let Hashed expectedValue = blockValidators blk
             Hashed actualValue   = hashed valSet
         unless (expectedValue == actualValue)
           $ throwM $ InconsistenceWhenRewinding h
                      "Validator set doesn't match validator set stored in block"
                      Mismatch{..}
      do let Hashed expectedValue = blockNewValidators blk
             Hashed actualValue   = merkleHashed validatorSet
         unless (expectedValue == actualValue)
           $ throwM $ InconsistenceWhenRewinding h
                      "New validator set doesn't match validator set stored in block"
                      Mismatch{..}
      -- Write validator set at H=1 to database if needed
      when (h == Height 0) $ mustQueryRW $ do
        hasValidatorSet (Height 1) >>= \case
          True  -> return ()
          False -> storeValSet (Height 1) validatorSet
      -- Put new state into state storage
      bchStoreStore appBchState h blockchainState
      return blockchainState

-- | Initialize blockchain: if we initialize fresh blockchain, compute
--   validator set for H=1 and
initializeBlockchain
  :: (MonadDB a m, MonadThrow m, MonadIO m, BlockData a, Show a, Eq a)
  => Genesis a
  -> AppLogic m a
  -> AppStore m a
  -> m ()
initializeBlockchain genesis appLogic appStore  = do
  mustQueryRW $ storeGenesis genesis
  rewindBlockchainState appStore appLogic

-- | Main loop for application. Here we update state machine and
--   blockchain in response to incoming messages.
--
--   * INVARIANT: Only this function can write to blockchain
runApplication
  :: ( MonadDB a m
     , MonadIO m
     , MonadMask m
     , MonadLogger m
     , MonadTMMonitoring m
     , BlockData a)
  => ConsensusCfg app
     -- ^ Configuration
  -> Maybe (PrivValidator (Alg a))
     -- ^ Private key of validator
  -> AppLogic m a
     -- ^ Get initial state of the application
  -> AppStore m a
  -> AppCallbacks m a
  -> AppChans a
     -- ^ Channels for communication with peers
  -> m ()
runApplication config appValidatorKey appLogic appStore appCall appCh = logOnException $ do
  logger InfoS "Starting consensus engine" ()
  -- Now we can start consensus
  appChanRxInternal <- liftIO newTQueueIO
  height <- queryRO $ blockchainHeight
  lastCm <- queryRO $ retrieveLocalCommit height
  iterateM lastCm $ fmap Just
                  . decideNewBlock config appValidatorKey appLogic appStore appCall appCh appChanRxInternal

-- This function uses consensus algorithm to decide which block we're
-- going to commit at current height, then stores it in database and
-- returns commit.
decideNewBlock
  :: ( MonadDB a m
     , MonadIO m
     , MonadMask m
     , MonadLogger m
     , MonadTMMonitoring m
     , Crypto (Alg a), BlockData a)
  => ConsensusCfg app
  -> Maybe (PrivValidator (Alg a))
  -> AppLogic     m a
  -> AppStore     m a
  -> AppCallbacks m a
  -> AppChans       a
  -> TQueue (MessageRx 'Unverified a)
  -> Maybe (Commit a)
  -> m (Commit a)
decideNewBlock config appValidatorKey
               appLogic@BChLogic{..} appStore@AppStore{..}
               appCall@AppCallbacks{..} appCh@AppChans{..} appChanRxInternal lastCommt = do
  -- Enter NEW HEIGHT and create initial state for consensus state
  -- machine
  hParam  <- makeHeightParameters appValidatorKey appLogic appStore appCall
  -- Run consensus engine
  (cmt, BChEval{..}) <- runEffect $ do
    let sink = handleEngineMessage hParam config appCh appChanRxInternal
    tm0 <-  newHeight hParam lastCommt
        >-> sink
    rxMessageSource hParam appCh appChanRxInternal
        >-> msgHandlerLoop hParam appLogic appStore appCh tm0
        >-> sink
  -- Update metrics
  do let nSign = maybe 0 (length . commitPrecommits . merkleValue) (blockPrevCommit bchValue)
         h     = blockHeight bchValue
     logger InfoS "Actual commit" $ LogBlockInfo h (merkleValue $ blockData bchValue) nSign
  -- We have decided which block we want to commit so let commit it
  do let h = blockHeight bchValue
     mustQueryRW $ do
       storeCommit cmt      bchValue
       storeValSet (succ h) validatorSet
       mapM_ storeBlockchainEvidence $ merkleValue $ blockEvidence bchValue
     bchStoreStore appBchState h blockchainState
     appCommitCallback bchValue
  return cmt

-- Producer for MessageRx. First we replay WAL and then we read
-- messages from channels.
rxMessageSource
  :: ( MonadIO m, MonadDB a m, MonadLogger m, MonadThrow m
     , Crypto (Alg a), BlockData a)
  => HeightParameters m a
  -> AppChans a
  -> TQueue (MessageRx 'Unverified a)
  -> Producer (MessageRx 'Verified a) m r
rxMessageSource HeightParameters{..} AppChans{..} appChanRxInternal = do
  -- First we replay messages from WAL. This is very important and
  -- failure to do so may violate protocol safety and possibly
  -- liveness.
  --
  -- Without WAL we're losing consensus state for current round which
  -- mean loss of lock if we were locked on block.
  walMessages <- mustQueryRW $ resetWAL currentH *> readWAL currentH
  forM_ walMessages yield
    >-> verify
  readMsg
    >-> verify
    >-> Pipes.chain (mustQueryRW . writeToWAL currentH . unverifyMessageRx)
  where
    verify  = verifyMessageSignature oldValidatorSet hValidatorSet currentH
    -- NOTE: We try to read internal messages first. This is needed to
    --       ensure that timeouts are delivered in timely manner
    readMsg = forever $ yield =<< atomicallyIO (  readTQueue  appChanRxInternal
                                              <|> readTBQueue appChanRx)


-- Main loop of message handler and message consumer. We process every
-- message even after we decided to commit block. This is needed to
--
--  1. If we're catching up it's possible that we don't have block
--     we want to commit yet.
--  2. Collect stragglers precommits.
msgHandlerLoop
  :: ( MonadReadDB a m, MonadThrow m, MonadIO m, MonadLogger m
     , Crypto (Alg a), Exception (BChError a))
  => HeightParameters m a
  -> AppLogic m a
  -> AppStore m a
  -> AppChans   a
  -> TMState a
  -> Pipe (MessageRx 'Verified a) (EngineMessage a) m
      (Commit a, ValidatedBlock a)
msgHandlerLoop hParam BChLogic{..} AppStore{..} AppChans{..} = mainLoop Nothing
  where
    height = currentH hParam
    mainLoop mCmt tm = do
      -- Make current state of consensus available for gossip
      atomicallyIO $ writeTVar appTMState $ Just (height , tm)
      await >>= handleVerifiedMessage hParam tm >>= \case
        Tranquility      -> mainLoop mCmt tm
        Misdeed          -> mainLoop mCmt tm
        Success  tm'     -> checkForCommit mCmt       tm'
        DoCommit cmt tm' -> checkForCommit (Just cmt) tm'
    --
    checkForCommit Nothing    tm = mainLoop Nothing tm
    checkForCommit (Just cmt) tm =
      case proposalByBID (smProposedBlocks tm) (commitBlockID cmt) of
        UnknownBlock    -> mainLoop (Just cmt) tm
        InvalidBlock    -> error "Trying to commit invalid block!"
        GoodBlock     b -> return (cmt, b)
        UntestedBlock b -> lift $ do
          valSet <- queryRO $ mustRetrieveValidatorSet height
          st     <- throwNothingM BlockchainStateUnavalable
                  $ bchStoreRetrieve appBchState $ pred height
          bst    <- throwLeftM
                  $ runExceptT
                  $ withExceptT TryingToCommitInvalidBlock
                  $ processBlock BChEval { bchValue        = b
                                         , validatorSet    = merkled valSet
                                         , blockchainState = st
                                         }
          return (cmt, b <$ bst)


-- Handle message and perform state transitions for both
handleVerifiedMessage
  :: (MonadLogger m, Crypto (Alg a))
  => HeightParameters m a
  -> TMState a
  -> MessageRx 'Verified a
  -> Pipe x (EngineMessage a) m (ConsensusResult a (TMState a))
handleVerifiedMessage hParam tm = \case
  RxProposal  p -> runConsesusM $ tendermintTransition hParam (ProposalMsg  p) tm
  RxPreVote   v -> runConsesusM $ tendermintTransition hParam (PreVoteMsg   v) tm
  RxPreCommit v -> runConsesusM $ tendermintTransition hParam (PreCommitMsg v) tm
  RxTimeout   t -> runConsesusM $ tendermintTransition hParam (TimeoutMsg   t) tm
  -- We update block storage
  RxBlock     b -> return $ Success $ tm { smProposedBlocks = addBlockToProps b (smProposedBlocks tm) }

-- Verify signature of message. If signature is not correct message is
-- simply discarded.
verifyMessageSignature
  :: (MonadLogger m, Crypto (Alg a))
  => ValidatorSet (Alg a)
  -> ValidatorSet (Alg a)
  -> Height
  -> Pipe (MessageRx 'Unverified a) (MessageRx 'Verified a) m r
verifyMessageSignature oldValSet valSet height = forever $ do
  await >>= \case
    RxPreVote   sv
      | h      == height -> verify "prevote"   RxPreVote   sv
      | otherwise        -> return ()
      where h = voteHeight $ signedValue sv
    RxPreCommit sv
      -- For messages from previous height we validate them against
      -- correct validator set
      | h      == height -> verify    "precommit" RxPreCommit sv
      | succ h == height -> verifyOld "precommit" RxPreCommit sv
      | otherwise        -> return ()
      where h = voteHeight $ signedValue sv
    RxProposal  sp
      | h == height -> verify "proposal"  RxProposal  sp
      | otherwise   -> return ()
      where h = propHeight $ signedValue sp
    RxTimeout   t  -> yield $ RxTimeout t
    RxBlock     b  -> yield $ RxBlock   b
  where
    verify    con = verifyAny valSet    con
    verifyOld con = verifyAny oldValSet con
    verifyAny vset name con sx = case verifySignature vset sx of
      Just sx' -> yield $ con sx'
      Nothing  -> lift $ logger WarningS "Invalid signature"
        (  sl "name" (name::Text)
        <> sl "addr" (show (signedKeyInfo sx))
        )


handleEngineMessage
  :: ( MonadIO m, MonadThrow m, MonadTMMonitoring m, MonadDB a m, MonadLogger m
     , Crypto (Alg a))
  => HeightParameters n a
  -> ConsensusCfg app
  -> AppChans a
  -> TQueue (MessageRx 'Unverified a)
  -> Consumer (EngineMessage a) m r
handleEngineMessage HeightParameters{..} ConsensusCfg{..} AppChans{..} appChanRxInternal = forever $ await >>= \case
  -- Timeout
  EngTimeout t@(Timeout h (Round r) step) -> do
    liftIO $ void $ forkIO $ do
      let calcDelay (baseT, delta) = baseT + delta * fromIntegral r
          delay = case step of
            StepNewHeight _   -> timeoutNewHeight
            StepProposal      -> calcDelay timeoutProposal
            StepPrevote       -> calcDelay timeoutPrevote
            StepPrecommit     -> calcDelay timeoutPrecommit
            StepAwaitCommit _ -> 0
      threadDelay $ 1000 * delay
      atomically $ writeTQueue appChanRxInternal $ RxTimeout t
    usingGauge prometheusHeight h
    usingGauge prometheusRound  (Round r)
  -- Misdeed
  EngMisdeed e -> mustQueryRW $ storeFreshEvidence e
  -- Announcements
  EngAnnPreVote sv -> do
    let Vote{..} = signedValue   sv
        i        = signedKeyInfo sv
    atomicallyIO $ writeTChan appChanTx $ TxAnn $ AnnHasPreVote voteHeight voteRound i
  EngAnnPreCommit sv -> do
    let Vote{..} = signedValue sv
        i        = signedKeyInfo sv
    atomicallyIO $ writeTChan appChanTx $ TxAnn $ AnnHasPreCommit voteHeight voteRound i
  EngAnnStep     s -> atomicallyIO $ writeTChan appChanTx $ TxAnn $ AnnStep s
  EngAnnLock     r -> atomicallyIO $ writeTChan appChanTx $ TxAnn $ AnnLock r
  EngAnnProposal r -> atomicallyIO $ writeTChan appChanTx $ TxAnn $ AnnHasProposal currentH r
  --
  EngCastPropose r bid lockInfo ->
    forM_ validatorKey $ \(PrivValidator pk, idx) -> do
      t <- getCurrentTime
      let prop = Proposal { propHeight    = currentH
                          , propRound     = r
                          , propTimestamp = t
                          , propPOL       = lockInfo
                          , propBlockID   = bid
                          }
      lift $ logger InfoS "Sending proposal"
        (   sl "R"    r
        <>  sl "BID" (show bid)
        )
      atomicallyIO $ do
        let p = signValue idx pk prop
        writeTQueue appChanRxInternal $ RxProposal p
        writeTChan  appChanTx         $ TxProposal p
  --
  EngCastPreVote r b ->
    forM_ validatorKey $ \(PrivValidator pk, idx) -> do
      t <- getCurrentTime
      let vote = Vote { voteHeight  = currentH
                      , voteRound   = r
                      , voteTime    = t
                      , voteBlockID = b
                      }
      lift $ logger InfoS "Sending prevote"
        (  sl "R"    r
        <> sl "bid" (show b)
        )
      atomicallyIO $ do
        let v = signValue idx pk vote
        writeTChan  appChanTx         $ TxPreVote v
        writeTQueue appChanRxInternal $ RxPreVote v
  --
  EngCastPreCommit r b ->
    forM_ validatorKey $ \(PrivValidator pk, idx) -> do
      t <- getCurrentTime
      let vote = Vote { voteHeight  = currentH
                      , voteRound   = r
                      , voteTime    = t
                      , voteBlockID = b
                      }
      lift $ logger InfoS "Sending precommit"
        (  sl "R" r
        <> sl "bid" (show b)
        )
      atomicallyIO $ do
        let v = signValue idx pk vote
        writeTChan  appChanTx         $ TxPreCommit v
        writeTQueue appChanRxInternal $ RxPreCommit v


----------------------------------------------------------------
-- Concrete implementation of ConsensusMonad
----------------------------------------------------------------

makeHeightParameters
  :: forall m a.
     ( MonadDB a m
     , MonadIO m
     , MonadThrow m
     , MonadLogger m
     , Crypto (Alg a), BlockData a)
  => Maybe (PrivValidator (Alg a))
  -> AppLogic            m a
  -> AppStore            m a
  -> AppCallbacks        m a
  -> m (HeightParameters m a)
makeHeightParameters appValidatorKey BChLogic{..} AppStore{..} AppCallbacks{appCanCreateBlock} = do
  bchH <- queryRO $ blockchainHeight
  let currentH = succ bchH
  oldValSet <- queryRO $ mustRetrieveValidatorSet bchH
  valSet    <- queryRO $ mustRetrieveValidatorSet currentH
  let ourIndex = indexByValidator valSet . publicKey . validatorPrivKey
             =<< appValidatorKey
  --
  return HeightParameters
    { hValidatorSet    = valSet
    , oldValidatorSet  = oldValSet
    , validatorKey     = liftA2 (,) appValidatorKey ourIndex
    , proposerForRound = selectProposer (proposerSelection @a) valSet currentH
    --
    , readyCreateBlock = \n ->
        queryRO (retrieveBlockReadyFromWAL currentH n) >>= \case
          Nothing -> do b <- fromMaybe True <$> appCanCreateBlock bchH
                        b <$ mustQueryRW (writeBlockReadyToWAL currentH n b)
          Just b  -> return b
    --
    , validateBlock = \props bid ->
        case proposalByBID props bid of
          UnknownBlock    -> return (id, UnseenProposal)
          InvalidBlock    -> return (id, InvalidProposal)
          GoodBlock{}     -> return (id, GoodProposal)
          UntestedBlock b -> do
            inconsistencies <- checkProposedBlock currentH b
            st              <- throwNothingM BlockchainStateUnavalable
                             $ bchStoreRetrieve appBchState bchH
            mvalSet'        <- runExceptT
                             $ processBlock BChEval { bchValue        = b
                                                    , validatorSet    = merkled valSet
                                                    , blockchainState = st
                                                    }
            evidenceState   <- queryRO $ mapM evidenceRecordedState (merkleValue $ blockEvidence b)
            evidenceOK      <- mapM evidenceCorrect (merkleValue $ blockEvidence b)
            if | not (null inconsistencies) -> do
               -- Block is not internally consistent
                   logger ErrorS "Proposed block has inconsistencies"
                     (  sl "H" currentH
                     <> sl "errors" (map show inconsistencies)
                     )
                   invalid
               -- We don't allow evidence which was already
               -- submitted. Neither we allow duplicate evidence
               | any (==Just True) evidenceState          -> invalid
               | nub (merkleValue (blockEvidence b)) /= merkleValue (blockEvidence b) -> invalid
               | any not evidenceOK                       -> invalid
               -- Block is correct and validators change is correct as
               -- well
               | Right bst <- mvalSet'
               , vals      <- validatorSet bst
               , blockStateHash b == merkleHashed (blockchainState bst)
               , validatorSetSize (merkleValue vals) > 0
               , blockNewValidators b == merkleHashed vals
                 -> return ( setProposalValidation bid (Just bst)
                           , GoodProposal
                           )
               | otherwise
                 -> invalid
               where
                 invalid = return ( setProposalValidation bid Nothing
                                  , InvalidProposal
                                  )
    --
    , createProposal = \r commit -> do
        -- Obtain block either from WAL or actually genrate it
        res@BChEval{..} <- queryRO (retrieveBlockFromWAL currentH r) >>= \case
          Just b  -> do
            st  <- throwNothingM BlockchainStateUnavalable
                 $ bchStoreRetrieve appBchState bchH
            res <- throwLeft <=< runExceptT
                 $ withExceptT InvalidBlockInWAL
                 $ processBlock BChEval { bchValue        = b
                                        , validatorSet    = merkled valSet
                                        , blockchainState = st
                                        }
            return $ b <$ res
          Nothing -> do
            lastBID <- throwNothing (DBMissingBlockID bchH) <=< queryRO
                     $ retrieveBlockID bchH
            -- Call block generator
            st <- throwNothingM BlockchainStateUnavalable
                $ bchStoreRetrieve appBchState bchH
            res@BChEval{..}
              <- throwLeft
             =<< runExceptT
               . withExceptT InvalidBlockGenerated
               . generateBlock NewBlock { newBlockHeight   = currentH
                                        , newBlockLastBID  = lastBID
                                        , newBlockCommit   = commit
                                        , newBlockEvidence = []
                                        , newBlockState    = st
                                        , newBlockValSet   = valSet
                                        }
             =<< peekNTransactions appMempool
            -- Get evidence
            evidence <- queryRO retrieveUnrecordedEvidence
            -- Assemble proper block
            let block = Block
                  { blockHeight        = currentH
                  , blockPrevBlockID   = Just lastBID
                  , blockValidators    = hashed  valSet
                  , blockNewValidators = merkleHashed validatorSet
                  , blockPrevCommit    = merkled <$> commit
                  , blockEvidence      = merkled evidence
                  , blockData          = merkled bchValue
                  , blockStateHash     = merkleHashed blockchainState
                  }
            mustQueryRW $ writeBlockToWAL r block
            return $ block <$ res
        --
        let bid = blockHash bchValue
        return ( setProposalValidation bid (Just $ () <$ res)
               . addBlockToProps bchValue
               . acceptBlockID r bid
               , bid
               )
    , ..
    }

evidenceCorrect
  :: forall m a. (Crypto (Alg a), BlockData a, MonadIO m, MonadReadDB a m)
  => ByzantineEvidence a -> m Bool
evidenceCorrect evidence = do
  queryRO (retrieveValidatorSet h) >>= \case
    -- We don't have validator set (vote from future)
    Nothing   -> return False
    Just vals -> return $ case evidence of
      OutOfTurnProposal    p
        | Nothing <- verifySignature vals p                   -> False
        | choice vals propHeight propRound == signedKeyInfo p -> False
        | otherwise                                           -> True
        where
          choice       = selectProposer (proposerSelection @a)
          Proposal{..} = signedValue p
      ConflictingPreVote   v1 v2
        | ((/=) `on` voteHeight  . signedValue) v1 v2 -> False
        | ((/=) `on` voteRound   . signedValue) v1 v2 -> False
        | ((==) `on` voteBlockID . signedValue) v1 v2 -> False
        | v1 >= v2                                    -> False
        | Nothing <- verifySignature vals v1          -> False
        | Nothing <- verifySignature vals v2          -> False
        | otherwise                                   -> True
      ConflictingPreCommit v1 v2
        | ((/=) `on` voteHeight  . signedValue) v1 v2 -> False
        | ((/=) `on` voteRound   . signedValue) v1 v2 -> False
        | ((==) `on` voteBlockID . signedValue) v1 v2 -> False
        | v1 >= v2                                    -> False
        | Nothing <- verifySignature vals v1          -> False
        | Nothing <- verifySignature vals v2          -> False
        | otherwise                                   -> True
  where
    h = case evidence of
          OutOfTurnProposal    p   -> propHeight (signedValue p)
          ConflictingPreVote   v _ -> voteHeight (signedValue v)
          ConflictingPreCommit v _ -> voteHeight (signedValue v)
