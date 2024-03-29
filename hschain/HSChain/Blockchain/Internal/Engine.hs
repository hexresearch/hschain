{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE MultiWayIf          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
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
import           Data.Maybe    (fromMaybe)
import           Data.List     (nub)
import Data.Aeson            (toJSON)
import Data.Text             (Text)
import Data.Function         (on)
import Pipes                 (Pipe,Producer,Consumer,runEffect,yield,await,(>->))
import qualified Pipes.Prelude as Pipes
import Katip (sl)

import HSChain.Blockchain.Internal.Algorithm
import HSChain.Blockchain.Internal.Engine.Types
import HSChain.Control.Util (throwNothing,throwLeft,atomicallyIO,iterateM)
import HSChain.Crypto
import HSChain.Exceptions
import HSChain.Internal.Types.Messages
import HSChain.Internal.Types.Config
import HSChain.Internal.Types.Consensus
import HSChain.Logger
import HSChain.Monitoring
import HSChain.Store
import HSChain.Store.Internal.BlockDB
import HSChain.Store.Internal.Proposals
import HSChain.Types.Blockchain
import HSChain.Types.Merkle.Types
import HSChain.Types.Validators



----------------------------------------------------------------
--
----------------------------------------------------------------

newAppChans :: (MonadIO m) => ConsensusCfg app -> m (AppChans view)
newAppChans ConsensusCfg{incomingQueueSize = sz} = do
  appChanRx         <- liftIO $ newTBQueueIO sz
  appChanTx         <- liftIO   newBroadcastTChanIO
  appTMState        <- liftIO $ newTVarIO Nothing
  return AppChans{..}

rewindBlockchainState
  :: ( MonadDB m, MonadIO m, MonadThrow m, MonadCached a m
     , a ~ BlockType view, StateView view
     , ViewConstraints view m)
  => view -> m view
rewindBlockchainState st0 = do
  -- We need to generate validator set for H=1. We do so in somewhat
  -- fragile way.
  --
  -- We only could have no validator set at H=1 when we just started
  -- and only wrote genesis into database. So blockchain state must be
  -- at initial state. Therefore we can write validator state during
  -- evaluation of blocks.
  hChain <- queryRO blockchainHeight
  let h0 = case stateHeight st0 of
             Nothing -> Height 0
             Just h  -> succ h
  let step st h = do
        (blk,valSet) <- queryRO $ (,) <$> mustRetrieveBlock        h
                                      <*> mustRetrieveValidatorSet h
        -- Check validator set consistency
        do let Hashed expectedValue = blockValidators blk
               Hashed actualValue   = hashed valSet
           unless (expectedValue == actualValue)
             $ throwM $ InconsistenceWhenRewinding h
                 "Validator set doesn't match validator set stored in block"
                  Mismatch{..}
        -- Validate block
        validatePropBlock st blk valSet >>= \case
          Left  e   -> throwM e
          Right st' -> do
            -- Check new validator consistency
            do let Hashed expectedValue = blockNewValidators blk
                   Hashed actualValue   = hashed $ newValidators st'
               unless (expectedValue == actualValue)
                 $ throwM $ InconsistenceWhenRewinding h
                            "New validator set doesn't match validator set stored in block"
                            Mismatch{..}
            -- Write new validator set for H=1
            when (h == Height 0) $ mustQueryRW $ do
              hasValidatorSet (Height 1) >>= \case
                True  -> return ()
                False -> storeValSet (Height 1) (merkled $ newValidators st')
            commitState st'
  --
  foldM step st0 [h0 .. hChain]


-- | Initialize blockchain: if we initialize fresh blockchain, compute
--   validator set for H=1 and
initializeBlockchain
  :: (MonadDB m, MonadCached a m, MonadThrow m, MonadIO m, BlockData a, Show a, Eq a
     , a ~ BlockType view, StateView view, ViewConstraints view m)
  => Genesis a -> view -> m view
initializeBlockchain genesis stateView  = do
  mustQueryRW $ storeGenesis genesis
  rewindBlockchainState stateView


-- | Main loop for application. Here we update state machine and
--   blockchain in response to incoming messages.
--
--   * INVARIANT: Only this function can write to blockchain
runApplication
  :: ( MonadDB m
     , MonadCached a m
     , MonadIO m
     , MonadMask m
     , MonadLogger m
     , MonadTMMonitoring m
     , BlockData a
     , a ~ BlockType view
     , StateView view
     , ViewConstraints view m
     )
  => ConsensusCfg app              -- ^ Configuration
  -> Maybe (PrivValidator (Alg a)) -- ^ Private key of validator
  -> view                          -- ^ View on blockchain state
  -> AppCallbacks m a
  -> AppChans     view             -- ^ Channels for communication with peers
  -> m ()
runApplication config appValidatorKey stateView appCall appCh = logOnException $ do
  logger InfoS "Starting consensus engine" ()
  -- Now we can start consensus
  appChanRxInternal <- liftIO newTQueueIO
  height <- queryRO $ blockchainHeight
  lastCm <- queryRO $ retrieveLocalCommit height
  iterateM (lastCm, stateView) $ \(mcm,st) -> do
    (cm',st') <- decideNewBlock config appValidatorKey st appCall appCh appChanRxInternal mcm
    return (Just cm',st')

-- This function uses consensus algorithm to decide which block we're
-- going to commit at current height, then stores it in database and
-- returns commit.
decideNewBlock
  :: forall m view a app.
     ( MonadDB m
     , MonadCached a m
     , MonadIO m
     , MonadMask m
     , MonadLogger m
     , MonadTMMonitoring m
     , Crypto (Alg a), BlockData a
     , a ~ BlockType view, StateView view, ViewConstraints view m)
  => ConsensusCfg app
  -> Maybe (PrivValidator (Alg a))
  -> view
  -> AppCallbacks m a
  -> AppChans     view
  -> TQueue (MessageRx 'Unverified a)
  -> Maybe (Commit a)
  -> m (Commit a, view)
decideNewBlock config appValidatorKey
               stateView
               appCall@AppCallbacks{..} appCh appChanRxInternal lastCommt = do
  -- Enter NEW HEIGHT and create initial state for consensus state
  -- machine
  hParam  <- makeHeightParameters appValidatorKey stateView appCall
  -- Run consensus engine
  (cmt, blk, st') <- runEffect $ do
    let sink :: Consumer (EngineMessage a) m r
        sink = handleEngineMessage hParam config appCh appChanRxInternal
    tm0 <-  newHeight hParam lastCommt
        >-> sink
    rxMessageSource hParam appCh appChanRxInternal
        >-> msgHandlerLoop hParam stateView appCh tm0
        >-> sink
  -- Update metrics
  do let nSign = maybe 0 (length . commitPrecommits . merkleValue) (blockPrevCommit blk)
         h     = blockHeight blk
     logger InfoS "Actual commit" $ LogBlockInfo h (merkleValue $ blockData blk) nSign
  -- We have decided which block we want to commit so let commit it
  st'' <- do
    let h = blockHeight blk
    mustQueryRW $ do
      storeCommit cmt      blk
      storeValSet (succ h) (merkled $ newValidators st')
      mapM_ storeBlockchainEvidence $ merkleValue $ blockEvidence blk
    appCommitCallback blk
    commitState st'
  return (cmt, st'')

  
-- Producer for MessageRx. First we replay WAL and then we read
-- messages from channels.
rxMessageSource
  :: forall m a view r.
     ( MonadIO m, MonadDB m, MonadLogger m, MonadCached a m
     , BlockData a, a ~ BlockType view)
  => HeightParameters m view
  -> AppChans view
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
    verify :: forall x. Pipe (MessageRx 'Unverified a) (MessageRx 'Verified a) m x
    verify = verifyMessageSignature oldValidatorSet hValidatorSet currentH
    -- -- NOTE: We try to read internal messages first. This is needed to
    -- --       ensure that timeouts are delivered in timely manner
    readMsg = forever $ yield =<< atomicallyIO (  readTQueue  appChanRxInternal
                                              <|> readTBQueue appChanRx)


-- Main loop of message handler and message consumer. We process every
-- message even after we decided to commit block. This is needed to
--
--  1. If we're catching up it's possible that we don't have block
--     we want to commit yet.
--  2. Collect stragglers precommits.
msgHandlerLoop
  :: ( MonadReadDB m, MonadThrow m, MonadIO m, MonadLogger m, MonadCached a m
     , a ~ BlockType view, StateView view, ViewConstraints view m)
  => HeightParameters m view
  -> view
  -> AppChans  view
  -> TMState   view
  -> Pipe (MessageRx 'Verified a) (EngineMessage a) m
      (Commit a, Block a, view)
msgHandlerLoop hParam view AppChans{..} = mainLoop Nothing
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
        UnknownBlock       -> mainLoop (Just cmt) tm
        InvalidBlock  _    -> error "Trying to commit invalid block!"
        GoodBlock     b st -> return (cmt, b, st)
        UntestedBlock b    -> lift $ do
          valSet <- queryRO $ mustRetrieveValidatorSet height
          st     <- throwLeft =<< validatePropBlock view b valSet
          return (cmt, b, st)


-- Handle message and perform state transitions for both
handleVerifiedMessage
  :: (MonadLogger m, Crypto (Alg a), a ~ BlockType view)
  => HeightParameters m view
  -> TMState view
  -> MessageRx 'Verified a
  -> Pipe x (EngineMessage a) m (ConsensusResult view (TMState view))
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
  :: forall m a r. (MonadLogger m, Crypto (Alg a))
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
    -- FIXME: No Let polymorphism
    verify :: (CryptoHashable msg)
           => Text
           -> (Signed 'Verified (Alg a) msg -> MessageRx 'Verified a)
           -> Signed 'Unverified (Alg a) msg
           -> Pipe x (MessageRx 'Verified a) m ()
    verify    con = verifyAny valSet    con
    verifyOld con = verifyAny oldValSet con

verifyAny
  :: (CryptoSign alg, CryptoHash alg, CryptoHashable a, MonadLogger m)
  => ValidatorSet alg
  -> Text
  -> (Signed 'Verified alg a -> b)
  -> Signed 'Unverified alg a
  -> Pipe x b m ()
verifyAny vset name con sx = case verifySignature vset sx of
  Just sx' -> yield $ con sx'
  Nothing  -> lift $ logger WarningS "Invalid signature"
    (  sl "name" (name::Text)
    <> sl "addr" (show (signedKeyInfo sx))
    )


handleEngineMessage
  :: ( MonadIO m, MonadTMMonitoring m, MonadDB m, MonadLogger m, MonadCached a m
     , Crypto (Alg a), a ~ BlockType view)
  => HeightParameters n view
  -> ConsensusCfg app
  -> AppChans view
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
  :: forall m a view.
     ( MonadDB m
     , MonadCached a m
     , MonadIO m
     , MonadThrow m
     , Crypto (Alg a), BlockData a
     , a ~ BlockType view, StateView view, ViewConstraints view m)
  => Maybe (PrivValidator (Alg a))
  -> view
  -> AppCallbacks        m a
  -> m (HeightParameters m view)
makeHeightParameters appValidatorKey st AppCallbacks{appCanCreateBlock} = do
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
          InvalidBlock e  -> return (id, InvalidProposal e)
          GoodBlock{}     -> return (id, GoodProposal)
          UntestedBlock b -> do
            inconsistencies <- checkProposedBlock currentH b
            validation      <- validatePropBlock st b valSet
            evidenceState   <- queryRO $ mapM evidenceRecordedState $ merkleValue $ blockEvidence b
            evidenceOK      <- mapM evidenceCorrect $ merkleValue $ blockEvidence b
               -- Block is not internally consistent
            if | not (null inconsistencies) -> invalid $ toJSON inconsistencies
               -- We don't allow evidence which was already
               -- submitted. Neither we allow duplicate evidence
               | any (==Just True) evidenceState
                 -> invalid $ toJSON ( "Already submitted evidence" :: Text
                                     , merkleValue $ blockEvidence b
                                     )
               | nub (merkleValue (blockEvidence b)) /= merkleValue (blockEvidence b)
                 -> invalid $ toJSON ( "Duplicate evidence" :: Text
                                     , merkleValue $ blockEvidence b
                                     )
               | any not evidenceOK
                 -> invalid $ toJSON ( "Invalid evidence" :: Text
                                     , merkleValue $ blockEvidence b
                                     )
               -- Block is correct and validators change is correct as
               -- well
               | otherwise -> case validation of
                   Left err  -> invalid $ toJSON ( "State processing error" :: Text
                                                 , err )
                   Right st'
                     | validatorSetSize (newValidators st') <= 0
                       -> invalid $ toJSON ( "Empty new validator set" :: Text
                                           , newValidators st'
                                           )
                     | blockNewValidators b /= hashed (newValidators st')
                       -> invalid $ toJSON ( "New validators set hash mismatch" :: Text
                                           , blockNewValidators b
                                           , newValidators st'
                                           )
                     | otherwise -> return ( setProposalValidation bid (Right st')
                                           , GoodProposal
                                           )
               where
                 invalid e = return ( setProposalValidation bid (Left e)
                                    , InvalidProposal e
                                    )
    --
    , createProposal = \r commit -> do
        -- Obtain block either from WAL or actually generate it
        (b,st') <- queryRO (retrieveBlockFromWAL currentH r) >>= \case
          Just b  -> do
            st' <- validatePropBlock st b valSet >>= \case
              Left  e -> throwM $ InvalidBlockInWAL e
              Right s -> return s
            return (b, st')
          Nothing -> do
            -- Retrieve BID & evidence
            lastBID  <- throwNothing (DBMissingBlockID bchH) <=< queryRO
                      $ retrieveBlockID bchH
            evidence <- queryRO retrieveUnrecordedEvidence
            -- Call block generator
            (dat,st') <- generateCandidate st NewBlock
              { newBlockHeight   = currentH
              , newBlockLastBID  = lastBID
              , newBlockCommit   = commit
              , newBlockEvidence = evidence
              , newBlockValSet   = valSet
              }
            -- Assemble proper block
            let block = Block
                  { blockHeight        = currentH
                  , blockPrevBlockID   = Just lastBID
                  , blockValidators    = hashed valSet
                  , blockNewValidators = hashed $ newValidators st'
                  , blockPrevCommit    = merkled <$> commit
                  , blockEvidence      = merkled evidence
                  , blockData          = merkled dat
                  }
            mustQueryRW $ writeBlockToWAL r block
            return (block, st')
        -- --
        let bid = blockHash b
        return ( setProposalValidation bid (Right st')
               . addBlockToProps b
               . acceptBlockID r bid
               , bid
               )
    , ..
    }

evidenceCorrect
  :: forall m a. (Crypto (Alg a), BlockData a, MonadIO m, MonadReadDB m, MonadCached a m)
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
