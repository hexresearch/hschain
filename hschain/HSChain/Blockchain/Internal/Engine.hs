{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE NumDecimals       #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TupleSections     #-}
-- |
-- Core of blockchain application. This module provides function which
-- continuously updates blockchain using consensus algorithm and
-- communicates with outside world using STM channels.
module HSChain.Blockchain.Internal.Engine (
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
import           Data.Monoid   ((<>))
import Data.Text             (Text)
import Pipes                 (Pipe,Producer,Consumer,runEffect,yield,await,(>->))
import qualified Pipes.Prelude as Pipes

import HSChain.Blockchain.Internal.Engine.Types
import HSChain.Blockchain.Internal.Types
import HSChain.Blockchain.Internal.Algorithm
import HSChain.Types.Blockchain
import HSChain.Control (throwNothing,throwNothingM,iterateM,atomicallyIO)
import HSChain.Crypto
import HSChain.Exceptions
import HSChain.Logger
import HSChain.Store
import HSChain.Store.STM
import HSChain.Store.Internal.BlockDB
import HSChain.Monitoring
import HSChain.Types.Validators

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

rewindBlockchainState
  :: ( MonadReadDB m alg a, MonadIO m, MonadThrow m
     , Crypto alg, Serialise a)
  => AppLogic m alg a
  -> m ()
rewindBlockchainState AppLogic{appBchState,appValidationFun} = do
  hChain   <- queryRO blockchainHeight
  (h0,st0) <- bchCurrentState appBchState >>= \case
    (Nothing, s) -> return (Height 0, s)
    (Just h,  s) -> return (succ h  , s)
  void $ foldr (>=>) return
    (interpretBlock <$> [h0 .. hChain])
    st0
  where
    interpretBlock h st = do
      blk      <- queryRO $ mustRetrieveBlock h
      valSet   <- queryRO $ mustRetrieveValidatorSet (succ h)
      bst      <- throwNothingM (ImpossibleError)
                $ appValidationFun blk (BlockchainState st valSet)
      let st' = blockchainState bst
      bchStoreStore appBchState h st'
      return st'

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
  rewindBlockchainState appSt
  height <- queryRO $ blockchainHeight
  lastCm <- queryRO $ retrieveLocalCommit height
  iterateM lastCm $ fmap Just
                  . decideNewBlock config appValidatorKey appSt appCall appCh

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
  hParam  <- makeHeightParameters appValidatorKey appLogic appCall appPropStorage
  resetPropStorage appPropStorage $ currentH hParam
  -- Run consensus engine
  (cmt, block, bchSt) <- runEffect $ do
    let sink = handleEngineMessage hParam config appCh
    tm0 <-  newHeight hParam lastCommt
        >-> sink
    rxMessageSource hParam appCh
        >-> msgHandlerLoop hParam appLogic appCh tm0
        >-> sink
  -- Update metrics
  do let nTx = maybe 0 (length . commitPrecommits) (blockLastCommit block)
         h   = headerHeight $ blockHeader block
     logger InfoS "Actual commit" $ LogBlockInfo h (blockData block) nTx
     usingCounter prometheusNTx nTx
  -- We have decided which block we want to commit so let commit it
  mustQueryRW $ storeCommit cmt block (bChValidatorSet bchSt)
  bchStoreStore appBchState (headerHeight $ blockHeader block) $ blockchainState bchSt
  appCommitCallback block
  return cmt

-- Producer for MessageRx. First we replay WAL and then we read
-- messages from channels.
rxMessageSource
  :: ( MonadIO m, MonadDB m alg a, MonadLogger m, MonadThrow m
     , Crypto alg, BlockData a)
  => HeightParameters m alg a
  -> AppChans m alg a
  -> Producer (MessageRx 'Verified alg a) m r
rxMessageSource HeightParameters{..} AppChans{..} = do
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
    verify  = verifyMessageSignature oldValidatorSet validatorSet currentH
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
  :: ( MonadReadDB m alg a, MonadThrow m, MonadIO m, MonadLogger m
     , Crypto alg)
  => HeightParameters m alg a
  -> AppLogic m alg a
  -> AppChans m alg a
  -> TMState alg a
  -> Pipe (MessageRx 'Verified alg a) (EngineMessage alg a) m
      (Commit alg a, Block alg a, BlockchainState alg a)
msgHandlerLoop hParam AppLogic{..} AppChans{..} = mainLoop Nothing
  where
    height = currentH hParam
    mainLoop mCmt tm = do
      -- Make current state of consensus available for gossip
      atomicallyIO $ writeTVar appTMState $ Just (height , tm)
      await >>=  handleVerifiedMessage appPropStorage hParam tm >>= \case
        Tranquility      -> mainLoop mCmt tm
        Misdeed  _       -> mainLoop mCmt tm
        Success  tm'     -> checkForCommit mCmt       tm'
        DoCommit cmt tm' -> checkForCommit (Just cmt) tm'
    --
    checkForCommit Nothing    tm = mainLoop Nothing tm
    checkForCommit (Just cmt) tm = do
      mBlk <- lift
            $ retrievePropByID appPropStorage height (commitBlockID cmt)
      case mBlk of
        UnknownBlock    -> mainLoop (Just cmt) tm
        InvalidBlock    -> error "Trying to commit invalid block!"
        GoodBlock b bst -> return (cmt, b, bst)
        UntestedBlock b -> lift $ do
          valSet <- queryRO $ mustRetrieveValidatorSet height
          st     <- throwNothingM BlockchainStateUnavalable
                  $ bchStoreRetrieve appBchState $ pred height
          appValidationFun b (BlockchainState st valSet) >>= \case
            Nothing  -> error "Trying to commit invalid block!"
            Just bst -> return (cmt, b, bst)


-- Handle message and perform state transitions for both
handleVerifiedMessage
  :: (MonadLogger m, Crypto alg)
  => ProposalStorage 'RW m alg a
  -> HeightParameters m alg a
  -> TMState alg a
  -> MessageRx 'Verified alg a
  -> Pipe x (EngineMessage alg a) m (ConsensusResult alg a (TMState alg a))
handleVerifiedMessage ProposalStorage{..} hParam tm = \case
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
  => Maybe (ValidatorSet alg)
  -> ValidatorSet alg
  -> Height
  -> Pipe (MessageRx 'Unverified alg a) (MessageRx 'Verified alg a) m r
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
    verify    con = verifyAny (Just valSet) con
    verifyOld con = verifyAny oldValSet     con
    verifyAny mvset name con sx = case mvset >>= flip verifySignature sx of
      Just sx' -> yield $ con sx'
      Nothing  -> logger WarningS "Invalid signature"
        (  sl "name" (name::Text)
        <> sl "addr" (show (signedKeyInfo sx))
        )


handleEngineMessage
  :: ( MonadIO m, MonadTMMonitoring m, MonadLogger m
     , Crypto alg)
  => HeightParameters n alg a
  -> ConsensusCfg
  -> AppChans m alg a
  -> Consumer (EngineMessage alg a) m r
handleEngineMessage HeightParameters{..} ConsensusCfg{..} AppChans{..} = forever $ await >>= \case
  -- Timeout
  EngTimeout t@(Timeout h (Round r) step) -> do
    liftIO $ void $ forkIO $ do
      let (baseT,delta) = case step of
            StepNewHeight _   -> timeoutNewHeight
            StepProposal      -> timeoutProposal
            StepPrevote       -> timeoutPrevote
            StepPrecommit     -> timeoutPrecommit
            StepAwaitCommit _ -> (0, 0)
      threadDelay $ 1000 * (baseT + delta * fromIntegral r)
      atomically $ writeTQueue appChanRxInternal $ RxTimeout t
    usingGauge prometheusHeight h
    usingGauge prometheusRound  (Round r)
  -- Announcements
  EngAnnPreVote sv -> do
    let Vote{..} = signedValue   sv
        i        = signedKeyInfo sv
    atomicallyIO $ writeTChan appChanTx $ TxAnn $ AnnHasPreVote voteHeight voteRound i
  EngAnnPreCommit sv -> do
    let Vote{..} = signedValue sv
        i        = signedKeyInfo sv
    atomicallyIO $ writeTChan appChanTx $ TxAnn $ AnnHasPreCommit voteHeight voteRound i
  EngAnnStep s ->
    atomicallyIO $ writeTChan appChanTx $ TxAnn $ AnnStep s
  EngAnnLock r ->
    atomicallyIO $ writeTChan appChanTx $ TxAnn $ AnnLock r
  --
  EngAcceptBlock r bid -> do
    atomicallyIO $ writeTChan appChanTx $ TxAnn $ AnnHasProposal currentH r
    lift $ allowBlockID appPropStorage r bid
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
      mBlock <- lift $ retrievePropByID appPropStorage (pred currentH) bid
      logger InfoS "Sending proposal"
        (   sl "R"    r
        <>  sl "BID" (show bid)
        )
      atomicallyIO $ do
        let p = signValue idx pk prop
        writeTQueue appChanRxInternal $ RxProposal p
        writeTChan  appChanTx         $ TxProposal p
        case blockFromBlockValidation mBlock of
          Just b  -> writeTQueue appChanRxInternal (RxBlock b)
          Nothing -> return ()
  --
  EngCastPreVote r b ->
    forM_ validatorKey $ \(PrivValidator pk, idx) -> do
      t <- getCurrentTime
      let vote = Vote { voteHeight  = currentH
                      , voteRound   = r
                      , voteTime    = t
                      , voteBlockID = b
                      }
      logger InfoS "Sending prevote"
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
      logger InfoS "Sending precommit"
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
  :: ( MonadDB m alg a
     , MonadIO m
     , MonadThrow m
     , MonadLogger m
     , Crypto alg, Serialise a)
  => Maybe (PrivValidator alg)
  -> AppLogic            m alg a
  -> AppCallbacks        m alg a
  -> ProposalStorage 'RW m alg a
  -> m (HeightParameters m alg a)
makeHeightParameters appValidatorKey AppLogic{..} AppCallbacks{appCanCreateBlock} propStorage = do
  bchH <- queryRO $ blockchainHeight
  let currentH = succ bchH
  oldValSet <- queryRO $ retrieveValidatorSet     bchH
  valSet    <- queryRO $ mustRetrieveValidatorSet currentH
  let ourIndex = indexByValidator valSet . publicKey . validatorPrivKey
             =<< appValidatorKey
  --
  return HeightParameters
    { validatorSet     = valSet
    , oldValidatorSet  = oldValSet
    , validatorKey     = liftA2 (,) appValidatorKey ourIndex
      -- FIXME: this is some random algorithms that should probably
      --        work (for some definition of work)
    , proposerForRound = appProposerChoice valSet currentH
    --
    , readyCreateBlock = \n ->
        queryRO (retrieveBlockReadyFromWAL currentH n) >>= \case
          Nothing -> do b <- fromMaybe True <$> appCanCreateBlock bchH
                        b <$ mustQueryRW (writeBlockReadyToWAL currentH n b)
          Just b  -> return b
    --
    , validateBlock = \bid -> do
        retrievePropByID propStorage currentH bid >>= \case
          UnknownBlock    -> return UnseenProposal
          InvalidBlock    -> return InvalidProposal
          GoodBlock{}     -> return GoodProposal
          UntestedBlock b -> do
            let invalid = InvalidProposal <$ setPropValidation propStorage bid Nothing
            inconsistencies <- checkProposedBlock currentH b
            st              <- throwNothingM BlockchainStateUnavalable
                             $ bchStoreRetrieve appBchState bchH
            mvalSet'        <- appValidationFun b (BlockchainState st valSet)
            if | not (null inconsistencies) -> do
               -- Block is not internally consistent
                   logger ErrorS "Proposed block has inconsistencies"
                     (  sl "H" currentH
                     <> sl "errors" (map show inconsistencies)
                     )
                   invalid
               -- We don't put evidence into blocks yet so there shouldn't be any
               | _:_ <- blockEvidence b -> invalid
               -- Block is correct and validators change is correct as
               -- well
               | Just bst <- mvalSet'
               , validatorSetSize (bChValidatorSet bst) > 0
               , blockValChange b == validatorsDifference valSet (bChValidatorSet bst)
                 -> do setPropValidation propStorage bid $ Just bst
                       return GoodProposal
               | otherwise
                 -> invalid
    --
    , createProposal = \r commit -> do
        -- Obtain block either from WAL or actually genrate it
        (block,bst) <- queryRO (retrieveBlockFromWAL currentH r) >>= \case
          Just b  -> do
            st       <- throwNothingM BlockchainStateUnavalable
                      $ bchStoreRetrieve appBchState bchH
            mvalSet' <- appValidationFun b (BlockchainState st valSet)
            case mvalSet' of
              Nothing -> throwM InvalidBlockInWAL
              Just s  -> return (b, s)
          Nothing -> do
            lastBID <- throwNothing (DBMissingBlockID bchH) <=< queryRO
                     $ retrieveBlockID bchH
            -- Call block generator
            st          <- throwNothingM BlockchainStateUnavalable
                         $ bchStoreRetrieve appBchState bchH
            (bData,bst) <- appBlockGenerator NewBlock
              { newBlockHeight   = currentH
              , newBlockLastBID  = lastBID
              , newBlockCommit   = commit
              , newBlockEvidence = []
              , newBlockState    = BlockchainState st valSet
              } =<< peekNTransactions appMempool
            -- Assemble proper block
            let valCh = validatorsDifference valSet (bChValidatorSet bst)
                block = Block
                  { blockHeader = Header
                      { headerHeight         = currentH
                      , headerLastBlockID    = Just lastBID
                      , headerValidatorsHash = hashed valSet
                      , headerDataHash       = hashed bData
                      , headerValChangeHash  = hashed valCh
                      , headerLastCommitHash = hashed commit
                      , headerEvidenceHash   = hashed []
                      }
                  , blockData       = bData
                  , blockValChange  = valCh
                  , blockLastCommit = commit
                  , blockEvidence   = []
                  }
            mustQueryRW $ writeBlockToWAL r block
            return (block, bst)
        --
        let bid = blockHash block
        allowBlockID      propStorage r bid
        storePropBlock    propStorage block
        setPropValidation propStorage bid (Just bst)
        return bid
    , ..
    }
