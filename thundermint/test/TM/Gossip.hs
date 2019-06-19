-- | Tests for gossip
--
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE ViewPatterns      #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE ScopedTypeVariables   #-}
module TM.Gossip (tests) where


import Control.Concurrent.Chan
import Control.Concurrent.STM.TBQueue
import Control.Concurrent.STM.TChan
import Control.Concurrent.STM.TVar
import Control.Monad.Catch
import Control.Monad
import Control.Monad.Fail
import Control.Monad.Fix
import Control.Monad.IO.Class
import Control.Monad.STM
import Data.Proxy (Proxy(..))
import Prelude as P
import qualified Data.List.NonEmpty as NE
import qualified Data.Map           as Map
import qualified Data.Set as Set

import Thundermint.Blockchain.Internal.Engine.Types
import Thundermint.Blockchain.Internal.Types
import Thundermint.Control
import Thundermint.Crypto
import Thundermint.Crypto.Ed25519
import Thundermint.Debug.Trace
import Thundermint.Logger
import Thundermint.Mock.Types
import Thundermint.P2P
import Thundermint.P2P.PeerState
import Thundermint.Run
import Thundermint.Store
import Thundermint.Store.Internal.BlockDB (storeCommit, storeValSet)
import Thundermint.Store.STM
import Thundermint.Types.Blockchain
import Thundermint.Types.Validators
import Thundermint.Utils

import Thundermint.Crypto.Containers

import Test.Tasty
import Test.Tasty.HUnit

import TM.Util.Network
import qualified Thundermint.Mock.KeyVal as Mock


-- TODO:
--
-- 2. Тест, который проверяет, что при изменении подписи не будет работать
-- 3. Тест, считающий количество лагов с одной  отстающей вершиной
-- 4. Проверить, что вызываются соответствующие event'ы
-- 5. Отдельный тест, который тестирует только функцию peerGossipVotes непосредственно
-- 6. Добиться 100% покрытия peerGossipVotes
-- 7. Добиться 100% покрытия P2P.hs
-- 
-- 8. Перенести useful-функции в отдельный модуль
--
-- КОЕНЦ.


-- *


testRawGossipUnknown :: IO ()
testRawGossipUnknown =
    withGossipEnv $ \peerStateObj peerChans gossipCh _env -> do
        runConcurrently
            [ peerGossipVotes peerStateObj peerChans gossipCh
            , waitSec 0.5
            ]
        -- TODO проверить, что ничего не поменялось


-- | Тест: если пир отстаёт от текущей ноды,
--   то его состояние -- Lagging, и текущая нода
--   посылает пиру голоса с предыдущих и текущей высот.
--
testRawGossipLagging :: IO ()
testRawGossipLagging = do
    withGossipEnv $ \peerStateObj peerChans gossipCh env@GossipEnv{..} -> do
        addSomeBlocks env 10
        ourH  <- succ <$> queryRO blockchainHeight
        liftIO $ putStrLn ("ourH: " ++ show ourH)
        advancePeer peerStateObj (FullStep (pred ourH) (Round 0) StepNewHeight)
        -- Запустить peerGossipVotes
        catchTestError $ runConcurrently
            [ peerGossipVotes peerStateObj peerChans gossipCh
            , waitForEvents envEventsQueue [(TepgvStarted, 1), (TepgvNewIter, 3), (TepgvLagging, 3)]
            , waitSec 0.5 >> throwM (TestError "Timeout!")
            ]
        -- Проверить, что отослано сообщение о прекоммите
        liftIO $ do
            atomically (tryReadTBQueue gossipCh) >>= \case
                Nothing                  -> assertFailure "`gossipCh` does not contains any message!"
                Just (GossipPreCommit _) -> return ()
                msg                      -> assertFailure ("Wrong message: " ++ show msg)
  where
    catchTestError act = catch act (\err@(TestError _) -> P.fail $ show err)


-- | Тест: если пир впереди от текущей ноды,
--   то его состояние -- Ahead. Ничего не меняется,
--   пир просто ждёт, когда его нагонят.
--
testRawGossipAhead :: IO ()
testRawGossipAhead = do
    withGossipEnv $ \peerStateObj peerChans gossipCh env@GossipEnv{..} -> do
        addSomeBlocks env 10
        ourH  <- succ <$> queryRO blockchainHeight
        liftIO $ putStrLn ("ourH: " ++ show ourH)
        advancePeer peerStateObj (FullStep (succ ourH) (Round 0) StepNewHeight)
        -- Запустить peerGossipVotes
        catchTestError $ runConcurrently
            [ peerGossipVotes peerStateObj peerChans gossipCh
            , waitForEvents envEventsQueue [(TepgvStarted, 1), (TepgvNewIter, 3), (TepgvAhead, 3)]
            , waitSec 0.5 >> throwM (TestError "Timeout!")
            ]
        -- Проверить, что отослано сообщение о прекоммите
        liftIO $
            atomically (tryReadTBQueue gossipCh) >>= \case
                Nothing  -> return ()
                Just msg -> assertFailure $ "`gossipCh` contains some message: " ++ show msg ++ "!"
  where
    catchTestError act = catch act (\err@(TestError _) -> P.fail $ show err)


-- | Тест: TODO
--
testRawGossipCurrentNoAction1 :: IO ()
testRawGossipCurrentNoAction1 = do
    withGossipEnv $ \peerStateObj peerChans gossipCh env@GossipEnv{..} -> do
        addSomeBlocks env 10
        ourH  <- succ <$> queryRO blockchainHeight
        liftIO $ putStrLn ("ourH: " ++ show ourH)
        advancePeer peerStateObj (FullStep ourH (Round 0) StepNewHeight)
        -- Запустить peerGossipVotes
        catchTestError $ runConcurrently
            [ peerGossipVotes peerStateObj peerChans gossipCh
            , waitForEvents envEventsQueue [(TepgvStarted, 1), (TepgvNewIter, 3), (TepgvCurrent, 3)]
            , waitSec 0.5 >> throwM (TestError "Timeout!")
            ]
        -- Проверить, что отослано сообщение о прекоммите
        liftIO $
            atomically (tryReadTBQueue gossipCh) >>= \case
                Nothing  -> return ()
                Just msg -> assertFailure $ "`gossipCh` contains some message: " ++ show msg ++ "!"
  where
    catchTestError act = catch act (\err@(TestError _) -> P.fail $ show err)


-- | Тест: TODO
--
testRawGossipCurrentNoAction2 :: IO ()
testRawGossipCurrentNoAction2 = do
    withGossipEnv $ \peerStateObj peerChans gossipCh env@GossipEnv{..} -> do
        addSomeBlocks env 10
        ourH  <- succ <$> queryRO blockchainHeight
        liftIO $ putStrLn ("ourH: " ++ show ourH)
        advancePeer peerStateObj (FullStep ourH (Round 0) StepNewHeight)
        newTMState env (pred ourH) id
        -- Запустить peerGossipVotes
        catchTestError $ runConcurrently
            [ peerGossipVotes peerStateObj peerChans gossipCh
            , waitForEvents envEventsQueue [(TepgvStarted, 1), (TepgvNewIter, 3), (TepgvCurrent, 3)]
            , waitSec 0.5 >> throwM (TestError "Timeout!")
            ]
        -- Проверить, что отослано сообщение о прекоммите
        liftIO $
            atomically (tryReadTBQueue gossipCh) >>= \case
                Nothing  -> return ()
                Just msg -> assertFailure $ "`gossipCh` contains some message: " ++ show msg ++ "!"
  where
    catchTestError act = catch act (\err@(TestError _) -> P.fail $ show err)

-- | Тест: TODO
--
testRawGossipCurrentCurrent :: IO ()
testRawGossipCurrentCurrent = do
    withGossipEnv $ \peerStateObj peerChans gossipCh env@GossipEnv{..} -> do
        (_lastBlock, lastCommit) <- last <$> addSomeBlocks' env 10
        ourH  <- succ <$> queryRO blockchainHeight
        liftIO $ putStrLn ("ourH: " ++ show ourH)
        advancePeer peerStateObj (FullStep ourH (Round 0) StepNewHeight)
        currentTime <- getCurrentTime
        let proposal = Proposal { propHeight = ourH
                                , propRound = Round 0
                                , propTimestamp = currentTime
                                , propPOL = Nothing
                                , propBlockID = commitBlockID lastCommit
                                }
        newTMState env ourH $ \tm -> tm { smProposals = Map.singleton (Round 0) (signValue privK proposal) }
        prePeerState <- (getPeerState peerStateObj >>= \case
                Current cp -> return cp
                _ -> liftIO $ assertFailure "Wrong initial state")
        -- Запустить peerGossipVotes
        catchTestError $ runConcurrently
            [ peerGossipVotes peerStateObj peerChans gossipCh
            , waitForEvents envEventsQueue [(TepgvStarted, 1), (TepgvNewIter, 2), (TepgvCurrent, 1)]
            , waitSec 0.5 >> throwM (TestError "Timeout!")
            ]
        -- Проверить, что отослано сообщение о пропозале
        liftIO $
            atomically (tryReadTBQueue gossipCh) >>= \case
                Nothing -> assertFailure "`gossipCh` does not contains any message!"
                Just (GossipProposal (signedValue -> sentProposal)) ->
                    proposal @=? sentProposal
                msg -> assertFailure ("Wrong message: " ++ show msg)
        -- Проверить, что список обновился, а остальные структуры -- нет
        getPeerState peerStateObj >>= liftIO . \case
            Current ppp -> ppp @?= (prePeerState { peerProposals = Set.insert (Round 0) (peerProposals prePeerState) })
            s           -> assertFailure ("Wrong state: " ++ show s)
  where
    catchTestError act = catch act (\err@(TestError _) -> P.fail $ show err)






-- | Тест: TODO
--
internalTestRawGossipCurrentCurrent :: Bool -> Bool -> Bool -> IO ()
internalTestRawGossipCurrentCurrent isTestingSendProposals isTestingSendPrevotes isTestingSendPrecommits = do
    withGossipEnv $ \peerStateObj peerChans gossipCh env@GossipEnv{..} -> do
        (_lastBlock, lastCommit) <- last <$> addSomeBlocks' env 10
        ourH  <- succ <$> queryRO blockchainHeight
        liftIO $ putStrLn ("ourH: " ++ show ourH)
        advancePeer peerStateObj (FullStep ourH (Round 0) StepNewHeight)
        currentTime <- getCurrentTime
        let proposal = Proposal { propHeight = ourH
                                , propRound = Round 0
                                , propTimestamp = currentTime
                                , propPOL = Nothing
                                , propBlockID = commitBlockID lastCommit
                                }
        let vote = Vote { voteHeight = ourH
                        , voteRound = Round 0
                        , voteTime = currentTime
                        , voteBlockID = Just (commitBlockID lastCommit)
                        }
        newTMState env ourH $
            (\tm -> if isTestingSendProposals then
                        tm { smProposals = Map.singleton (Round 0) (signValue privK proposal) }
                    else tm) .
            (\tm -> if isTestingSendPrevotes then
                        case addSignedValue (Round 0) (signValue privK vote) (smPrevotesSet tm) of
                            InsertOK votes -> tm { smPrevotesSet = votes }
                            _ -> error "Can't insert votes"
                    else tm) .
            (\tm -> if isTestingSendPrecommits then
                        case addSignedValue (Round 0) (signValue privK vote) (smPrecommitsSet tm) of
                            InsertOK votes -> tm { smPrecommitsSet = votes }
                            _ -> error "Can't insert votes"
                    else tm)
        prePeerState <- (getPeerState peerStateObj >>= \case
                Current cp -> return cp
                _ -> liftIO $ assertFailure "Wrong initial state")
        -- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        -- Запустить peerGossipVotes
        catchTestError $ runConcurrently
            [ peerGossipVotes peerStateObj peerChans gossipCh
            , waitForEvents envEventsQueue [(TepgvStarted, 1), (TepgvNewIter, 2), (TepgvCurrent, 1)]
            , waitSec 0.5 >> throwM (TestError "Timeout!")
            ]
        -- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        -- Проверить, что отосланы нужные сообщения
        liftIO $ do
            messages <- atomically $ flushTBQueue gossipCh
            let expectedNumberOfMessages = sum $ map fromEnum [isTestingSendProposals, isTestingSendPrevotes, isTestingSendPrecommits]
            assertEqual "" expectedNumberOfMessages (length messages)
            assertBool "" $ (if isTestingSendProposals then id else not) $ (flip any) messages $ \case
                    GossipProposal (signedValue -> sentProposal) -> proposal == sentProposal
                    _ -> False
            assertBool "" $ (if isTestingSendPrevotes then id else not) $ (flip any) messages $ \case
                    GossipPreVote (signedValue -> sentPrevote) -> vote == sentPrevote
                    _ -> False
            assertBool "" $ (if isTestingSendPrecommits then id else not) $ (flip any) messages $ \case
                    GossipPreCommit (signedValue -> sentPrecommit) -> vote == sentPrecommit
                    _ -> False
        -- Проверить, что состояние изменено нужным образом
        let expectedPeerState =
                (\ps -> if isTestingSendProposals then
                            (ps { peerProposals = Set.insert (Round 0) (peerProposals ps) })
                        else ps) $
                (\ps -> if isTestingSendPrevotes then
                            (ps { peerPrevotes   = Map.insert (Round 0) (insertValidatorIdx (ValidatorIdx 0) (emptyValidatorISet 4 )) (peerPrevotes ps) })
                        else ps) $
                (\ps -> if isTestingSendPrecommits then
                            (ps { peerPrecommits = Map.insert (Round 0) (insertValidatorIdx (ValidatorIdx 0) (emptyValidatorISet 4 )) (peerPrecommits ps) })
                        else ps) $
                prePeerState
        getPeerState peerStateObj >>= liftIO . \case
            Current ps -> ps @?= expectedPeerState
            s          -> assertFailure ("Wrong state: " ++ show s)
  where
    catchTestError act = catch act (\err@(TestError _) -> P.fail $ show err)


testRawGossipCurrent1, testRawGossipCurrent2,
  testRawGossipCurrent3, testRawGossipCurrent4,
  testRawGossipCurrent5, testRawGossipCurrent6,
  testRawGossipCurrent7, testRawGossipCurrent8  :: IO ()

testRawGossipCurrent1 = internalTestRawGossipCurrentCurrent False False False
testRawGossipCurrent2 = internalTestRawGossipCurrentCurrent True  False False
testRawGossipCurrent3 = internalTestRawGossipCurrentCurrent False True  False
testRawGossipCurrent4 = internalTestRawGossipCurrentCurrent False False True
testRawGossipCurrent5 = internalTestRawGossipCurrentCurrent True  True  False
testRawGossipCurrent6 = internalTestRawGossipCurrentCurrent True  False True
testRawGossipCurrent7 = internalTestRawGossipCurrentCurrent False True  True
testRawGossipCurrent8 = internalTestRawGossipCurrentCurrent True  True  True

-- * Some useful utilities ------------------------


data GossipEnv = GossipEnv
    { envValidatorSet :: ValidatorSet TestAlg
    , envEventsQueue  :: Chan TraceEvents
    , envConsensus    :: TVar (Maybe (Height, TMState TestAlg TestBlock))
    }


withGossipEnv
    :: (    forall m . (MonadDB m TestAlg TestBlock, MonadMask m, MonadIO m, MonadLogger m, MonadTrace m, MonadFork m, MonadFail m)
            => PeerStateObj m TestAlg TestBlock
            -> PeerChans m TestAlg TestBlock
            -> TBQueue (GossipMsg TestAlg TestBlock)
            -> GossipEnv
            -> m ()
       )
    -> IO ()
withGossipEnv fun = do
    let dbValidatorSet = makeValidatorSetFromPriv testValidators
    eventsQueue <- newChan
    --
    withConnection ":memory:" $ \conn -> do
        initDatabase conn Proxy (Mock.genesisBlock dbValidatorSet) dbValidatorSet
        runTracerT (writeChan eventsQueue) . runNoLogsT . runDBT conn $ do
            --
            proposalStorage <- newSTMPropStorage
            peerStateObj <- newPeerStateObj (makeReadOnlyPS proposalStorage)
            -- let peerId = 0xDEADC0DE
            let peerChanRx = \_ -> return ()
            peerChanTx              <- liftIO $ newTChanIO
            peerChanPex             <- liftIO newBroadcastTChanIO
            peerChanPexNewAddresses <- liftIO newTChanIO
            consensusState'         <- liftIO (newTVarIO Nothing)
            let consensusState      =  readTVar consensusState'
            cntGossipPrevote        <- newCounter
            cntGossipPrecommit      <- newCounter
            cntGossipProposals      <- newCounter
            cntGossipBlocks         <- newCounter
            cntGossipTx             <- newCounter
            cntGossipPex            <- newCounter
            let peerChans = PeerChans { proposalStorage = makeReadOnlyPS proposalStorage
                                      , p2pConfig       = cfgNetwork (defCfg :: Configuration Example)
                                      , ..
                                      }
            gossipCh <- liftIO $ newTBQueueIO 1000
            --
            let genv = GossipEnv dbValidatorSet eventsQueue consensusState'
            --
            fun peerStateObj peerChans gossipCh genv


privK :: PrivKey Ed25519_SHA512
privK = validatorPrivKey $ snd $ head $ Map.toList testValidators


addSomeBlocks
    :: (MonadIO m, MonadFail m, MonadDB m TestAlg TestBlock)
    => GossipEnv
    -> Int -- ^ Number of blocks to add
    -> m ()
addSomeBlocks env blocksCount = void $ addSomeBlocks' env blocksCount


addSomeBlocks'
    :: (MonadIO m, MonadFail m, MonadDB m TestAlg TestBlock)
    => GossipEnv
    -> Int -- ^ Number of blocks to add
    -> m [(Block TestAlg TestBlock, Commit TestAlg TestBlock)]
addSomeBlocks' GossipEnv{..} blocksCount =
    mapM addOneBlock
         [ [(show i, NetAddrV6 (fromIntegral $ i `mod` 256, 2, 3, 4) 4433)]
         | i <- [1..blocksCount] ]
  where
    addOneBlock tx = do
        t  <- getCurrentTime
        h  <- queryRO blockchainHeight
        -- st <- stateAtH bchState (succ h) -- TODO WTF?
        let block = Block
                { blockHeader = Header
                    { headerChainID        = "TEST"
                    , headerHeight         = succ h
                    , headerTime           = t
                    , headerLastBlockID    = Nothing           -- TODO get from previous block
                    , headerValidatorsHash = Hashed $ Hash ""
                    , headerDataHash       = hashed tx
                    , headerValChangeHash  = hashed []
                    , headerLastCommitHash = Hashed $ Hash ""
                    , headerEvidenceHash   = hashed []
                    }
                , blockData       = tx
                , blockLastCommit = Nothing
                , blockEvidence   = []
                , blockValChange  = []
                }
            bid = blockHash block
            commit = Commit { commitBlockID    = bid
                            , commitPrecommits = NE.fromList [signValue privK Vote
                                      { voteHeight  = succ h
                                      , voteRound   = Round 0
                                      , voteTime    = t
                                      , voteBlockID = Just bid
                                      }]}
        Just () <- queryRW $ storeCommit commit block
        Just () <- queryRW $ storeValSet block envValidatorSet
        return (block, commit)


-- | Wait for events; fail if another events occurs
--
waitForEvents :: (MonadIO m, MonadFail m, MonadThrow m)
              => Chan TraceEvents
              -> [(PeerGossipVotesTraceEvents, Int)]
              -> m ()
waitForEvents queue events = liftIO $
    flip fix (Map.fromList events) $ \next residualEvents ->
        if Map.null residualEvents then
            return ()
        else
            readChan queue >>= \case
                TePeerGossipVotes v ->
                    case Map.updateLookupWithKey (\_ n -> if n == 1 then Nothing else Just (pred n)) v residualEvents of
                        (Just _, re) -> next re
                        a            -> throwM $ TestError ("Excessive event: " ++ show v ++ ", residual events: " ++ show a)
                _ -> next residualEvents


newTMState :: (MonadIO m, MonadFail m, MonadThrow m)
           => GossipEnv
           -> Height
           -> (TMState TestAlg TestBlock -> TMState TestAlg TestBlock)
           -> m ()
newTMState GossipEnv{..} h postProcess = do
    currentTime <- getCurrentTime
    let voteSet = newHeightVoteSet envValidatorSet currentTime
    let tmState = postProcess $ TMState (Round 0) StepNewHeight Map.empty voteSet voteSet Nothing Nothing
    liftIO $ atomically $ writeTVar envConsensus (Just (h, tmState))


data TestError = TestError String
  deriving Show
instance Exception TestError


tests :: TestTree
tests = testGroup "eigen-gossip"
    [ testCase "unknown"             testRawGossipUnknown
    , testCase "lagging"             testRawGossipLagging
    , testCase "ahead"               testRawGossipAhead
    , testCase "current no action 1" testRawGossipCurrentNoAction1
    , testCase "current no action 2" testRawGossipCurrentNoAction2
    , testCase "current current"     testRawGossipCurrentCurrent
    , testCase "current_1"            testRawGossipCurrent1
    , testCase "current_2"            testRawGossipCurrent2
    , testCase "current_3"            testRawGossipCurrent3
    , testCase "current_4"            testRawGossipCurrent4
    , testCase "current_5"            testRawGossipCurrent5
    , testCase "current_6"            testRawGossipCurrent6
    , testCase "current_7"            testRawGossipCurrent7
    , testCase "current_8"            testRawGossipCurrent8
    ]
