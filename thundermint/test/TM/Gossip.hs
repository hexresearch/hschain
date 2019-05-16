-- | Tests for gossip
--
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE RecordWildCards   #-}
module TM.Gossip (tests) where


import Control.Concurrent.Chan
import Control.Concurrent.STM.TBQueue
import Control.Concurrent.STM.TChan
import Control.Concurrent.STM.TVar
import Control.Monad.Catch
import Control.Monad.Fail
import Control.Monad.Fix
import Control.Monad.IO.Class
import Control.Monad.STM
import Data.Proxy (Proxy(..))
import Prelude as P
import qualified Data.List.NonEmpty as NE
import qualified Data.Map                 as Map

import Thundermint.Blockchain.Internal.Engine.Types
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
        advancePeer peerStateObj (FullStep (pred ourH) (Round 1) StepNewHeight)
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
                v                        -> assertFailure ("Wrong message: " ++ show v)
  where
    catchTestError act = catch act (\err@(TestError _) -> P.fail $ show err)


-- * Some useful utilities ------------------------


data GossipEnv = GossipEnv
    { envValidatorSet :: ValidatorSet TestAlg
    , envEventsQueue  :: Chan TraceEvents
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
            -- appTMState              <- liftIO newTVarIO
            cntGossipPrevote        <- newCounter
            cntGossipPrecommit      <- newCounter
            cntGossipProposals      <- newCounter
            cntGossipBlocks         <- newCounter
            cntGossipTx             <- newCounter
            cntGossipPex            <- newCounter
            let peerChans = PeerChans { proposalStorage = makeReadOnlyPS proposalStorage
                                      , consensusState  = readTVar undefined -- appTMState
                                      , p2pConfig       = cfgNetwork (defCfg :: Configuration Example)
                                      , ..
                                      }
            gossipCh <- liftIO $ newTBQueueIO 1000
            --
            let genv = GossipEnv dbValidatorSet eventsQueue
            --
            fun peerStateObj peerChans gossipCh genv


privK :: PrivKey Ed25519_SHA512
privK = validatorPrivKey $ snd $ head $ Map.toList testValidators


addSomeBlocks
    :: (MonadIO m, MonadFail m, MonadDB m TestAlg TestBlock)
    => GossipEnv
    -> Int -- ^ Number of blocks to add
    -> m ()
addSomeBlocks GossipEnv{..} blocksCount =
    mapM_ addOneBlock
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
                    , headerLastBlockID    = Nothing
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
        Just () <- queryRW $ storeCommit
                       Commit { commitBlockID    = blockHash block
                              , commitPrecommits = NE.fromList [signValue privK Vote
                                      { voteHeight  = h
                                      , voteRound   = Round 0
                                      , voteTime    = t
                                      , voteBlockID = Just bid
                                      }]}
                       block
        Just () <- queryRW $ storeValSet block envValidatorSet
        return ()


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


data TestError = TestError String
  deriving Show
instance Exception TestError


tests :: TestTree
tests = testGroup "eigen-gossip"
    [ testCase "unknown"          testRawGossipUnknown
    , testCase "lagging"          testRawGossipLagging
    ]

