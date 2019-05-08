-- | Tests for gossip
--
{-# LANGUAGE DataKinds #-}
module TM.Gossip (tests) where


import Control.Monad
import Control.Monad.Catch
import Control.Monad.IO.Class
import Data.IORef
import qualified Data.Set as Set

import Test.Tasty
import Test.Tasty.HUnit

import Thundermint.Blockchain.Internal.Engine.Types
import Thundermint.Control
import Thundermint.Debug.Trace
import Thundermint.Run
import Thundermint.Types.Blockchain
import Thundermint.Utils

import TM.Util.Network
import TM.Util.Tests



-- | В этом тесте одна из четырёх нод работает медленно
--   консенсус есть, БЧ растёт.
--
testGossipLightByzantine1 :: IO ()
testGossipLightByzantine1 = do
    achievedHeight <- newIORef (Height 0)
    runConcurrently
        [ createGossipTestNetwork
            [ byz $ \vote -> liftIO (waitSec 3.0) >> return (Just vote)
            , mempty
            , mempty
            , waitHeight (Height 2) achievedHeight
            ]
        , waitSec 5
        ]
    readIORef achievedHeight >>= assertBool "Must achieve some height" . (>= Height 1)


-- | В этом тесте одна из четырёх нод работает медленно
--   консенсус есть, БЧ растёт.
--
testGossipLightByzantine2 :: IO ()
testGossipLightByzantine2 = do
    achievedHeight <- newIORef (Height 0)
    runConcurrently
        [ createGossipTestNetwork
            [ byz $ \_ -> return Nothing
            , mempty
            , mempty
            , waitHeight (Height 2) achievedHeight
            ]
        , waitSec 5
        ]
    readIORef achievedHeight >>= assertBool "Must achieve some height" . (>= Height 1)



-- | В этом тесте одна из четырёх нод выдаёт неправильный голос;
--   консенсус есть, БЧ растёт.
--
testGossipLightByzantine3 :: IO ()
testGossipLightByzantine3 = do
    achievedHeight <- newIORef (Height 0)
    runConcurrently
        [ createGossipTestNetwork
            [ byz $ \vote -> return (Just $ vote { voteHeight = Height 555 })
            , byz $ \vote -> return (Just $ vote { voteHeight = Height 666 })
            , mempty
            , waitHeight (Height 2) achievedHeight
            ]
        , waitSec 3
        ]
    readIORef achievedHeight >>= assertEqual "Must stay on initial height" (Height 0)


-- | В этом тесте две из четырёх нод работает медленно;
--   консенсуса не образуется и поэтому сеть остаётся на исходной высоте.
--
testGossipLightByzantine4 :: IO ()
testGossipLightByzantine4 = do
    achievedHeight <- newIORef (Height 0)
    runConcurrently
        [ createGossipTestNetwork
            [ byz $ \vote -> liftIO (waitSec 3.0) >> return (Just vote)
            , byz $ \vote -> liftIO (waitSec 3.0) >> return (Just vote)
            , mempty
            , waitHeight (Height 2) achievedHeight
            ]
        , waitSec 3
        ]
    readIORef achievedHeight >>= assertEqual "Must stay on initial height" (Height 0)


-- | В этом тесте две из четырёх нод пропускают выдачу голоса;
--   консенсуса не образуется и поэтому сеть остаётся на исходной высоте.
--
testGossipLightByzantine5 :: IO ()
testGossipLightByzantine5 = do
    achievedHeight <- newIORef (Height 0)
    runConcurrently
        [ createGossipTestNetwork
            [ byz $ \_ -> return Nothing
            , byz $ \_ -> return Nothing
            , mempty
            , waitHeight (Height 2) achievedHeight
            ]
        , waitSec 3
        ]
    readIORef achievedHeight >>= assertEqual "Must stay on initial height" (Height 0)


-- | В этом тесте две из четырёх нод выдают не тот голос;
--   консенсуса не образуется и поэтому сеть остаётся на исходной высоте.
--
testGossipLightByzantine6 :: IO ()
testGossipLightByzantine6 = do
    achievedHeight <- newIORef (Height 0)
    runConcurrently
        [ createGossipTestNetwork
            [ byz $ \vote -> return (Just $ vote { voteHeight = Height 555 })
            , byz $ \vote -> return (Just $ vote { voteHeight = Height 666 })
            , mempty
            , waitHeight (Height 2) achievedHeight
            ]
        , waitSec 3
        ]
    readIORef achievedHeight >>= assertEqual "Must stay on initial height" (Height 0)


-- | Проверяет, что достигются разные куски кода (TODO расширить)
testGossip :: IO ()
testGossip = do
    [events1, events2, events3, events4] <- replicateM 4 (newIORef Set.empty)
    runConcurrently
        [ createTestNetwork
            [ mkNodeDescription 1 [2, 3, 4] (collectEvents events1)
            , mkNodeDescription 2 [1, 3, 4] (collectEvents events2)
            , mkNodeDescription 3 [1, 2, 4] (collectEvents events3)
            , mkNodeDescription 4 [1, 2, 3] (collectEvents events4)
            ]
        -- TODO дождаться каждого события; чтобы ускорить проверку
        , waitSec 0.5
        ]
    readIORef events1 >>= ([TePeerGossipVotesStarted] @~<?)
    readIORef events2 >>= ([TePeerGossipVotesStarted] @~<?)
    readIORef events3 >>= ([TePeerGossipVotesStarted] @~<?)
    readIORef events4 >>= ([TePeerGossipVotesStarted] @~<?)


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


-- * Some useful utilities ------------------------

blockHeight :: Block alg a -> Height
blockHeight = headerHeight . blockHeader


byz :: (Monad m, Monoid b)
    => (Vote 'PreVote alg a -> m (Maybe (Vote 'PreVote alg a)))
    -> (AppByzantine m alg a, b)
byz byzc = (mempty { byzantineCastPrevote = Just $ byzc } , mempty)


waitHeight :: (Monoid n, MonadIO m, MonadThrow m)
           => Height
           -> IORef Height
           -> (n, AppCallbacks m alg a)
waitHeight h achievedHeightIORef =
    (mempty, mempty { appCommitCallback = liftIO . writeIORef achievedHeightIORef . blockHeight }
                       <> callbackAbortAtH h)


tests :: TestTree
tests = testGroup "eigen-gossip"
    [ testGroup "simple tests"
        [ testCase "Simple"           testGossip
        , testCase "Some byzantine 1" testGossipLightByzantine1
        , testCase "Some byzantine 2" testGossipLightByzantine2
        , testCase "Some byzantine 3" testGossipLightByzantine3
        , testCase "Some byzantine 4" testGossipLightByzantine4
        , testCase "Some byzantine 5" testGossipLightByzantine5
        , testCase "Some byzantine 6" testGossipLightByzantine6
        ]
    ]

