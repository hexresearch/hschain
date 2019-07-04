-- | Tests for consensus
--
{-# LANGUAGE DataKinds #-}
module TM.Consensus (tests) where


import Control.Monad.Catch
import Control.Monad.IO.Class
import Data.IORef

import Thundermint.Blockchain.Internal.Engine.Types
import Thundermint.Control
import Thundermint.Run
import Thundermint.Types.Blockchain
import Thundermint.Utils

import Test.Tasty
import Test.Tasty.HUnit

import TM.Util.Network


-- | В этом тесте одна из четырёх нод работает медленно
--   консенсус есть, БЧ растёт.
--
testConsensusLightByzantine1 :: IO ()
testConsensusLightByzantine1 = do
    achievedHeight <- newIORef (Height 0)
    runConcurrently
        [ createGossipTestNetwork
            [ byz $ \vote -> liftIO (waitSec 3.0) >> return (Just vote)
            , mempty
            , mempty
            , waitHeight (Height 2) achievedHeight
            ]
        , waitSec 10
        ]
    readIORef achievedHeight >>= assertBool "Must achieve some height" . (>= Height 1)


-- | В этом тесте одна из четырёх нод работает медленно
--   консенсус есть, БЧ растёт.
--
testConsensusLightByzantine2 :: IO ()
testConsensusLightByzantine2 = do
    achievedHeight <- newIORef (Height 0)
    runConcurrently
        [ createGossipTestNetwork
            [ byz $ \_ -> return Nothing
            , mempty
            , mempty
            , waitHeight (Height 2) achievedHeight
            ]
        , waitSec 10
        ]
    readIORef achievedHeight >>= assertBool "Must achieve some height" . (>= Height 1)


-- | В этом тесте одна из четырёх нод выдаёт неправильный голос;
--   консенсус есть, БЧ растёт.
--
testConsensusLightByzantine3 :: IO ()
testConsensusLightByzantine3 = do
    achievedHeight <- newIORef (Height 0)
    runConcurrently
        [ createGossipTestNetwork
            [ byz $ \vote -> return (Just $ vote { voteHeight = Height 555 })
            , byz $ \vote -> return (Just $ vote { voteHeight = Height 666 })
            , mempty
            , waitHeight (Height 2) achievedHeight
            ]
        , waitSec 5
        ]
    readIORef achievedHeight >>= assertEqual "Must stay on initial height" (Height 0)


-- | В этом тесте две из четырёх нод работает медленно;
--   консенсуса не образуется и поэтому сеть остаётся на исходной высоте.
--
testConsensusLightByzantine4 :: IO ()
testConsensusLightByzantine4 = do
    achievedHeight <- newIORef (Height 0)
    runConcurrently
        [ createGossipTestNetwork
            [ byz $ \vote -> liftIO (waitSec 3.0) >> return (Just vote)
            , byz $ \vote -> liftIO (waitSec 3.0) >> return (Just vote)
            , mempty
            , waitHeight (Height 2) achievedHeight
            ]
        , waitSec 5
        ]
    readIORef achievedHeight >>= assertEqual "Must stay on initial height" (Height 0)


-- | В этом тесте две из четырёх нод пропускают выдачу голоса;
--   консенсуса не образуется и поэтому сеть остаётся на исходной высоте.
--
testConsensusLightByzantine5 :: IO ()
testConsensusLightByzantine5 = do
    achievedHeight <- newIORef (Height 0)
    runConcurrently
        [ createGossipTestNetwork
            [ byz $ \_ -> return Nothing
            , byz $ \_ -> return Nothing
            , mempty
            , waitHeight (Height 2) achievedHeight
            ]
        , waitSec 5
        ]
    readIORef achievedHeight >>= assertEqual "Must stay on initial height" (Height 0)


-- | В этом тесте две из четырёх нод выдают не тот голос;
--   консенсуса не образуется и поэтому сеть остаётся на исходной высоте.
--
testConsensusLightByzantine6 :: IO ()
testConsensusLightByzantine6 = do
    achievedHeight <- newIORef (Height 0)
    runConcurrently
        [ createGossipTestNetwork
            [ byz $ \vote -> return (Just $ vote { voteHeight = Height 555 })
            , byz $ \vote -> return (Just $ vote { voteHeight = Height 666 })
            , mempty
            , waitHeight (Height 2) achievedHeight
            ]
        , waitSec 5
        ]
    readIORef achievedHeight >>= assertEqual "Must stay on initial height" (Height 0)


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
tests = testGroup "eigen-consensus"
    [ testCase "Some byzantine 1" testConsensusLightByzantine1
    , testCase "Some byzantine 2" testConsensusLightByzantine2
    , testCase "Some byzantine 3" testConsensusLightByzantine3
    , testCase "Some byzantine 4" testConsensusLightByzantine4
    , testCase "Some byzantine 5" testConsensusLightByzantine5
    , testCase "Some byzantine 6" testConsensusLightByzantine6
    ]

