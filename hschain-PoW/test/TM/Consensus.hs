{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase       #-}
-- |
module TM.Consensus (tests) where

import Control.Monad.State.Strict
import Control.Monad.Except
import Data.Foldable
import qualified Data.Map.Strict as Map
import qualified Data.Set        as Set
import Lens.Micro
import Test.Tasty
import Test.Tasty.HUnit

import HSChain.PoW.Types
import HSChain.PoW.Consensus
import HSChain.PoW.Logger
import HSChain.PoW.Node
import HSChain.Types.Merkle.Types
import HSChain.Examples.Simple
import HSChain.Examples.Util

import TM.Util.Mockchain


tests :: TestTree
tests = testGroup "PoW consensus"
  [ testCase "Linear: blocks added ASC"  testLinear
  , testCase "Linear: blocks added DESC" testLinear2
  , testCase "Reorganization"            testReorg
  ]

----------------------------------------------------------------
--
----------------------------------------------------------------

-- Test linear addition of blocks in a way that simulates
-- synchronization when we catch up a lot of blocks
testLinear :: IO ()
testLinear = runTest
  [ MsgH header1 Nothing (\c -> candidatesSeqOK c [(header1, [header1])]
                             <> requiredOK      c [header1]
                             <> bestHeadOK      c genesis
                         )
  , MsgH header2 Nothing (\c -> candidatesSeqOK c [(header2, [header1, header2])]
                             <> requiredOK      c [header1,header2]
                             <> bestHeadOK      c genesis
                         ) 
  , MsgH header3 Nothing (\c -> candidatesSeqOK c [(header3, [header1, header2, header3])]
                             <> requiredOK      c [header1,header2,header3]
                             <> bestHeadOK      c genesis
                         )
  , MsgB block1  Nothing (\c -> candidatesSeqOK c [(header3, [header2,header3])]
                             <> requiredOK      c [header2,header3]
                             <> bestHeadOK      c block1
                         )
  , MsgB block2  Nothing (\c -> candidatesSeqOK c [(header3, [header3])]
                             <> requiredOK      c [header3]
                             <> bestHeadOK      c block2
                         )
  , MsgB block3  Nothing (\c -> candidatesSeqOK c []
                             <> requiredOK      c []
                             <> bestHeadOK      c block3
                         )
  ]

testLinear2 :: IO ()
testLinear2 = runTest
  [ MsgH header1 Nothing (\c -> candidatesSeqOK c [(header1, [header1])]
                             <> requiredOK      c [header1]
                             <> bestHeadOK      c genesis
                         )
  , MsgH header2 Nothing (\c -> candidatesSeqOK c [(header2, [header1, header2])]
                             <> requiredOK      c [header1,header2]
                             <> bestHeadOK      c genesis
                         ) 
  , MsgH header3 Nothing (\c -> candidatesSeqOK c [(header3, [header1, header2, header3])]
                             <> requiredOK      c [header1,header2,header3]
                             <> bestHeadOK      c genesis
                         )
  , MsgB block3  Nothing (\c -> candidatesSeqOK c [(header3, [header1,header2,header3])]
                             <> requiredOK      c [header1,header2]
                             <> bestHeadOK      c genesis
                         )
  , MsgB block2  Nothing (\c -> candidatesSeqOK c [(header3, [header1,header2,header3])]
                             <> requiredOK      c [header1]
                             <> bestHeadOK      c genesis
                         )
  , MsgB block1  Nothing (\c -> candidatesSeqOK c []
                             <> requiredOK      c []
                             <> bestHeadOK      c block3
                         )
  ]


testReorg :: IO ()
testReorg = runTest
  [ MsgH header1  Nothing (\c -> candidatesSeqOK c [(header1, [header1])]
                              <> requiredOK      c [header1]
                              <> bestHeadOK      c genesis
                          )
  , MsgH header2  Nothing (\c -> candidatesSeqOK c [(header2, [header1, header2])]
                              <> requiredOK      c [header1,header2]
                              <> bestHeadOK      c genesis
                          ) 
  , MsgH header3  Nothing (\c -> candidatesSeqOK c [(header3, [header1, header2, header3])]
                              <> requiredOK      c [header1,header2,header3]
                              <> bestHeadOK      c genesis
                          )
  , MsgH header2' Nothing (\c -> candidatesSeqOK c [ (header3,  [header1, header2, header3])
                                                   , (header2', [header1, header2'])
                                                   ]
                          
                              <> requiredOK      c [header1,header2,header3,header2']
                              <> bestHeadOK      c genesis
                          )
  --
  , MsgB block1   Nothing (\c -> candidatesSeqOK c [ (header3,  [header2, header3])
                                                   , (header2', [header2'])
                                                   ]
                              <> requiredOK      c [header2,header3,header2']
                              <> bestHeadOK      c block1
                          )
  , MsgB block2'  Nothing (\c -> candidatesSeqOK c [ (header3,  [header2, header3])
                                                   ]
                              <> requiredOK      c [header2,header3]
                              <> bestHeadOK      c block2'
                          )
  -- Now we will reorg!
  , MsgB block2   Nothing (\c -> candidatesSeqOK c [ (header3,  [header3])
                                                   ]
                              <> requiredOK      c [header3]
                              <> bestHeadOK      c block2'
                          )
  , MsgB block3   Nothing (\c -> candidatesSeqOK c []
                              <> requiredOK      c []
                              <> bestHeadOK      c block3
                          )
  
  ]


bestHeadOK :: IsMerkle f => Consensus m (KV MockChain) -> GBlock (KV MockChain) f -> [String]
bestHeadOK c h
  | expected == got = []
  | otherwise       =
      [ "bestHeadOK: mismatch"
      , "  expected: " ++ show expected
      , "  got:      " ++ show got
      ]
  where
    expected = blockID h
    got      = c^.bestHead._1.to bhBID

-- candidatesOK :: Consensus m (KV MockChain) -> [Header (KV MockChain)] -> [String]
-- candidatesOK c hs
--   | expected == headSet = []
--   | otherwise           = ["candidatesOK: mismatch"]
--   where
--     expected = Set.fromList $ blockID <$> hs
--     headSet  = Set.fromList $ map (bhBID . bchHead) $ c^.candidateHeads

candidatesSeqOK :: Consensus m (KV MockChain) -> [(Header (KV MockChain),[Header (KV MockChain)])] -> [String]
candidatesSeqOK c hs
  | expected == headSet = []
  | otherwise           =
      [ "candidatesSeqOK: mismatch"
      , "  expected: "]
      ++ map show (Map.toList expected)
      ++
      [ "  got:      "]
      ++ map show (Map.toList headSet)
  where
    expected = Map.fromList [ (blockID h, blockID <$> ss) | (h,ss) <- hs ]
    headSet  = Map.fromList [ (bhBID bh, map bhBID $ toList bseq)
                            | Head bh bseq<- c^.candidateHeads
                            ]


requiredOK :: Consensus m (KV MockChain) -> [Header (KV MockChain)] -> [String]
requiredOK c hs
  | c^.requiredBlocks == expected = []
  | otherwise                     =
      [ "requiredOK: mismatch"
      , "  expected: " ++ show expected
      , "  got:      " ++ show (c^.requiredBlocks)
      ]
  where
    expected = Set.fromList $ blockID <$> hs


----------------------------------------------------------------
--
----------------------------------------------------------------


data Message
  = MsgH !(Header (KV MockChain)) (Maybe HeaderError) (Consensus (NoLogsT IO) (KV MockChain) -> [String])
  | MsgB !(Block  (KV MockChain)) (Maybe BlockError ) (Consensus (NoLogsT IO) (KV MockChain) -> [String])



runTest :: [Message] -> IO ()
runTest msgList = runNoLogsT $ do
  db <- inMemoryDB
  let s0 = consensusGenesis (head mockchain) (inMemoryView kvViewStep Map.empty (blockID genesis))
  runExceptT (loop db s0 msgList) >>= \case
    Left  e  -> error $ unlines e
    Right () -> return ()
  where
    check f s = case f s of
      [] -> return ()
      es -> throwError es
    toE   = either Just (\() -> Nothing)
    run s = flip runStateT s . runExceptT
    --
    loop _  _ []     = return ()
    loop db s (m:ms) = do
      (s',val) <- case m of
        MsgH h e0 val -> do (e,s') <- lift $ run s $ processHeader h
                            when (toE e /= e0) $ throwError
                              [ "Mismatch of header error"
                              , "  expected: " ++ show e0
                              , "  got:      " ++ show (toE e)
                              ]
                            return (s',val)
        MsgB b e0 val -> do (e,s') <- lift $ run s $ processBlock db b
                            when (toE e /= e0) $ throwError
                              [ "Mismatch of header error"
                              , "  expected: " ++ show e0
                              , "  got:      " ++ show (toE e)
                              ]
                            return (s',val)
      -- Perform checks
      check val            s'
      check checkConsensus s'
      --
      loop db s' ms


-- Check for invariants in tracking of PoW consensus 
checkConsensus :: (Show (BlockID b), BlockData b) => Consensus m b -> [String]
checkConsensus c = concat
  [ [ "Head with less work: " ++ show (bhBID bh)
    | Head bh _ <- c^.candidateHeads
    , bhWork bh <= bestWork
    ]
  -- , [
  ]
  where
    bestWork = bhWork $ c ^. bestHead . _1

