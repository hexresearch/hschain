{-# LANGUAGE TypeFamilies #-}
-- |
module TM.Consensus (tests) where

import Control.Lens
import Control.Monad.State.Strict
import Control.Monad.Except
import Data.Foldable
import qualified Data.Map.Strict as Map
import qualified Data.Set        as Set
import Test.Tasty
import Test.Tasty.HUnit

import HSChain.PoW.Types
import HSChain.PoW.Consensus
import HSChain.Logger
import HSChain.PoW.Node
import HSChain.Types.Merkle.Types
import HSChain.Examples.Simple
import HSChain.Crypto.SHA
import HSChain.Crypto

import TM.Util.Mockchain


tests :: TestTree
tests = testGroup "PoW consensus"
  [ testCase "Linear: blocks added ASC"  testLinear
  , testCase "Linear: blocks added DESC" testLinear2
  , testCase "Reorganization"            testReorg
  , testCase "Reorganization depth 2"    testReorg2
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

-- |Test deeper (2 rollbacks) reorganization.
--
-- Please note that we are telling our consensus about headers first
-- (some MsgH there).
--
-- Then, after we specified all headers of blocks, we can supplement blocks
-- there (some MsgB after). And consensus should reach some valid state
-- after it was supplemented by all blocks and headers.
--
-- To get a better test we must have some branching temporal logic here.
--
-- For example, we can pass a block to consensus that is not rooted on any known block.
-- What will consensus do then?
--
-- Basically, we should permute messages there in some clever way, keeping track
-- of what we would expect from consensus. But it is slightly too much work for now.

testReorg2 :: IO ()
testReorg2 = runTest
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
  , MsgB block1   Nothing (\c -> candidatesSeqOK c [ (header3,  [header2, header3])
                                                   , (header2', [header2'])
                                                   ]
                              <> requiredOK      c [header2,header3,header2']
                              <> bestHeadOK      c block1
                          )
  , MsgH header3' Nothing (\c -> candidatesSeqOK c [ (header3,  [header2, header3])
                                                   , (header3', [header2', header3'])
                                                   ]

                              <> requiredOK      c [header2,header3,header2',header3']
                              <> bestHeadOK      c block1
                          )
  , MsgH header4' Nothing (\c -> candidatesSeqOK c [ (header3,  [header2, header3])
                                                   , (header4', [header2', header3',header4'])
                                                   ]

                              <> requiredOK      c [header2,header3,header2',header3', header4']
                              <> bestHeadOK      c block1
                          )
  --
  , MsgB block2  Nothing (\c -> candidatesSeqOK c [ (header3,  [header3])
                                                  , (header4', [header2', header3', header4'])
                                                  ]
                              <> requiredOK      c [header2', header3, header3', header4']
                              <> bestHeadOK      c block2
                          )
  , MsgB block4' Nothing (\c -> candidatesSeqOK c [ (header4', [header2', header3', header4'])
                                                  , (header3, [header3])
                                                  ]
                             <> requiredOK      c [header2', header3', header3]
                             <> bestHeadOK      c block2
                         )
  -- Now we will reorg!
  , MsgB block2'   Nothing (\c -> candidatesSeqOK c [ (header3,  [header3])
                                                    , (header4', [header3', header4'])
                                                    ]
                               <> requiredOK      c [header3', header3]
                               <> bestHeadOK      c block2
                           )
  , MsgB block3    Nothing (\c -> candidatesSeqOK c [(header4', [header3', header4'])]
                               <> requiredOK      c [header3']
                               <> bestHeadOK      c block3
                           )
  , MsgB block3'   Nothing (\c -> candidatesSeqOK c []
                               <> requiredOK      c []
                               <> bestHeadOK      c block4'
                           )
  
  ]



bestHeadOK :: IsMerkle f => Consensus (KVState MockChain m) m (KV MockChain) -> GBlock (KV MockChain) f -> [String]
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

-- | Runs 
candidatesSeqOK :: Consensus (KVState MockChain m) m (KV MockChain)
                -> [(Header (KV MockChain),[Header (KV MockChain)])] -> [String]
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


requiredOK :: Consensus (KVState MockChain m) m (KV MockChain) -> [Header (KV MockChain)] -> [String]
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
  = MsgH !(Header (KV MockChain))
         !(Maybe  (HeaderError (KV MockChain)))
         !(Consensus (KVState MockChain (NoLogsT IO)) (NoLogsT IO) (KV MockChain) -> [String])
  | MsgB !(Block  (KV MockChain))
         !(Maybe  (BlockError (KV MockChain)))
         !(Consensus (KVState MockChain (NoLogsT IO)) (NoLogsT IO) (KV MockChain) -> [String])


runTest :: [Message] -> IO ()
runTest msgList = runNoLogsT $ do
  db <- inMemoryDB genesis
  let s0 = consensusGenesis (head mockchain) $ kvMemoryView (blockID genesis)
  runExceptT (loop db s0 msgList) >>= \case
    Left  e  -> error $ unlines e
    Right () -> return ()
  where
    check extraInfo f s = case f s of
      [] -> return ()
      es -> throwError $ es ++ [extraInfo] ++
                       [ "  >> "++show (hash b :: Hash SHA256) ++ " => "++show b
                       | b <- [genesis, block1, block2, block3, block2', block3', block4']
                       ]
    toE   = either Just (\_ -> Nothing)
    run s = flip runStateT s . runExceptT
    --
    loop _  _ []     = return ()
    loop db s (m:ms) = do
      (s',val, extraInfo) <- case m of
        MsgH h e0 val -> do (e,s') <- lift $ run s $ processHeader h
                            let ei = "header " ++ show h
                            when (toE e /= e0) $ throwError
                              [ "Mismatch of header error"
                              , "  expected: " ++ show e0
                              , "  got:      " ++ show (toE e)
                              , ""
                              , "  "++ei
                              ]
                            return (s',val, ei)
        MsgB b e0 val -> do (e,s') <- lift $ run s $ processBlock db b
                            let ei = "block " ++ show b
                            when (toE e /= e0) $ throwError
                              [ "Mismatch of header error"
                              , "  expected: " ++ show e0
                              , "  got:      " ++ show (toE e)
                              , ""
                              , "  "++ei
                              ]
                            return (s',val, ei)
      -- Perform checks
      check extraInfo val            s'
      check extraInfo checkConsensus s'
      --
      loop db s' ms


-- Check for invariants in tracking of PoW consensus 
checkConsensus :: (Show (BlockID b), StateView' view m b) => Consensus view m b -> [String]
checkConsensus c = concat
  [ [ "Head with less work: " ++ show (bhBID bh)
    | Head bh _ <- c^.candidateHeads
    , bhWork bh <= bestWork
    ]
  -- , [
  ]
  where
    bestWork = bhWork $ c ^. bestHead . _1

