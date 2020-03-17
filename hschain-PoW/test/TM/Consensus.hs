{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase       #-}
-- |
module TM.Consensus (tests) where

import Control.Monad.IO.Class
import Control.Monad.State.Strict
import Control.Monad.Except
import Data.IORef
import Data.List (unfoldr)
import Data.Foldable
import qualified Data.Map.Strict as Map
import qualified Data.Set        as Set
import Lens.Micro
import Test.Tasty
import Test.Tasty.HUnit

import HSChain.PoW.Types
import HSChain.PoW.Consensus
import HSChain.Types.Merkle.Types
import HSChain.Examples.Simple

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


bestHeadOK :: IsMerkle f => Consensus m KV -> GBlock KV f -> [String]
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

-- candidatesOK :: Consensus m KV -> [Header KV] -> [String]
-- candidatesOK c hs
--   | expected == headSet = []
--   | otherwise           = ["candidatesOK: mismatch"]
--   where
--     expected = Set.fromList $ blockID <$> hs
--     headSet  = Set.fromList $ map (bhBID . bchHead) $ c^.candidateHeads

candidatesSeqOK :: Consensus m KV -> [(Header KV,[Header KV])] -> [String]
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


requiredOK :: Consensus m KV -> [Header KV] -> [String]
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
  = MsgH !(Header KV) (Maybe HeaderError) (Consensus IO KV -> [String])
  | MsgB !(Block  KV) (Maybe BlockError ) (Consensus IO KV -> [String])



runTest :: [Message] -> IO ()
runTest msgList = do
  db <- inMemoryDB
  let s0 = consensusGenesis (head mockchain) (viewKV (blockID genesis)) db
  runExceptT (loop s0 msgList) >>= \case
    Left  e  -> error $ unlines e
    Right () -> return ()
  where
    check f s = case f s of
      [] -> return ()
      es -> throwError es
    toE   = either Just (\() -> Nothing)
    run s = flip runStateT s . runExceptT
    --
    loop _ [] = return ()
    loop s (m:ms) = do
      (s',val) <- case m of
        MsgH h e0 val -> do (e,s') <- lift $ run s $ processHeader h
                            when (toE e /= e0) $ throwError
                              [ "Mismatch of header error"
                              , "  expected: " ++ show e0
                              , "  got:      " ++ show (toE e)
                              ]
                            return (s',val)
        MsgB b e0 val -> do (e,s') <- lift $ run s $ processBlock b
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
      loop s' ms


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




----------------------------------------------------------------
--
----------------------------------------------------------------

mockchain :: [Block KV]
mockchain = gen : unfoldr (Just . (\b -> (b,b)) . mineBlock "VAL") gen
  where
    gen = GBlock { blockHeight = Height 0
                 , prevBlock   = Nothing
                 , blockData   = KV { kvData = merkled [] }
                 }

mineBlock :: String -> Block KV -> Block KV
mineBlock val b = GBlock
  { blockHeight = succ $ blockHeight b
  , prevBlock   = Just $! blockID b
  , blockData   = KV { kvData = merkled [ let Height h = blockHeight b
                                          in (fromIntegral h, val)
                                        ]
                     }
  }

genesis :: Block KV
genesis = head mockchain

block1,block2,block3,block2' :: Block KV
[_,block1,block2,block3] = take 4 mockchain
block2' = mineBlock "Z" block1


header1,header2,header3,header2' :: Header KV
header1  = toHeader block1
header2  = toHeader block2
header3  = toHeader block3
header2' = toHeader block2'


----------------------------------------------------------------
--
----------------------------------------------------------------

-- | Simepl in-memory implementation of DB
inMemoryView
  :: (Monad m, BlockData b, Show (BlockID b))
  => (Block b -> s -> Maybe s)  -- ^ Step function 
  -> s                          -- ^ Initial state
  -> BlockID b
  -> StateView m b
inMemoryView step = make (error "No revinding past genesis")
  where
    make previous s bid = view
      where
        view = StateView
          { stateBID    = bid
          , applyBlock  = \b -> case step b s of
              Nothing -> return Nothing
              Just s' -> return $ Just $  make view s' (blockID b)
          , revertBlock = return previous
          , flushState  = return ()
          }

viewKV :: Monad m => BlockID KV -> StateView m KV
viewKV bid = inMemoryView step Map.empty bid
  where
    step b m
      | or [ k `Map.member` m | (k,_) <- txs ] = Nothing
      | otherwise                              = Just $ Map.fromList txs <> m
      where
        txs = merkleValue $ kvData $ blockData b

inMemoryDB
  :: (MonadIO m, BlockData b)
  => m (BlockDB m b)
inMemoryDB = do
  var <- liftIO $ newIORef Map.empty
  return BlockDB
    { storeBlock     = \b -> liftIO $ modifyIORef' var $ Map.insert (blockID b) b
    , retrieveBlock  = \bid -> liftIO $ Map.lookup bid <$> readIORef var
    , retrieveHeader = \bid -> liftIO $ fmap toHeader . Map.lookup bid <$> readIORef var
    }
