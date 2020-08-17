{-# LANGUAGE FlexibleContexts #-}
-- |
module TM.Types (tests) where

import Test.Tasty
import Test.Tasty.HUnit

import HSChain.Types.Merkle.Types
import HSChain.PoW.BlockIndex
import HSChain.PoW.Types
import HSChain.Examples.Simple (KV)
import TM.Util.Mockchain

tests :: TestTree
tests = testGroup "Block index"
  [ testGroup "traversal"
    [ testCase "none" testTraverseNone
    , testCase "fwd"  testTraverseFwd
    , testCase "bwd"  testTraverseBwd
    , testCase "full" testTraverseFull
    ]
  ]

testTraverseNone :: IO ()
testTraverseNone = expected @=? delta
  where
    Just bhA = lookupIdx (blockID block3') blockIndex
    delta    = makeBlockIndexPath bhBID bhA bhA
    expected = NoChange

testTraverseFwd :: IO ()
testTraverseFwd = expected @=? delta
  where
    Just bhA = lookupIdx (blockID block1) blockIndex
    Just bhB = lookupIdx (blockID block3) blockIndex
    delta    = makeBlockIndexPath bhBID bhA bhB
    expected
      = ApplyBlock  (blockID block3)
      $ ApplyBlock  (blockID block2)
      $ NoChange

testTraverseBwd :: IO ()
testTraverseBwd = expected @=? delta
  where
    Just bhA = lookupIdx (blockID block3) blockIndex
    Just bhB = lookupIdx (blockID block1) blockIndex
    delta    = makeBlockIndexPath bhBID bhA bhB
    expected
      = RevertBlock (blockID block2)
      $ RevertBlock (blockID block3)
      $ NoChange

testTraverseFull :: IO ()
testTraverseFull = expected @=? delta
  where
    Just bhA = lookupIdx (blockID block3') blockIndex
    Just bhB = lookupIdx (blockID block3 ) blockIndex
    delta    = makeBlockIndexPath bhBID bhA bhB
    expected
      = ApplyBlock  (blockID block3)
      $ ApplyBlock  (blockID block2)
      $ RevertBlock (blockID block2')
      $ RevertBlock (blockID block3')
      $ NoChange

blockIndex :: BlockIndex (KV MockChain)
blockIndex
  = id
  $ addBlock block4'
  $ addBlock block3'
  $ addBlock block2'
  $ addBlock block3
  $ addBlock block2
  $ addBlock block1
  $ blockIndexFromGenesis genesis

addBlock
  :: (BlockData b, MerkleMap b)
  => Block b -> BlockIndex b -> BlockIndex b
addBlock b bIdx = insertIdx bh bIdx
  where
    Just parent = do bid <- prevBlock b
                     lookupIdx bid bIdx
    bh = BH { bhHeight   = blockHeight b
            , bhTime     = blockTime   b
            , bhBID      = blockID     b
            , bhWork     = blockWork   b
            , bhPrevious = Just parent
            , bhData     = blockData $ toHeader b
            }
      
