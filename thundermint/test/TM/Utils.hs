{-# LANGUAGE BinaryLiterals #-}
module TM.Utils (tests) where


import Test.Tasty
import Test.Tasty.HUnit

import Thundermint.Types.BitVector as BV


tests :: TestTree
tests = testGroup "utils testsing"
    [ testGroup "bitvectors"
        [ testCase "1"  $ [1] @=? (BV.toList $ BV.insert 1 $ BV.new 10)
        , testCase "2"  $ [0,1,2,3,50]        @=? (BV.toList $ foldl (flip BV.insert) (BV.new 60) [0, 1, 0, 50, 3, 2, 50])
        , testCase "3"  $ [1,3..61]           @=? (BV.toList $ foldl (flip BV.insert) (BV.new 63) [1,3..61])
        , testCase "4"  $ [0..34]             @=? (BV.toList $ foldl (flip BV.insert) (BV.new 35) [0..34])
        , testCase "5"  $ [0,1,500]           @=? (BV.toList $ foldl (flip BV.insert) (BV.new 520) [0,1,500])
        , testCase "6"  $ [0,1,2,3,50,65,987] @=? (BV.toList $ foldl (flip BV.insert) (BV.new 1000) [987, 0, 1, 0, 65, 50, 3, 2, 50])
        , testCase "7"  $ [0..255]            @=? (BV.toList $ foldl (flip BV.insert) (BV.new 256) [255,254..0])
        , testCase "8"  $ [0,2..254]          @=? (BV.toList $ foldl (flip BV.insert) (BV.new 256) [254,252..0])
        , testCase "9"  $ [1,3..255]          @=? (BV.toList $ foldl (flip BV.insert) (BV.new 256) [255,253..1])
        ]
    ]

