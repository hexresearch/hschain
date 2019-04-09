{-# LANGUAGE BangPatterns #-}
module ValidatorSetsBench (benchValidatorSets) where


import Control.DeepSeq
import Criterion.Main
import qualified Data.Vector as V
import System.Random

import Thundermint.Types.Validators

type BenchValidatorIdx = ValidatorIdx ()



benchValidatorSets :: Benchmark
benchValidatorSets = bgroup "ValidatorISet"
    [ {- bgroup "empty"
        [ bench "emptyValidatorISet 4"   $ nf emptyValidatorISet 4
        , bench "emptyValidatorISet 8"   $ nf emptyValidatorISet 8
        , bench "emptyValidatorISet 128" $ nf emptyValidatorISet 128
        ]
    , bgroup "insert"
        [ let !size = 4;   !count = 1000 ; !lst = generateIdxes 0 size count in bench (show size ++ "-" ++ show count) $ nf insertIdxes (size, lst)
        , let !size = 8;   !count = 1000 ; !lst = generateIdxes 0 size count in bench (show size ++ "-" ++ show count) $ nf insertIdxes (size, lst)
        , let !size = 16;  !count = 1000 ; !lst = generateIdxes 0 size count in bench (show size ++ "-" ++ show count) $ nf insertIdxes (size, lst)
        , let !size = 32;  !count = 1000 ; !lst = generateIdxes 0 size count in bench (show size ++ "-" ++ show count) $ nf insertIdxes (size, lst)
        , let !size = 64;  !count = 1000 ; !lst = generateIdxes 0 size count in bench (show size ++ "-" ++ show count) $ nf insertIdxes (size, lst)
        , let !size = 128; !count = 1000 ; !lst = generateIdxes 0 size count in bench (show size ++ "-" ++ show count) $ nf insertIdxes (size, lst)
        ]
    , -} bgroup "getValidatorIntSet"
        [ let !size = 4;   !set = force $ generateSet size in bench (show size) $ nf getValidatorIntSet set
        , let !size = 8;   !set = force $ generateSet size in bench (show size) $ nf getValidatorIntSet set
        , let !size = 16;  !set = force $ generateSet size in bench (show size) $ nf getValidatorIntSet set
        , let !size = 32;  !set = force $ generateSet size in bench (show size) $ nf getValidatorIntSet set
        , let !size = 64;  !set = force $ generateSet size in bench (show size) $ nf getValidatorIntSet set
        , let !size = 128; !set = force $ generateSet size in bench (show size) $ nf getValidatorIntSet set
        ]
    ]


generateIdxes :: Int -> Int -> Int -> V.Vector BenchValidatorIdx
generateIdxes from to count =
    V.fromList $ take count $ map ValidatorIdx $ randomRs (from, to) (mkStdGen 0xDEADBEEF)


generateSet :: Int -> ValidatorISet
generateSet size =
    insertIdxes (size, generateIdxes 0 size ((size * 2) `div` 3))


insertIdxes :: (Int, V.Vector BenchValidatorIdx) -> ValidatorISet
insertIdxes (size, idxs) =
    V.foldl' (\s i -> insertValidatorIdx i s) (emptyValidatorISet size) idxs

