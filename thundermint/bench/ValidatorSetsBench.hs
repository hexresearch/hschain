{-# LANGUAGE BangPatterns #-}
module ValidatorSetsBench (benchValidatorSets) where


import Control.DeepSeq
import Criterion.Main
import System.Random
import qualified Data.Vector as VS

import Thundermint.Types.Validators


type BenchValidatorIdx = ValidatorIdx ()


benchValidatorSets :: Benchmark
benchValidatorSets = bgroup "ValidatorISet"
    [ bgroup "empty"
        [ bench "emptyValidatorISet 4"   $ nf emptyValidatorISet 4
        , bench "emptyValidatorISet 8"   $ nf emptyValidatorISet 8
        , bench "emptyValidatorISet 16"  $ nf emptyValidatorISet 16
        , bench "emptyValidatorISet 32"  $ nf emptyValidatorISet 32
        , bench "emptyValidatorISet 64"  $ nf emptyValidatorISet 64
        , bench "emptyValidatorISet 128" $ nf emptyValidatorISet 128
        , bench "emptyValidatorISet 256" $ nf emptyValidatorISet 256
        ]
    , bgroup "insert"
        [ let !size = 4;   !count = 1000 ; !lst = generateIdxes 0 size count in bench (show size ++ "-" ++ show count) $ nf insertIdxes (size, lst)
        , let !size = 8;   !count = 1000 ; !lst = generateIdxes 0 size count in bench (show size ++ "-" ++ show count) $ nf insertIdxes (size, lst)
        , let !size = 16;  !count = 1000 ; !lst = generateIdxes 0 size count in bench (show size ++ "-" ++ show count) $ nf insertIdxes (size, lst)
        , let !size = 32;  !count = 1000 ; !lst = generateIdxes 0 size count in bench (show size ++ "-" ++ show count) $ nf insertIdxes (size, lst)
        , let !size = 64;  !count = 1000 ; !lst = generateIdxes 0 size count in bench (show size ++ "-" ++ show count) $ nf insertIdxes (size, lst)
        , let !size = 128; !count = 1000 ; !lst = generateIdxes 0 size count in bench (show size ++ "-" ++ show count) $ nf insertIdxes (size, lst)
        , let !size = 256; !count = 1000 ; !lst = generateIdxes 0 size count in bench (show size ++ "-" ++ show count) $ nf insertIdxes (size, lst)
        ]
    , bgroup "getValidatorIntSet"
        [ let !size = 4;   !count = 1000; !set = force $ generateSet count size in bench (show size) $ nf getLists set
        , let !size = 8;   !count = 1000; !set = force $ generateSet count size in bench (show size) $ nf getLists set
        , let !size = 16;  !count = 1000; !set = force $ generateSet count size in bench (show size) $ nf getLists set
        , let !size = 32;  !count = 1000; !set = force $ generateSet count size in bench (show size) $ nf getLists set
        , let !size = 64;  !count = 1000; !set = force $ generateSet count size in bench (show size) $ nf getLists set
        , let !size = 128; !count = 1000; !set = force $ generateSet count size in bench (show size) $ nf getLists set
        , let !size = 256; !count = 1000; !set = force $ generateSet count size in bench (show size) $ nf getLists set
        ]
    ]


generateIdxes :: Int -> Int -> Int -> VS.Vector BenchValidatorIdx
generateIdxes from to count =
    VS.fromList $ take count $ map ValidatorIdx $ randomRs (from, to - 1) (mkStdGen 0xDEADBEEF)


generateSet :: Int -> Int -> VS.Vector ValidatorISet
generateSet count size = result
  where
    sizePerOneSet = ((size * 2) `div` 3) + 1
    idxes         = generateIdxes 0 size (sizePerOneSet * count)
    slices        = map (\i -> VS.slice i sizePerOneSet idxes) [0..(count - 1)]
    result        = VS.fromList $ map (\slice -> insertIdxes (size, slice)) slices


insertIdxes :: (Int, VS.Vector BenchValidatorIdx) -> ValidatorISet
insertIdxes (size, idxs) =
    -- VS.foldl' (flip insertValidatorIdx) (emptyValidatorISet size) idxs
    VS.foldr' (insertValidatorIdx) (emptyValidatorISet size) idxs -- TODO перейти потом на foldl'


getLists :: VS.Vector ValidatorISet -> [ValidatorIdx alg]
getLists = concat . VS.map (getValidatorIntSet)

