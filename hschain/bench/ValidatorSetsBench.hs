{-# LANGUAGE BangPatterns #-}
module ValidatorSetsBench (benchValidatorSets) where

import Criterion.Main
import System.Random
import qualified Data.Vector as VS

import HSChain.Types.Validators


type BenchValidatorIdx = ValidatorIdx ()


benchValidatorSets :: Benchmark
benchValidatorSets = bgroup "ValidatorISet"
  [ bgroup "insert"
    [ bench (show size ++ "-" ++ show count) $ nf insertIdxes (size, lst)
    | size <- [4,8,16,32,64,128,256]
    , let count = 1000
          lst   = generateIdxes 0 size count
    ]
  ]


generateIdxes :: Int -> Int -> Int -> VS.Vector BenchValidatorIdx
generateIdxes from to count =
    VS.fromList $ take count $ map ValidatorIdx $ randomRs (from, to - 1) (mkStdGen 0xDEADBEEF)

insertIdxes :: (Int, VS.Vector BenchValidatorIdx) -> ValidatorISet
insertIdxes (size, idxs) =
    -- VS.foldl' (flip insertValidatorIdx) (emptyValidatorISet size) idxs
    VS.foldr' (insertValidatorIdx) (emptyValidatorISetFromSize size) idxs -- TODO перейти потом на foldl'
