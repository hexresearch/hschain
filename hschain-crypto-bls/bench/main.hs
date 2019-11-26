module Main where

import Criterion.Main
import qualified Ben.BLS


main :: IO ()
main = defaultMain
  [ Ben.BLS.benchmarks
  ]
