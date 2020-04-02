module Main where

import Criterion.Main
import qualified Ben.BLS

import Crypto.Bls (initBls)


main :: IO ()
main = initBls >> defaultMain
  [ Ben.BLS.benchmarks
  ]
