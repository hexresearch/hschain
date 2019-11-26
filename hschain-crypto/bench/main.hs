module Main where

import Criterion.Main

import qualified Ben.Ed25519
import qualified Ben.Hash

main :: IO ()
main = defaultMain
  [ Ben.Ed25519.benchmarks
  , Ben.Hash.benchmarks
  ]
