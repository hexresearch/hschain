{-# LANGUAGE CPP #-}
module Main where

import Criterion.Main

import qualified Ben.Ed25519
import qualified Ben.Hash
#if USE_BLS
import qualified Ben.BLS
#endif

main :: IO ()
main = defaultMain
  [ Ben.Ed25519.benchmarks
  , Ben.Hash.benchmarks
#if USE_BLS
  , Ben.BLS.benchmarks
#endif
  ]
