{-# LANGUAGE CPP #-}
module Main where

import Criterion.Main


--import Control.DeepSeq
--import Control.Exception (evaluate)

--import Crypto.Random              (getRandomBytes)
--import HSChain.Crypto
--import HSChain.Crypto.Ed25519


#if USE_BLS
import qualified Crypto
#endif

main :: IO ()
main = defaultMain
  [
#if USE_BLS
    Crypto.benchmarks
#endif
  ]
