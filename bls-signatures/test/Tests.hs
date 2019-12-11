module Main where


import Crypto.Bls as Bls
import Test.Tasty

import Tests.Bls


main :: IO ()
main = do
    Bls.initBls
    defaultMain $ testGroup "BLS tests" [ testBls ]

