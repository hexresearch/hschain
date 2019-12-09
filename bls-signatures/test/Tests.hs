module Main where

import Test.Tasty

-- import {-@ HTF_TESTS @-} Tests.Signature
-- import {-@ HTF_TESTS @-} Tests.Threshold
import Tests.PrivateKeys

import Crypto.Bls as Bls

main :: IO ()
main = do
    Bls.initBls
    defaultMain $ testGroup "BLS tests" [ testBls ]

