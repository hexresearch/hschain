import Test.Tasty
import Test.Tasty.HUnit

import Crypto.Bls

import qualified TM.BLS


main :: IO ()
main = do
    initBls
    defaultMain $ testGroup "BLS test suite" [ TM.BLS.tests ]
