import Test.Tasty

import Crypto.Bls

import qualified TM.BLS


main :: IO ()
main = do
    initBls
    defaultMain $ testGroup "BLS test suite" [ TM.BLS.tests ]
