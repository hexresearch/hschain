import Test.Tasty

import qualified TM.Crypto
import qualified TM.BLS


main :: IO ()
main = defaultMain $ testGroup "test suite"
  [ TM.Crypto.tests
  , TM.BLS.tests
  ]
