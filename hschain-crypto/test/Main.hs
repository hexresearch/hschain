import Test.Tasty

import qualified TM.Crypto

main :: IO ()
main = defaultMain $ testGroup "test suite"
  [ TM.Crypto.tests
  ]
