import Test.Tasty

import qualified TM.BLS

main :: IO ()
main = defaultMain $ testGroup "test suite"
  [ TM.BLS.tests
  ]
