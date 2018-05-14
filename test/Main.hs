import Test.Tasty

import qualified TM.MockNet

main :: IO ()
main = defaultMain $ testGroup "test suite"
  [ TM.MockNet.tests
  ]

