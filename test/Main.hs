import Test.Tasty

import qualified TM.Network

main :: IO ()
main = defaultMain $ testGroup "test suite"
  [ TM.Network.tests
  ]
