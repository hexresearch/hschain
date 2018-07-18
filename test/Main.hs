import Test.Tasty

import qualified TM.MockNet
import qualified TM.RealNetwork

main :: IO ()
main = defaultMain $ testGroup "test suite"
  [ TM.MockNet.tests
  , TM.RealNetwork.tests
  ]
