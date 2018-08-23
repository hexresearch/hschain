import Test.Tasty

import qualified TM.Network
import qualified TM.P2P

main :: IO ()
main = defaultMain $ testGroup "test suite"
  [ TM.Network.tests
  , TM.P2P.tests
  ]
