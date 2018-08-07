import Test.Tasty

import qualified TM.Network
import qualified TM.Serialisation

main :: IO ()
main = defaultMain $ testGroup "test suite"
  [ TM.Network.tests
  , TM.Serialisation.tests
  ]
