import Test.Tasty

import qualified TM.Network
import qualified TM.Serialisation
import qualified TM.Store

main :: IO ()
main = defaultMain $ testGroup "test suite"
  [ TM.Network.tests
  , TM.Serialisation.tests
  , TM.Store.tests
  ]
