import Test.Tasty

import qualified TM.Network
import qualified TM.NetworkTls
import qualified TM.P2P
import qualified TM.Serialisation

main :: IO ()
main = defaultMain $ testGroup "test suite"
  [ TM.Network.tests
  , TM.P2P.tests
  , TM.NetworkTls.tests
  , TM.Serialisation.tests
  ]
